{-# LANGUAGE Arrows, FlexibleContexts #-}

module Menu where

import Control.Monad.Cont
import Control.Monad.Reader
import Data.Bool
import FRP.Yampa hiding (next)
import Data.Vector2
import Data.Point2
import Data.AffineSpace

import Lightarrow
import Message
import OfflineData
import Output
import Output.Window
import SoundName
import SpriteName

choiceOverlay menu = do
    Embedding embed <- ask
    ((action, k), out) <- lift . swont $ embed (menu >>> arr attachOutput)
    return (local (`compEmbed` embedArr (out >.=)) action, k)

choiceReplace menu = ask >>= \(Embedding embed) -> lift . swont $ embed menu

choiceInlay f menu = do
    Embedding embed <- ask
    ((action, k), out) <- lift . swont $ embed (menu >>> arr attachOutput)
    return (local (f out) action, k)

backgroundMenu width height position menu = proc commands -> do
    (draw, invoke) <- menu -< (position .+^ vector2 16 16, commands)
    returnA -< (drawBg >.= draw, invoke)
  where
    drawBg = drawWindow position (tw, th)
    tw = round $ width / 16
    th = round $ height / 16

fullscreenMenu menu = arr (\x -> (Point2 8 8, x)) >>> menu >>> first (arr (clearScreen White >.=))

-- standard menu
columnMenu = columnMenuEx columnPresenter

columnMenuEx presenter [] cancel _ = constant (nullOut, Event cancel)
columnMenuEx presenter options cancel k0 = attachOffsets >>> listMenu selector drawer dispatcher
  where
    (labels, dispatches) = unzip options
    listeners = map listenSelection (zip dispatches [0 .. length dispatches - 1])
    selector = rangeListSelector k0 0 (length options - 1)
    drawer = columnMenuDraw cursor (presenter labels)
    dispatcher = listDispatcher (listenCancel cancel) listeners
    offsets = [ vector2 0 (fromIntegral n * 16) | n <- [0 .. length options] ]
    attachOffsets = first (arr (flip (,) offsets))

-- scrolling menu
scrollMenu size _ [] cancel _ = constant (nullOut, Event cancel)
scrollMenu size presenter options cancel k0 = attachOffsets >>> listMenu selector drawer dispatcher
  where
    (labels, dispatches) = unzip options
    listeners = map listenSelection (zip dispatches [0 .. length dispatches - 1])
    safeSize = ((size `min` length options) `max` 0)
    selector = loopListSelector k0 (length options)
    drawer = scrollMenuDraw safeSize 0 (length labels - 1) cursor (presenter labels) k0
    dispatcher = listDispatcher (listenCancel cancel) listeners
    offsets = [ vector2 0 (fromIntegral n * 16) | n <- [0 .. safeSize - 1] ]
    attachOffsets = first (arr (flip (,) offsets))

-- menu template
listMenu selector drawer dispatcher = proc ((position, offsets), inbox) -> do
    n    <- selector   -< inbox
    draw <- drawer     -< ((position, offsets, n), inbox)
    es   <- dispatcher -< (n, inbox)
    let dispatch = mergeEvents es
    dispatch' <- iPre NoEvent -< dispatch
    sound <- soundTrigger Accept -< dispatch
    returnA -< (draw >.= sound, dispatch')

-- drawer
columnMenuDraw cursor presenter = proc ((position, offsets, n), inbox) -> do
    let relays = replicate n [] ++ (inbox : replicate (length offsets - (n + 1)) [])
        transforms = map (position .+^) offsets
    cursorDraw <- cursor    -< position .+^ (offsets !! n)
    itemDraws  <- presenter -< transforms `zip` relays
    returnA -< foldr (>.=) cursorDraw itemDraws

scrollMenuDraw size high low cursor presenter initial = runCont (loop (initTop, initBottom)) final
  where
    loop = scrolling >=> loop
    initTop = ((initial - 1) `min` (low - size + 1)) `max` high
    initBottom = initTop + size - 1
    final _ = constant nullOut
    scrolling (top, bottom) = cont . switch $ proc ((position, offsets, n), inbox) -> do
        let window = bottom - top + 1
            transforms = map (position .+^) (take window offsets)
            relays = replicate n [] ++ (inbox : replicate (window - (n + 1)) [])
        scrollUp     <- edgeTag (top - 1, bottom - 1)   -< n <= top && top /= high
        scrollDown   <- edgeTag (top + 1, bottom + 1)   -< n >= bottom && bottom /= low
        scrollTop    <- edgeTag (high, high + size - 1) -< n == high && top /= high
        scrollBottom <- edgeTag (low - size + 1, low)   -< n == low && bottom /= low
        cursorDraw   <- cursor                          -< position .+^ (offsets !! (n - top))
        itemDraws    <- presenter top bottom            -< transforms `zip` relays
        let scroll = scrollTop `lMerge` scrollBottom `lMerge` scrollUp `lMerge` scrollDown
        returnA -< (foldr (>.=) cursorDraw itemDraws, scroll)

cursor = arr $ (drawSprite Cursor) . (.+^ vector2 (-8) 0)

columnPresenter labels = par zip $ map (arr . (. fst) . drawLabel) labels

scrollPresenter labels t b = par zip $ map (arr . (. fst) . drawLabel) range
  where
    range = take (b - t + 1) $ drop t labels

-- selector
listSelector initial next previous = arr (mergeEvents . map curse) >>> accumHold initial
  where
    curse CursorNext     = Event $ next
    curse CursorPrevious = Event $ previous
    curse _              = NoEvent

rangeListSelector initial minimum maximum = listSelector initial next previous
  where
    next = (`min` maximum) . (+ 1)
    previous = (`max` minimum) . (subtract 1)

loopListSelector initial size = listSelector initial next previous
  where
    next = (`mod` size) . (+ 1)
    previous = (`mod` size) . (subtract 1)

-- dispatcher
listDispatcher cListener sListeners = proc (n, inbox) -> do
    new     <- cursorChange       -< n
    cancel  <- cListener          -< inbox
    outputs <- par zip sListeners -< route (event inbox (: inbox) new) n
    returnA -< cancel : outputs
  where
    size = length sListeners
    route inbox n = replicate n [] ++ (inbox : replicate (size - (n + 1)) [])
    cursorChange = edgeBy ((bool Nothing (Just MenuConsideration) .) . (/=)) (-1)

listenSelection s = arr (mergeEvents . map f)
  where
    f MenuSelection = Event s
    f _             = NoEvent

listenCancel c = arr (mergeEvents . map f)
  where
    f MenuCancel = Event c
    f _          = NoEvent
