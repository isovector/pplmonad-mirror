{-# LANGUAGE Arrows #-}

module TextBox (
    autoScrollingProse,
    drawConstant,
    pauser,
    scrollingProse,
    waitArrow,
    writer
    ) where

import Control.Monad
import Control.Monad.Cont
import qualified Data.Text as T
import FRP.Yampa
import FRP.Yampa.Geometry

import Lightarrow
import Message
import OfflineData
import Output
import Output.Window
import SpriteName
import SoundName
import Teletype
import TextUtil

drawBgExplicit rows position = drawWindow position (10, rows + 1)
    
drawBg = drawBgExplicit 2 (Point2 0 96)

textPosition = Point2 8 112

drawConstant text = drawBg >.= displayLines lines textPosition
  where
    lines = map T.unpack (wrap 17 text)

writer write = proc inbox -> do
    interval <- clock -< inbox
    (proseDraw, outbox) <- write -< ((textPosition, interval), inbox)
    done                <- edge  -< any isDone outbox
    returnA -< (drawBg >.= proseDraw, done `tag` (drawBg >.= proseDraw))

clock :: SF [Message] Double
clock = runCont normal final
  where
    normal = slow >> fast >> normal
    fast = swont $ constant 0.01 &&& (arr (any isSlow) >>> edge)
    slow = swont $ constant 0.04 &&& (arr (any isFast) >>> edge)
    final = const $ constant 0.04

--waiter finalOut = constant finalOut `listen` isScroll
pauser finalOut = proc inbox -> do
    done <- edge -< any isScroll inbox
    returnA -< (finalOut, done)

scrollingProse text = runCont initial final
  where
    initial = typing 2 ([], keep $ teletype 17 (T.unpack text)) >>= mainLoop
    final _ = constant (const $ return (), [Done])
    mainLoop = typing 2 >=> waiting >=> scrolling >=> mainLoop

autoScrollingProse text = runCont initial final
  where
    initial = typing 2 ([], keep $ teletype 17 (T.unpack text)) >>= mainLoop
    final _ = constant (const $ return (), [Done])
    mainLoop = typing 2 >=> scrolling >=> mainLoop

typing rows (lines, KeepSF prose) = cont . switch $ proc ((x, interval), inbox) -> do
    rec
        (ts, outbox)             <- iPre (lines, []) -< (drop (length nTs - rows) nTs, nOutbox)
        done                     <- edge             -< any isMarginBell outbox
        ((nTs, nOutbox), frozen) <- prose            -< (interval, [])
    returnA -< ((displayLines ts x, outbox), done `tag` (ts, frozen))

waiting (lines, frozen) = cont . dSwitch $ proc ((x, _), inbox) -> do
    scroll <- edge                -< any isScroll inbox
    wait   <- waitArrow           -< 0.5
    sound  <- soundTrigger Accept -< scroll
    returnA -< ((displayLines lines x >.= wait >.= sound, []), scroll `tag` (lines, frozen))

scrolling (lines, frozen) = cont . dSwitch $ proc ((x, _), inbox) -> do
    (x', outbox) <- scroll -< (x, [])
    done         <- edge   -< any isDone outbox
    returnA -< ((displayLines lines x', []), done `tag` (lines, frozen))
  where
    scroll = (arr $ (.+^ (vector2 0 (-8)))) *** (after (1 / 16) () >>> arr (Done `sayUpon`))

displayLines lines x = fst . foldM display (return (), x) lines
  where
    display (out, x) line od = (out >> drawText (T.pack line) x od, x .+^ vector2 0 16)

waitArrow = flasher (drawSprite ScrollArrow (Point2 144 129)) (const $ return ())

isMarginBell MarginBell = True
isMarginBell _          = False

isDone Done = True
isDone _    = False

isScroll ProseScroll = True
isScroll _           = False

isSlow ProseSlow = True
isSlow _         = False

isFast ProseFast = True
isFast _         = False
