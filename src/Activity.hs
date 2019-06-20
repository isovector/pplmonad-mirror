{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings, NoMonomorphismRestriction #-}

module Activity where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Text as T
import FRP.Yampa
import FRP.Yampa.Geometry

import ControlsMaps
import Lightarrow
import Message
import OfflineData
import Output
import Ppmn.Parameters
import qualified TextBox as TB
import SoundName

stdCommentary = stdProse $ \text -> proseControl >>> TB.writer (TB.autoScrollingProse text)

stdLecture = stdProse (\text -> breathe `switch` const (speak text))
  where
    breathe = constant (TB.drawConstant "", NoEvent) &&& after 0.25 ()
    speak text = proseControl >>> TB.writer (TB.scrollingProse text)

stdProse textbox prose = do
    Embedding embed <- ask
    text <- gets prose
    let ls = T.lines text
        first = init ls
        final = last ls
    sequence_ $ map ((>>= arrowWait) . lift . swont . embed . textbox) first
    lift . swont $ embed (textbox final)

fadeTo color duration = ask >>= \(Embedding embed) -> lift . swont $ embed fade
  where
    fade = timedSequence t (map mask colors ++ [final])
    colors = [Translucent (Translucent color), Translucent color, color, color]
    t = duration / fromIntegral (length colors)
    mask color = constant (drawRectangle 160 144 color (Point2 0 0), NoEvent)
    final = constant (error "fadeTo terminated", Event ())

fadeFrom color duration = ask >>= \(Embedding embed) -> lift . swont $ embed fade
  where
    fade = timedSequence t (map mask colors ++ [final])
    colors = [color, Translucent color, Translucent (Translucent color), Translucent (Translucent (Translucent color))]
    t = duration / fromIntegral (length colors)
    mask color = constant (drawRectangle 160 144 color (Point2 0 0), NoEvent)
    final = constant (error "fadeTo terminated", Event ())

stdHesitation output = ask >>= \(Embedding embed) -> lift . swont $ embed (hesitate 0.5 output)

longHesitation output = ask >>= \(Embedding embed) -> lift . swont $ embed (hesitate 1.5 output)

hesitate t = (&&& after t ()) . constant

stdWait output = ask >>= \(Embedding embed) -> lift . swont $ embed (wait output)

arrowWait output = ask >>= \(Embedding embed) -> lift . swont $ embed $ (wait output >>> first draw) >>> arrowSound
  where
    draw = (identity &&& (constant 0.5 >>> TB.waitArrow)) >>> (arr $ uncurry (>.=))

wait output = constant output &&& (proseControl >>> arr (any isProseScroll) >>> edge)

arrowSound = proc (output, trigger) -> do
    sound    <- soundTrigger Accept -< trigger
    trigger' <- iPre NoEvent        -< trigger
    returnA -< (output >.= sound, trigger')

isProseScroll ProseScroll = True
isProseScroll _           = False

slide = slideStretch 1 1

slideStretch f0 f1 (Point2 x0 y0) (Point2 x1 y1) t1 sprite = proc _ -> do
    f    <- timedInterp f0 f1 t1 -< ()
    x    <- timedInterp x0 x1 t1 -< ()
    y    <- timedInterp y0 y1 t1 -< ()
    returnA -< drawSpriteExplicit Original f White (vector2 0 0) sprite (Point2 x y)

slide' (Point2 x0 y0) (Point2 x1 y1) t1 = proc _ -> do
    x    <- timedInterp x0 x1 t1 -< ()
    y    <- timedInterp y0 y1 t1 -< ()
    returnA -< Point2 x y

stdHPEffect delta = do
    object <- gets actionObject
    Embedding embed <- ask
    let newHP = min maxHP $ max 0 (oldHP + delta)
        oldHP = ppmnHitPoints object
        maxHP = ppmnMaxHitPoints object
    final <- lift . swont $ embed (changeHp oldHP newHP maxHP)
    modify (actionUpdateObject (object { ppmnHitPoints = final }))
    return final

changeHp oldHP newHP maxHP = changer &&& after duration newHP
  where
    duration = 1.5 * (abs $ newHP - oldHP) / maxHP
    changer = timedInterp oldHP newHP duration

playCry p = momentary (playSoloSound (ppmnCry p))

momentary output = do
    Embedding embed <- ask
    lift . dSwont $ embed $ constant output &&& now ()

over interval sf = do
    Embedding embed <- ask
    lift . swont $ embed $ sf &&& (after interval () >>> iPre NoEvent)
