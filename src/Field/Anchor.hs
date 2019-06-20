{-# LANGUAGE Arrows, FlexibleContexts, NoMonomorphismRestriction, OverloadedStrings #-}

module Field.Anchor where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import FRP.Yampa

import ControlsMaps
import Field.Avatar
import Field.Character
import {-# SOURCE #-} Field.Editor
import Field.Output
import Field.Parameters
import Field.Terrain
import Lightarrow
import Message
import OfflineData

anchor scene = do
    l <- gets fpLocale
    a <- gets fpAvatar
    (next, (out, a', l')) <- lift . swont $ scene (animateAvatar (snap a)) l
    modify (\fp -> fp { fpAvatar = a', fpLocale = l' })
    local (const $ embedArr (out >.=)) next
    anchor scene

explore avatar (KeepSF kept) = proc controls -> do
    command   <- avatarControl             -< controls
    rMsg      <- initially (Event Restart) -< NoEvent
    rec
        t'                                 <- iPre empty   -< et 
        (a, aMsg)                          <- avatar       -< (t', command)
        (((lDraw, sound, t), lMsg), kept') <- kept         -< (a, rMsg `lMerge` aMsg)
        lMsg'                              <- iPre NoEvent -< lMsg
        let xlate = characterXlate a
        (eIO, et) <- fieldEditor -< (controls, xlate, t)
    let action = getFieldAction lMsg' `lMerge` getFieldAction aMsg
        aDraw  = drawCharacter a xlate
        tDraw  = drawTerrain et xlate
        draw   = tDraw >.= lDraw >.= aDraw
    returnA -< (draw >.= sound >.= eIO, action `attach` (draw, a, kept'))

changeScene scene map = do
    fp <- get
    let world = M.insert (fpMap fp) (fpLocale fp) (fpWorld fp)
        locale = world M.! map
    modify (\fp -> fp { fpLocale = locale, fpMap = map, fpWorld = world })
    local (const $ Embedding id) (anchor scene)

observe avatar (KeepSF kept) = proc _ -> do
    rec
        t'                                 <- iPre empty   -< t
        (a, aMsg)                          <- avatar       -< (t', NoEvent)
        (((lDraw, sound, t), lMsg), kept') <- kept         -< (a, aMsg)
        lMsg'                              <- iPre NoEvent -< lMsg
    let action = getFieldAction lMsg'
        xlate  = characterXlate a
        aDraw  = drawCharacter a xlate
        tDraw  = drawTerrain t xlate
        draw   = tDraw >.= lDraw >.= aDraw
    returnA -< (draw >.= sound, action `attach` (draw, a, kept'))

sceneWith f g scene = (. g) . scene . f

passAvatarReport (KeepSF kept) = KeepSF $ proc (a, aMsg) -> do
    (((lDraw, sound, t), lMsg), kept') <- kept -< (a, aMsg)
    returnA -< (((lDraw, sound, t), lMsg `lMerge` aMsg), kept')

getFieldAction (Event (FieldAction x)) = Event x
getFieldAction _                       = NoEvent
