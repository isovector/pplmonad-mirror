{-# LANGUAGE OverloadedStrings #-}

module Field.Personae.Protagonist (protagonist) where

import FRP.Yampa hiding (left, right)
import System.Random

import Field.CardinalDirection
import Field.Character
import Field.Output
import Field.PersonaName
import Field.Terrain
import Lightarrow
import OfflineData
import SpriteName

protagonist (x0, y0) = Character {
    cAnimation = keep (gait (protagonist (x0, y0))),
    cDirection = South,
    cDraw = const (const nullOut),
    cGaits = CharacterGaits east north south west,
    cIndex = 0,
    cName = Protagonist,
    cOccupy = occupy x0 y0,
    cPosition = (fromIntegral x0, fromIntegral y0),
    cRandomGenerator = mkStdGen 0,
    cSpeed = 4.0,
    cStances = CharacterStances right back front left
}

back       = constant $ drawXlatedSprite AvatarBack
backLeft   = constant $ drawXlatedSprite AvatarBackStepLeft
backRight  = constant $ drawXlatedSprite AvatarBackStepRight
left       = constant $ drawXlatedSprite AvatarLeft
leftStep   = constant $ drawXlatedSprite AvatarLeftStep
front      = constant $ drawXlatedSprite AvatarFront
frontLeft  = constant $ drawXlatedSprite AvatarFrontStepLeft
frontRight = constant $ drawXlatedSprite AvatarFrontStepRight
right      = constant $ drawXlatedSprite AvatarRight
rightStep  = constant $ drawXlatedSprite AvatarRightStep

north = let frames = [backLeft, back, backRight, back] ++ frames      in timedSequence 0.125 frames
west  = let frames = [leftStep, left] ++ frames                       in timedSequence 0.125 frames
south = let frames = [frontLeft, front, frontRight, front] ++ frames  in timedSequence 0.125 frames
east  = let frames = [rightStep, right] ++ frames                     in timedSequence 0.125 frames
