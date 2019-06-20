{-# LANGUAGE OverloadedStrings #-}

module Field.Personae.Kid (kid) where

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

kid (x0, y0) = Character {
    cAnimation = keep (gait (kid (x0, y0))),
    cDirection = South,
    cDraw = const (const nullOut),
    cGaits = CharacterGaits east north south west,
    cIndex = 0,
    cName = Kid,
    cOccupy = occupy x0 y0,
    cPosition = (fromIntegral x0, fromIntegral y0),
    cRandomGenerator = mkStdGen 0,
    cSpeed = 4.0,
    cStances = CharacterStances right back front left
}

back       = constant $ drawXlatedSprite KidBack
backLeft   = constant $ drawXlatedSprite KidBackStepLeft
backRight  = constant $ drawXlatedSprite KidBackStepRight
left       = constant $ drawXlatedSprite KidLeft
leftStep   = constant $ drawXlatedSprite KidLeftStep
front      = constant $ drawXlatedSprite KidFront
frontLeft  = constant $ drawXlatedSprite KidFrontStepLeft
frontRight = constant $ drawXlatedSprite KidFrontStepRight
right      = constant $ drawXlatedSprite KidRight
rightStep  = constant $ drawXlatedSprite KidRightStep

north = let frames = [backLeft, back, backRight, back] ++ frames      in timedSequence 0.125 frames
west  = let frames = [leftStep, left] ++ frames                       in timedSequence 0.125 frames
south = let frames = [frontLeft, front, frontRight, front] ++ frames  in timedSequence 0.125 frames
east  = let frames = [rightStep, right] ++ frames                     in timedSequence 0.125 frames
