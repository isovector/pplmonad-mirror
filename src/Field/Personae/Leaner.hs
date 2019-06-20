{-# LANGUAGE OverloadedStrings #-}

module Field.Personae.Leaner (leaner) where

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

leaner (x0, y0) = Character {
    cAnimation = keep (gait (leaner (x0, y0))),
    cDirection = South,
    cDraw = const (const nullOut),
    cGaits = CharacterGaits east north south west,
    cIndex = 0,
    cName = Field.PersonaName.Leaner,
    cOccupy = occupy x0 y0,
    cPosition = (fromIntegral x0, fromIntegral y0),
    cRandomGenerator = mkStdGen 0,
    cSpeed = 4.0,
    cStances = CharacterStances right back front left
}

back       = constant $ drawXlatedSprite LeanerBack
backLeft   = constant $ drawXlatedSprite LeanerBackStepLeft
backRight  = constant $ drawXlatedSprite LeanerBackStepRight
left       = constant $ drawXlatedSprite LeanerLeft
leftStep   = constant $ drawXlatedSprite LeanerLeftStep
front      = constant $ drawXlatedSprite LeanerFront
frontLeft  = constant $ drawXlatedSprite LeanerFrontStepLeft
frontRight = constant $ drawXlatedSprite LeanerFrontStepRight
right      = constant $ drawXlatedSprite LeanerRight
rightStep  = constant $ drawXlatedSprite LeanerRightStep

north = let frames = [backLeft, back, backRight, back] ++ frames      in timedSequence 0.125 frames
west  = let frames = [leftStep, left] ++ frames                       in timedSequence 0.125 frames
south = let frames = [frontLeft, front, frontRight, front] ++ frames  in timedSequence 0.125 frames
east  = let frames = [rightStep, right] ++ frames                     in timedSequence 0.125 frames
