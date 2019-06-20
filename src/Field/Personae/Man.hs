{-# LANGUAGE OverloadedStrings #-}

module Field.Personae.Man (man) where

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

man (x0, y0) = Character {
    cAnimation = keep (gait (man (x0, y0))),
    cDirection = South,
    cDraw = const (const nullOut),
    cGaits = CharacterGaits east north south west,
    cIndex = 0,
    cName = Man,
    cOccupy = occupy x0 y0,
    cPosition = (fromIntegral x0, fromIntegral y0),
    cRandomGenerator = mkStdGen 0,
    cSpeed = 4.0,
    cStances = CharacterStances right back front left
}

back       = constant $ drawXlatedSprite ManBack
backLeft   = constant $ drawXlatedSprite ManBackStepLeft
backRight  = constant $ drawXlatedSprite ManBackStepRight
left       = constant $ drawXlatedSprite ManLeft
leftStep   = constant $ drawXlatedSprite ManLeftStep
front      = constant $ drawXlatedSprite ManFront
frontLeft  = constant $ drawXlatedSprite ManFrontStepLeft
frontRight = constant $ drawXlatedSprite ManFrontStepRight
right      = constant $ drawXlatedSprite ManRight
rightStep  = constant $ drawXlatedSprite ManRightStep

north = let frames = [backLeft, back, backRight, back] ++ frames      in timedSequence 0.125 frames
west  = let frames = [leftStep, left] ++ frames                       in timedSequence 0.125 frames
south = let frames = [frontLeft, front, frontRight, front] ++ frames  in timedSequence 0.125 frames
east  = let frames = [rightStep, right] ++ frames                     in timedSequence 0.125 frames
