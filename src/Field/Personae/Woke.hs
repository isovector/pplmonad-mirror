{-# LANGUAGE OverloadedStrings #-}

module Field.Personae.Woke (msWoke) where

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

msWoke (x0, y0) = Character {
    cAnimation = keep (gait (msWoke (x0, y0))),
    cDirection = South,
    cDraw = const (const nullOut),
    cGaits = CharacterGaits east north south west,
    cIndex = 0,
    cName = Field.PersonaName.Protagonist,
    cOccupy = occupy x0 y0,
    cPosition = (fromIntegral x0, fromIntegral y0),
    cRandomGenerator = mkStdGen 0,
    cSpeed = 4.0,
    cStances = CharacterStances right back front left
}

back       = constant $ drawXlatedSprite WokeBack
backLeft   = constant $ drawXlatedSprite WokeBackStepLeft
backRight  = constant $ drawXlatedSprite WokeBackStepRight
left       = constant $ drawXlatedSprite WokeLeft
leftStep   = constant $ drawXlatedSprite WokeLeftStep
front      = constant $ drawXlatedSprite WokeFront
frontLeft  = constant $ drawXlatedSprite WokeFrontStepLeft
frontRight = constant $ drawXlatedSprite WokeFrontStepRight
right      = constant $ drawXlatedSprite WokeRight
rightStep  = constant $ drawXlatedSprite WokeRightStep

north = let frames = [backLeft, back, backRight, back] ++ frames      in timedSequence 0.125 frames
west  = let frames = [leftStep, left] ++ frames                       in timedSequence 0.125 frames
south = let frames = [frontLeft, front, frontRight, front] ++ frames  in timedSequence 0.125 frames
east  = let frames = [rightStep, right] ++ frames                     in timedSequence 0.125 frames
