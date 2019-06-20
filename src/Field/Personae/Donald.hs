{-# LANGUAGE OverloadedStrings #-}

module Field.Personae.Donald (donald) where

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

donald (x0, y0) = Character {
    cAnimation = keep (gait (donald (x0, y0))),
    cDirection = South,
    cDraw = const (const nullOut),
    cGaits = CharacterGaits east north south west,
    cIndex = 0,
    cName = Field.PersonaName.Donald,
    cOccupy = occupy x0 y0,
    cPosition = (fromIntegral x0, fromIntegral y0),
    cRandomGenerator = mkStdGen 0,
    cSpeed = 4.0,
    cStances = CharacterStances right back front left
}

back       = constant $ drawXlatedSprite DonaldBack
backLeft   = constant $ drawXlatedSprite DonaldBackStepLeft
backRight  = constant $ drawXlatedSprite DonaldBackStepRight
left       = constant $ drawXlatedSprite DonaldLeft
leftStep   = constant $ drawXlatedSprite DonaldLeftStep
front      = constant $ drawXlatedSprite DonaldFront
frontLeft  = constant $ drawXlatedSprite DonaldFrontStepLeft
frontRight = constant $ drawXlatedSprite DonaldFrontStepRight
right      = constant $ drawXlatedSprite DonaldRight
rightStep  = constant $ drawXlatedSprite DonaldRightStep

north = let frames = [backLeft, back, backRight, back] ++ frames      in timedSequence 0.125 frames
west  = let frames = [leftStep, left] ++ frames                       in timedSequence 0.125 frames
south = let frames = [frontLeft, front, frontRight, front] ++ frames  in timedSequence 0.125 frames
east  = let frames = [rightStep, right] ++ frames                     in timedSequence 0.125 frames
