{-# LANGUAGE OverloadedStrings #-}

module Field.Personae.Mom (mom) where

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

mom (x0, y0) = Character {
    cAnimation = keep (gait (mom (x0, y0))),
    cDirection = South,
    cDraw = const (const nullOut),
    cGaits = CharacterGaits east north south west,
    cIndex = 0,
    cName = Mom,
    cOccupy = occupy x0 y0,
    cPosition = (fromIntegral x0, fromIntegral y0),
    cRandomGenerator = mkStdGen 0,
    cSpeed = 4.0,
    cStances = CharacterStances right back front left
}

back       = constant $ drawXlatedSprite MomBack
backLeft   = constant $ drawXlatedSprite MomBackStepLeft
backRight  = constant $ drawXlatedSprite MomBackStepRight
left       = constant $ drawXlatedSprite MomLeft
leftStep   = constant $ drawXlatedSprite MomLeftStep
front      = constant $ drawXlatedSprite MomFront
frontLeft  = constant $ drawXlatedSprite MomFrontStepLeft
frontRight = constant $ drawXlatedSprite MomFrontStepRight
right      = constant $ drawXlatedSprite MomRight
rightStep  = constant $ drawXlatedSprite MomRightStep

north = let frames = [backLeft, back, backRight, back] ++ frames      in timedSequence 0.125 frames
west  = let frames = [leftStep, left] ++ frames                       in timedSequence 0.125 frames
south = let frames = [frontLeft, front, frontRight, front] ++ frames  in timedSequence 0.125 frames
east  = let frames = [rightStep, right] ++ frames                     in timedSequence 0.125 frames
