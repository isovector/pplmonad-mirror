{-# LANGUAGE OverloadedStrings #-}

module Ppmn.Species.Incub where

import FRP.Yampa.Geometry

import Battle.MoveName
import LabelName
import Ppmn.Parameters
import ProseName
import SpriteName
import SoundName

incubBase = Ppmn {
    ppmnAccuracyStage = 0,
    ppmnAttack = 56,
    ppmnAttackStage = 0,
    ppmnCry = SoundName.Incub,
    ppmnDefense = 35,
    ppmnDefenseStage = 0,
    ppmnDescription = ItIsBornRichAnd,
    ppmnElement = TwoElements Ppmn.Parameters.Greed Ppmn.Parameters.Wit,
    ppmnEpithet = Talent,
    ppmnEvasivenessStage = 0,
    ppmnHitPoints = 30,
    ppmnLevel = 3,
    ppmnMaxHitPoints = 30,
    ppmnMoves = [],
    ppmnName = LabelName.Incub,
    ppmnNumber = 2,
    ppmnPosition = Point2 0 0,
    ppmnSpeed = 72,
    ppmnSprite = IncubFront,
    ppmnSpriteSet = PpmnSpriteSet { pssBack = IncubBack, pssFront = IncubFront }
}

incubLearnMove 1 = Just Battle.MoveName.SuckerPunch
incubLearnMove 3 = Just Battle.MoveName.EyePoke
incubLearnMove _ = Nothing
