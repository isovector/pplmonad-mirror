{-# LANGUAGE OverloadedStrings #-}

module Ppmn.Species.Slidek where

import FRP.Yampa.Geometry

import Battle.MoveName
import LabelName
import Ppmn.Parameters
import ProseName
import SpriteName
import SoundName

slidekBase = Ppmn {
    ppmnAccuracyStage = 0,
    ppmnAttack = 54,
    ppmnAttackStage = 0,
    ppmnCry = SoundName.Slidek,
    ppmnDefense = 39,
    ppmnDefenseStage = 0,
    ppmnDescription = OftenSeenPontificatingInLoud,
    ppmnElement = OneElement Ppmn.Parameters.Wit,
    ppmnEpithet = Weathercock,
    ppmnEvasivenessStage = 0,
    ppmnHitPoints = 40,
    ppmnLevel = 3,
    ppmnMaxHitPoints = 40,
    ppmnMoves = [],
    ppmnName = LabelName.Slidek,
    ppmnNumber = 2,
    ppmnPosition = Point2 0 0,
    ppmnSpeed = 70,
    ppmnSprite = SlidekFront,
    ppmnSpriteSet = PpmnSpriteSet { pssBack = SlidekBack, pssFront = SlidekFront }
}

slidekLearnMove 1 = Just Battle.MoveName.SuckerPunch
slidekLearnMove 3 = Just Battle.MoveName.EyePoke
slidekLearnMove _ = Nothing
