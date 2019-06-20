{-# LANGUAGE OverloadedStrings #-}

module Ppmn.Species.Unner where

import FRP.Yampa.Geometry

import Battle.MoveName
import LabelName
import Ppmn.Parameters
import ProseName
import SpriteName
import SoundName

unnerBase = Ppmn {
    ppmnAccuracyStage = 0,
    ppmnAttack = 26,
    ppmnAttackStage = 0,
    ppmnCry = SoundName.Unner,
    ppmnDefense = 22,
    ppmnDefenseStage = 0,
    ppmnDescription = IntelligentButSkittishItTends,
    ppmnElement = OneElement Ppmn.Parameters.Wit,
    ppmnEpithet = Supplicant,
    ppmnEvasivenessStage = 0,
    ppmnHitPoints = 25,
    ppmnLevel = 4,
    ppmnMaxHitPoints = 30,
    ppmnMoves = [],
    ppmnName = LabelName.Unner,
    ppmnNumber = 2,
    ppmnPosition = Point2 0 0,
    ppmnSpeed = 72,
    ppmnSprite = UnnerFront,
    ppmnSpriteSet = PpmnSpriteSet { pssBack = UnnerBack, pssFront = UnnerFront }
}

unnerLearnMove 1 = Just Battle.MoveName.SuckerPunch
unnerLearnMove 3 = Just Battle.MoveName.EyePoke
unnerLearnMove _ = Nothing
