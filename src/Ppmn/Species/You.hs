{-# LANGUAGE OverloadedStrings #-}

module Ppmn.Species.You where

import FRP.Yampa.Geometry

import Battle.MoveName
import LabelName
import Ppmn.Parameters
import ProseName
import SpriteName
import SoundName

youBase = Ppmn {
    ppmnAccuracyStage = 0,
    ppmnAttack = 56,
    ppmnAttackStage = 0,
    ppmnCry = SoundName.Unner,
    ppmnDefense = 35,
    ppmnDefenseStage = 0,
    ppmnDescription = AtRestItOftenSpews,
    ppmnElement = OneElement Ppmn.Parameters.Apathy,
    ppmnEpithet = Oaf,
    ppmnEvasivenessStage = 0,
    ppmnHitPoints = 30,
    ppmnLevel = 1,
    ppmnMaxHitPoints = 30,
    ppmnMoves = [],
    ppmnName = Blank,
    ppmnNumber = 1,
    ppmnPosition = Point2 0 0,
    ppmnSpeed = 72,
    ppmnSprite = AvatarPortrait,
    ppmnSpriteSet = PpmnSpriteSet { pssBack = BattleAvatarBack, pssFront = AvatarPortrait }
}

youLearnMove 1 = Just Battle.MoveName.SuckerPunch
youLearnMove 3 = Just Battle.MoveName.EyePoke
youLearnMove _ = Nothing
