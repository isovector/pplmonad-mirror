{-# LANGUAGE OverloadedStrings #-}

module Ppmn.Species.Ignoloof where

import Data.Point2

import Battle.MoveName
import LabelName
import Ppmn.Parameters
import ProseName
import SpriteName
import SoundName

ignoloofBase = Ppmn {
    ppmnAccuracyStage = 0,
    ppmnAttack = 56,
    ppmnAttackStage = 0,
    ppmnCry = SoundName.Ignoloof,
    ppmnDefense = 39,
    ppmnDefenseStage = 0,
    ppmnDescription = AtRestItOftenSpews,
    ppmnElement = OneElement Ppmn.Parameters.Apathy,
    ppmnEpithet = Oaf,
    ppmnEvasivenessStage = 0,
    ppmnHitPoints = 35,
    ppmnLevel = 1,
    ppmnMaxHitPoints = 35,
    ppmnMoves = [],
    ppmnName = LabelName.Ignoloof,
    ppmnNumber = 1,
    ppmnPosition = Point2 0 0,
    ppmnSpeed = 75,
    ppmnSprite = IgnoloofFront,
    ppmnSpriteSet = PpmnSpriteSet { pssBack = IgnoloofBack, pssFront = IgnoloofFront }
}

ignoloofLearnMove 1 = Just Battle.MoveName.SuckerPunch
ignoloofLearnMove 3 = Just Battle.MoveName.EyePoke
ignoloofLearnMove _ = Nothing
