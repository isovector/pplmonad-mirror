{-# LANGUAGE OverloadedStrings #-}

module Ppmn.Species.Blamotage where

import Data.Point2

import Battle.MoveName
import LabelName
import Ppmn.Parameters
import ProseName
import SpriteName
import SoundName

blamotageBase = Ppmn {
    ppmnAccuracyStage = 0,
    ppmnAttack = 52,
    ppmnAttackStage = 0,
    ppmnCry = SoundName.Blamotage,
    ppmnDefense = 43,
    ppmnDefenseStage = 0,
    ppmnDescription = ShitsOnAnythingToWard,
    ppmnElement = OneElement Ppmn.Parameters.Spite,
    ppmnEpithet = LabelName.Backstabber,
    ppmnEvasivenessStage = 0,
    ppmnHitPoints = 40,
    ppmnLevel = 1,
    ppmnMaxHitPoints = 40,
    ppmnMoves = [],
    ppmnName = LabelName.Blamotage,
    ppmnNumber = 3,
    ppmnPosition = Point2 0 0,
    ppmnSpeed = 65,
    ppmnSprite = BlamotageFront,
    ppmnSpriteSet = PpmnSpriteSet { pssBack = BlamotageBack, pssFront = BlamotageFront }
}

blamotageLearnMove 1 = Just Battle.MoveName.SuckerPunch
blamotageLearnMove 3 = Just Battle.MoveName.EyePoke
blamotageLearnMove 6 = Just Battle.MoveName.MeToo
blamotageLearnMove _ = Nothing
