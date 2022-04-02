{-# LANGUAGE StandaloneDeriving #-}

module Ppmn.Parameters where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe
import System.Random
import Data.Point2

import Battle.MoveName
import Controls
import qualified LabelName as Label
import Lightarrow
import OfflineData
import ProseName
import SpriteName
import SoundName

deriving instance (Read a, RealFloat a) => Read (Point2 a)

data Ppmn = Ppmn {
    ppmnAccuracyStage :: Int,
    ppmnAttack :: Double,
    ppmnAttackStage :: Int,
    ppmnCry :: SoundName,
    ppmnDefense :: Double,
    ppmnDefenseStage :: Int,
    ppmnDescription :: ProseName,
    ppmnElement :: ElementPack,
    ppmnEpithet :: Label.LabelName,
    ppmnEvasivenessStage :: Int,
    ppmnHitPoints :: Double,
    ppmnLevel :: Int,
    ppmnMaxHitPoints :: Double,
    ppmnMoves :: [MoveName],
    ppmnName :: Label.LabelName,
    ppmnNumber :: Int,
    ppmnPosition :: Point2 Double,
    ppmnSpeed :: Double,
    ppmnSprite :: SpriteName,
    ppmnSpriteSet :: PpmnSpriteSet
}
    deriving (Read, Show)

data PpmnSpriteSet = PpmnSpriteSet {
    pssBack :: SpriteName,
    pssFront :: SpriteName
}
    deriving (Read, Show)

data MoveState = MoveState {
    msParams :: MoveParameters,
    msSubject :: Ppmn,
    msSubjectName :: T.Text,
    msObject :: Ppmn,
    msObjectName :: T.Text,
    msOfflineData :: OfflineData,
    msRandomGenerator :: StdGen
}

data MoveParameters = MoveParameters {
    mpAccuracy :: Double,
    mpCont :: FlatEmbeddedActivity MoveState Controls OfflineIO (),
    mpElement :: Element,
    mpName :: Label.LabelName,
    mpPower :: Double
}

data ElementPack = OneElement Element | TwoElements Element Element
    deriving (Read, Show)

data Element = Normal
             | Authority
             | Apathy
             | Wit
             | Spite
             | Greed
    deriving (Enum, Eq, Ord, Read, Show)

elementLabel Normal = Label.Normal
elementLabel Authority = Label.Authority
elementLabel Wit = Label.Wit
elementLabel Spite = Label.Spite
elementLabel Apathy = Label.Apathy
elementLabel Greed = Label.Greed

atLevel level learnMove base
    | level < 1  = atLevel 1 learnMove base
    | otherwise  = base {
            ppmnAttack = paramAtLevel level ppmnAttack base,
            ppmnDefense = paramAtLevel level ppmnDefense base,
            ppmnHitPoints = paramAtLevel level ppmnMaxHitPoints base,
            ppmnLevel = level,
            ppmnMaxHitPoints = paramAtLevel level ppmnMaxHitPoints base,
            ppmnMoves = catMaybes (map learnMove [1 .. level]),
            ppmnSpeed = paramAtLevel level ppmnSpeed base
        }

paramAtLevel level param base = (1 + (fromIntegral level) / 50) * param base

resetStages p = p {
    ppmnAccuracyStage = 0,
    ppmnAttackStage = 0,
    ppmnDefenseStage = 0,
    ppmnEvasivenessStage = 0
}

class PpmnAction s where
    actionObject :: s -> Ppmn
    actionObjectName :: s -> T.Text
    actionSubject :: s -> Ppmn
    actionSubjectName :: s -> T.Text
    actionUpdateObject :: Ppmn -> s -> s
    actionUpdateSubject :: Ppmn -> s -> s

data PpmnCounters = PpmnCounters {
    pcCatches :: Int,
    pcKills :: Int,
    pcLosses :: Int,
    pcPlays :: M.Map Label.LabelName Int
}

catch pc@(PpmnCounters { pcCatches = catches }) = pc { pcCatches = catches + 1 }
kill pc@(PpmnCounters { pcKills = kills }) = pc { pcKills = kills + 1 }
lose pc@(PpmnCounters { pcLosses = losses }) = pc { pcLosses = losses + 1 }

mostPlayed :: PpmnCounters -> Maybe Label.LabelName
mostPlayed (PpmnCounters { pcPlays = plays }) = fst $ M.foldlWithKey most (Nothing, 0) plays
  where
    most (oldName, oldCount) name count = case compare oldCount count of
        LT -> (Just name, count)
        EQ -> (oldName >>= Just . min name, count)
        _  -> (oldName, oldCount)
