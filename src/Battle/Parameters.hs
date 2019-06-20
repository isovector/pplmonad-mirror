{-# LANGUAGE FlexibleContexts, OverloadedStrings, ViewPatterns #-}

module Battle.Parameters where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import System.Random

import Battle.ElementalEffect
import Controls
import Inventory.Parameters
import LabelName
import Lightarrow
import OfflineData
import Ppmn.Parameters
import ProseName
import SpriteName
import StateClass

import Debug.Trace

class PpmnAction s => BattleMove s where
    moveAccuracy :: s -> Double
    moveElement :: s -> Element
    moveName :: s -> LabelName
    movePower :: s -> Double

class BattleState s where
    battleFriend :: s -> Ppmn
    battleEnemy :: s -> Ppmn

instance TextSource MoveState where
    prose name ms = odGetProse (msOfflineData ms) name
    label name ms = odGetLabel (msOfflineData ms) name

instance PpmnAction MoveState where
    actionObject = msObject
    actionObjectName = msObjectName
    actionSubject = msSubject
    actionSubjectName = msSubjectName
    actionUpdateObject object ms = ms { msObject = object }
    actionUpdateSubject subject ms = ms { msSubject = subject }

instance BattleMove MoveState where
    moveAccuracy = mpAccuracy . msParams
    moveElement = mpElement . msParams
    moveName = mpName . msParams
    movePower  = mpPower . msParams

instance RandomGen MoveState where
    next ms = let (k, g) = next (msRandomGenerator ms) in
                (k, ms { msRandomGenerator = g })
    split ms = let (g1, g2) = split (msRandomGenerator ms) in
                (ms { msRandomGenerator = g1 }, ms { msRandomGenerator = g2 })
    genRange = genRange . msRandomGenerator

enemyMoveState params bp = MoveState {
    msParams = params { mpPower = mpPower params * 0.75, mpAccuracy = mpAccuracy params * 0.9 },
    msSubject = bpEnemy bp,
    msSubjectName = enemyName bp,
    msObject = bpFriend bp,
    msObjectName = friendName bp,
    msOfflineData = bpOfflineData bp,
    msRandomGenerator = bpRandomGenerator bp
}

friendMoveState params bp = MoveState {
    msParams = params { mpPower = mpPower params * 1.25, mpAccuracy = mpAccuracy params * 1.1 },
    msSubject = bpFriend bp,
    msSubjectName = friendName bp,
    msObject = bpEnemy bp,
    msObjectName = enemyName bp,
    msOfflineData = bpOfflineData bp,
    msRandomGenerator = bpRandomGenerator bp
}

friendName s = label (ppmnName $ battleFriend s) s

enemyName s = T.concat [enemy, " ", name]
  where
    enemy = prose Enemy s
    name = label (ppmnName $ battleEnemy s) s

data EnemyTrainer = EnemyTrainer {
    etName :: LabelName,
    etSpeech :: ProseName,
    etSprite :: SpriteName
}

data BattleParameters = BattleParameters {
    bpAvatarName :: T.Text,
    bpCounters :: PpmnCounters,
    bpEnemies :: [Ppmn],
    bpEnemyIndex :: Int,
    bpEnemyTrainer :: Maybe EnemyTrainer,
    bpExit :: () -> FlatEmbeddedActivity BattleParameters Controls OfflineIO (),
    bpFleeAttempts :: Int,
    bpFriendIndex :: Int,
    bpItems :: M.Map LabelName ItemParameters,
    bpLose :: FlatEmbeddedActivity BattleParameters Controls OfflineIO (),
    bpOfflineData :: OfflineData,
    bpPpmn :: [Ppmn],
    bpRandomGenerator :: StdGen,
    bpRun :: FlatEmbeddedActivity BattleParameters Controls OfflineIO (),
    bpWin :: FlatEmbeddedActivity BattleParameters Controls OfflineIO ()
}

bpEnemy = bpEnemies >>= \ppmn -> (ppmn !!) . bpEnemyIndex
bpFriend = bpPpmn >>= \ppmn -> (ppmn !!) . bpFriendIndex

instance TextSource BattleParameters where
    prose name bp = odGetProse (bpOfflineData bp) name
    label name bp = odGetLabel (bpOfflineData bp) name

instance BattleState BattleParameters where
    battleFriend = bpFriend
    battleEnemy = bpEnemy

instance RandomGen BattleParameters where
    next bp = let (k, g) = next (bpRandomGenerator bp) in
                (k, bp { bpRandomGenerator = g })
    split bp = let (g1, g2) = split (bpRandomGenerator bp) in
                (bp { bpRandomGenerator = g1 }, bp { bpRandomGenerator = g2 })
    genRange = genRange . bpRandomGenerator

moveSucceeds :: (BattleMove s, RandomGen s, MonadState s m) => m Bool
moveSucceeds = do
    moveAcc <- gets moveAccuracy
    accuracyStage <- gets (ppmnAccuracyStage . actionSubject)
    evasivenessStage <- gets (ppmnEvasivenessStage . actionObject)
    let accuracy = stageMultiplier accuracyStage * moveAcc
        evasiveness = stageMultiplier evasivenessStage
    threshold <- state (randomR (0.0, 1.0))
    Debug.Trace.traceShow (threshold :: Double) $ return ()
    return (accuracy / evasiveness > (threshold :: Double))

moveEffectiveness :: BattleMove s => s -> Effectiveness
moveEffectiveness s =
    case objectElement of
        OneElement e      -> effectiveness (moveElement s) e
        TwoElements e1 e2 -> effectiveness (moveElement s) e1 <> effectiveness (moveElement s) e2
  where
    objectElement = ppmnElement (actionObject s)

moveDamage :: BattleMove s => s -> Int
moveDamage s = round $ levelFactor * power * attack / defense / 50 + 2
  where
    levelFactor = 2 / 5 * fromIntegral level + 2
    attack = baseAttack * stageMultiplier attackStage
    defense = baseDefense * stageMultiplier defenseStage
    power = movePower s
    Ppmn { ppmnAttack = baseAttack, ppmnAttackStage = attackStage, ppmnLevel = level } = actionSubject s
    Ppmn { ppmnDefense = baseDefense, ppmnDefenseStage = defenseStage } = actionObject s

stageMultiplier :: Int -> Double
stageMultiplier (-6) = 1 / 4
stageMultiplier (-5) = 7 / 25
stageMultiplier (-4) = 1 / 3
stageMultiplier (-3) = 2 / 5
stageMultiplier (-2) = 1 / 2
stageMultiplier (-1) = 2 / 3
stageMultiplier 0 = 1 
stageMultiplier 1 = 3 / 2
stageMultiplier 2 = 2
stageMultiplier 3 = 5 / 2
stageMultiplier 4 = 3
stageMultiplier 5 = 7 / 2
stageMultiplier 6 = 4
