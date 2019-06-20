{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings, Rank2Types, NoMonomorphismRestriction #-}

module Battle.Activity where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import FRP.Yampa

import Activity
import Battle.ElementalEffect
import Battle.Output
import Battle.Parameters
import Inventory.Parameters
import LabelName
import Lightarrow
import Output
import Ppmn.Output
import Ppmn.Parameters
import ProseName
import OfflineData
import SoundName
import StateClass

stdMoveAttempt succeeding failing = do
    narration <- stdCommentary announceProse
    success <- moveSucceeds
    if success
        then succeeding narration
        else stdHesitation narration >> failing

stdMoveExecute effect report announcement = do
    effectiveness <- gets moveEffectiveness
    if effectiveness == Futile
        then stdHesitation announcement 
        else effect announcement
    report

stdAttackBuffEffect = buffEffect 1 ppmnAttackStage (\p s -> p { ppmnAttackStage = s }) Attack

stdAttackGreatBuffEffect = buffEffect 2 ppmnAttackStage (\p s -> p { ppmnAttackStage = s }) Attack

stdAttackDebuffEffect = debuffEffect (-1) ppmnAttackStage (\p s -> p { ppmnAttackStage = s }) Attack

stdAttackGreatDebuffEffect = debuffEffect (-2) ppmnAttackStage (\p s -> p { ppmnAttackStage = s }) Attack

stdDefenseBuffEffect = buffEffect 1 ppmnDefenseStage (\p s -> p { ppmnDefenseStage = s }) Defense

stdDefenseGreatBuffEffect = buffEffect 2 ppmnDefenseStage (\p s -> p { ppmnDefenseStage = s }) Defense

stdDefenseDebuffEffect = debuffEffect (-1) ppmnDefenseStage (\p s -> p { ppmnDefenseStage = s }) Defense

stdDefenseGreatDebuffEffect = debuffEffect (-2) ppmnDefenseStage (\p s -> p { ppmnDefenseStage = s }) Defense

stdAccuracyBuffEffect = buffEffect 1 ppmnAccuracyStage (\p s -> p { ppmnAccuracyStage = s }) Accuracy

stdAccuracyGreatBuffEffect = buffEffect 2 ppmnAccuracyStage (\p s -> p { ppmnAccuracyStage = s }) Accuracy

stdAccuracyDebuffEffect = debuffEffect (-1) ppmnAccuracyStage (\p s -> p { ppmnAccuracyStage = s }) Accuracy

stdAccuracyGreatDebuffEffect = debuffEffect (-2) ppmnAccuracyStage (\p s -> p { ppmnAccuracyStage = s }) Accuracy

buffEffect = stageEffect actionSubject actionUpdateSubject actionSubjectName

debuffEffect = stageEffect actionObject actionUpdateObject actionObjectName

stageEffect getPpmn putPpmn getName delta getStage putStage statName = do
    ppmn <- gets getPpmn
    pName <- gets getName
    let stage = getStage ppmn
        newPpmn = putStage ppmn $ max (-6) $ min 6 (stage + delta)
    modify (putPpmn newPpmn)
    case delta of
        d | d < (-1) -> stdCommentary (greatDebuffProse pName statName) >>= stdHesitation
        d | d < 0    -> stdCommentary (debuffProse pName statName) >>= stdHesitation
        d | d > 1    -> stdCommentary (greatBuffProse pName statName) >>= stdHesitation
        d | d > 0    -> stdCommentary (buffProse pName statName) >>= stdHesitation
            
stdEffectReport = do
    effectiveness <- gets moveEffectiveness
    if effectiveness == Basic
        then return ()
        else do
            narration <- stdCommentary (effectProse effectiveness)
            stdHesitation narration

longEffectReport = do
    effectiveness <- gets moveEffectiveness
    narration <- stdCommentary (effectProse effectiveness)
    stdHesitation narration

stdMoveFail = stdCommentary (prose ButItFailed) >>= stdHesitation

stdMoveMiss missProse = stdCommentary missProse >>= stdHesitation

subjectSummaryEmbedding = do
    sub <- gets actionSubject
    obj <- gets actionObject
    let objSummary hp = statusSummary (obj { ppmnHitPoints = hp })
        subSummary = statusSummary sub
        addStatic = (subSummary >.=)
    return $ embedArr (addStatic . objSummary)
    
stdDamageEffect announcement damage = do
    e1 <- subjectSummaryEmbedding
    let e2 = compEmbed (embedArr (announcement >.=)) e1
    withEmbedding (`compEmbed` e2) (stdHPEffect (- fromIntegral damage)) 
    return ()

stdHealingEffect healing = do
    embedding <- subjectSummaryEmbedding
    withEmbedding (`compEmbed` embedding) (stdHPEffect healing) 
    return ()

flee = do
    sF <- gets ((/ 255) . ppmnSpeed . bpFriend)
    sE <- gets ((/ 255) . ppmnSpeed . bpEnemy)
    attempts <- gets bpFleeAttempts
    let x = (sF / (2 * sE)) + (fromIntegral attempts / 8)
    threshold <- state (randomR (0.0, 1.0))
    if x > threshold
        then do
            narration <- stdLecture (const "Got away safely.")
            exit <- gets bpExit
            arrowWait narration
            scene <- gets sceneDefault
            summary <- gets sceneSummary
            let embedding = embedArr ((scene >.= summary >.= narration) >.=)
            local (const embedding) exitFade >> exit ()
        else do
            modify (\bp -> bp { bpFleeAttempts = bpFleeAttempts bp + 1 })
            stdLecture (const "Can't get away!") >>= arrowWait
            return ()

exitFade = fadeTo White 0.5

pickUpItem item number
    | number < 1 = return nullOut
    | otherwise = do
        modify (\bp -> bp { bpItems =
            let items = bpItems bp
                newItem = item { itemStock = number }
            in
                M.insertWith (const increment) name newItem items
            })
        narration <- stdCommentary ((\t -> sentence '!' ["Got", t]) . plural . label name)
        arrowWait narration
        return narration
  where
    name = itemName item
    plural = case number of
        1 -> T.append "a "
        _ -> T.append (T.pack (show number ++ " ")) . flip T.snoc 's'
    increment = (!! number) . iterate itemAdd

throwPhone p0 p1 h interval = over interval $ phoneTrajectory p0 p1 h interval

phoneTrajectory p0 p1 h interval = proc _ -> do
    t <- timedInterp 0 1 interval   -< ()
    let p = ballisticArc p0 p1 h t
        r = round (16 * t)
    returnA -< drawPhone (rotations !! r) p
  where
    rotations = [Original, TurnR, FlipBoth, TurnL] ++ rotations
