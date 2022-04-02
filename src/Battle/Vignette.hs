{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, OverloadedStrings #-}

module Battle.Vignette where

import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Maybe
import FRP.Yampa
import Data.Vector2
import Data.Point2
import Data.AffineSpace

import Activity
import Battle.Activity
import Battle.Output
import Battle.Parameters
import Lightarrow
import OfflineData
import Output
import Ppmn.Output
import Ppmn.Parameters
import SoundName
import SpriteName
import StateClass
import TextBox as TB

wildBattleIntro = do
    facingOffWild
    let drawAvatar = drawSprite BattleAvatarBack (Point2 8 40)
    drawEnemy <- gets sceneSecond
    drawSummary <- gets sceneSecondSummary
    let embed1 = embedArr ((drawEnemy >.= drawAvatar >.= TB.drawConstant "") >.=)
    enemy <- gets battleEnemy
    local (const embed1) $ do
        playCry enemy
        stdLecture (wildAlertProse enemy) >>= arrowWait
    avatarResponse

trainerBattleIntro = do
    trainerName <- gets (etName . fromJust . bpEnemyTrainer)
    trainerSprite <- gets (etSprite . fromJust . bpEnemyTrainer)
    facingOffTrainer trainerSprite
    let drawAvatar = drawSprite BattleAvatarBack (Point2 8 40)
        drawTrainer = drawSprite trainerSprite (Point2 100 0)
        embed1 = embedArr ((drawTrainer >.= drawAvatar >.= TB.drawConstant "") >.=)
        embed2 = embedArr (\x -> TB.drawConstant "" >.= x >.= drawAvatar)
    local (const embed1) $ do
        momentary $ playSound Bell
        stdLecture (trainerAlertProse trainerName) >>= arrowWait
    local (const embed2) $ steppingBackTrainer trainerSprite >> deployEnemy
    avatarResponse

trainerBattleDenouement = do
    trainerSprite <- gets (etSprite . fromJust . bpEnemyTrainer)
    trainerProse <- gets (etSpeech . fromJust .bpEnemyTrainer)
    drawFriend <- gets sceneFirst
    drawSummary <- gets sceneFirstSummary
    let embed1 = embedArr (>.= (drawFriend >.= drawSummary >.= TB.drawConstant ""))
    local (const embed1) $ steppingForwardTrainer trainerSprite
    let drawTrainer = drawSprite trainerSprite (Point2 100 0)
        embed2 = embedArr ((drawTrainer >.= drawFriend >.= drawSummary) >.=)
    local (const embed2) $ do
        n1 <- stdLecture (prose trainerProse)
        stdWait n1
        return (embed2 `compEmbed` embedArr (n1 >.=))

avatarResponse = do
    drawEnemy <- gets sceneSecond
    drawSummary <- gets sceneSecondSummary
    let embed2 = embedArr (>.= (drawEnemy >.= drawSummary >.= TB.drawConstant ""))
    local (const embed2) steppingBackAvatar
    deploy

deploy = do
    drawEnemy <- gets sceneSecond
    drawEnemySummary <- gets sceneSecondSummary
    drawSceneSummary <- gets sceneSummary
    let embed1 = embedArr (>.= (drawEnemy >.= drawEnemySummary))
    narration <- local (const embed1) $ stdCommentary choosePpmnProse
    let embed3 = embedArr (>.= (drawEnemy >.= drawSceneSummary >.= narration))
    local (const embed3) advancingFriend
    drawFriend <- gets sceneFirst
    let embed4 = embedArr (>.= (drawEnemy >.= drawFriend >.= drawSceneSummary))
    friend <- gets battleFriend
    local (const $ compEmbed embed4 (embedArr (>.= narration))) (playCry friend)
    local (const embed4) (stdHesitation narration)

deployEnemy = do
    trainerName <- gets (etName . fromJust . bpEnemyTrainer)
    narration <- stdCommentary (trainerDeployProse trainerName)
    let embed2 = embedArr (>.= narration)
    local (`compEmbed` embed2) advancingEnemy
    enemy <- gets battleEnemy
    drawEnemy <- gets sceneSecond
    let embed3 = embedArr ((narration >.= drawEnemy) >.=)
    local (`compEmbed` embed3) (playCry enemy)
    stdHesitation (drawEnemy >.= narration)

recall = do
    drawEnemy <- gets sceneSecond
    drawEnemySummary <- gets sceneSecondSummary
    drawScene <- gets sceneDefault
    drawSceneSummary <- gets sceneSummary
    let embed1 = embedArr (>.= (drawScene >.= drawSceneSummary))
    narration <- local (const embed1) $ stdCommentary recallPpmnProse
    let embed2 = embedArr (>.= (drawEnemy >.= drawEnemySummary >.= narration))
    local (const embed2) retreating

faint = do
    drawOther <- gets sceneFirst
    drawOtherSummary <- gets sceneFirstSummary
    let embed1 = embedArr (>.= (drawOther >.= drawOtherSummary >.= TB.drawConstant ""))
    object <- gets actionObject
    local (const embed1) $ do
        momentary $ playSound Death
        descending object
    let embed2 = embedArr (>.= (drawOther >.= drawOtherSummary))
    local (const embed2) (stdLecture faintProse >>= arrowWait)

facingOffWild = gets (ppmnSprite . battleEnemy) >>= facingOff

facingOffTrainer = facingOff

facingOff enemySprite = over interval $ (enemy &&& avatar) >>> merge
  where
    avatar = slide (Point2 164 40) (Point2 8 40) interval BattleAvatarBack
    enemy = slide (Point2 (-56) 0) (Point2 100 0) interval enemySprite
    merge = arr ((>.= TB.drawConstant "") . uncurry (>.=))
    interval = 1.245

retreating = do
    friend <- gets battleFriend
    let p0 = ppmnPosition friend
        p1 = Point2 (-56) (point2Y p0)
        sprite = ppmnSprite friend
    capture friend (vector2 0 5)
    --over 0.25 $ slide p0 p1 0.25 (ppmnSprite friend)

advancingFriend = do
    friend <- gets battleFriend
    let p0 = Point2 (-56) (point2Y p1)
        p1 = ppmnPosition friend
        sprite = ppmnSprite friend
    throwPhone (Point2 (-16) 80) (Point2 28 100) 40 (5/12)
    release friend (vector2 0 5)
    --over 0.25 $ slide p0 p1 0.25 (ppmnSprite friend)

advancingEnemy = do
    enemy <- gets battleEnemy
    let p0 = Point2 156 (point2Y p1)
        p1 = ppmnPosition enemy
        sprite = ppmnSprite enemy
    throwPhone (Point2 160 30) (Point2 124 48) 40 (5/12)
    local (`compEmbed` embedArr (landedPhone >.=)) $ release enemy (vector2 0 0)
    --over 0.25 $ slide p0 p1 0.25 (ppmnSprite enemy)

steppingBackAvatar = over 0.25 $ slide p0 p1 0.25 BattleAvatarBack
  where
    p0 = Point2 8 40
    p1 = Point2 (-56) 40

steppingForwardAvatar = over 0.25 $ slide p0 p1 0.25 BattleAvatarBack
  where
    p0 = Point2 (-56) 40
    p1 = Point2 8 40

steppingBackTrainer sprite = over 0.25 $ slide p0 p1 0.25 sprite
  where
    p0 = Point2 100 0
    p1 = Point2 156 0

steppingForwardTrainer sprite = over 0.25 $ slide p0 p1 0.25 sprite
  where
    p0 = Point2 156 0
    p1 = Point2 100 0

descending object = over 0.25 $ slide p0 p1 0.2 (ppmnSprite object) >>> arr (>.= blank)
  where
    p0 = ppmnPosition object
    p1 = Point2 (point2X p0) (point2Y p0 + 56)
    sprite = ppmnSprite object
    blank = drawRectangle 56 56 White p1

capture p flashOffset = do
    let frames = map constant $ intersperse nullOut drawFlashes
        drawFlashes = map (flip drawSprite (x .+^ flashOffset)) flashes
        x      = ppmnPosition p
        sprite = ppmnSprite p
        shrink = slideStretch 1 0 x (x .+^ vector2 28 56) 0.5 sprite
    momentary (playSound Capture)
    over 0.5 $ (constant 0.03 >>> flasher (drawPpmn p) nullOut) &&& timedSequence 0.05 frames >>> arr (uncurry (>.=))
    over 0.5 $ (((shrink &&& constant nullOut) &&& constant 0.03) >>> flipper) &&& timedSequence 0.05 frames >>> arr (uncurry (>.=))
  where
    flashes = PPhoneFlash1 : PPhoneFlash2 : PPhoneFlash3 : flashes

release p flashOffset = do
    let frames = map constant $ intersperse nullOut drawFlashes
        drawFlashes = map (flip drawSprite (x .+^ flashOffset)) flashes
        x      = ppmnPosition p
        sprite = ppmnSprite p
        expand = slideStretch 0 1 (x .+^ vector2 28 56) x 0.5 sprite
    momentary (playSound SoundName.Release)
    over 0.5 $ timedSequence 0.05 frames
    over 0.5 $ (((expand &&& constant nullOut) &&& constant 0.03) >>> flipper) &&& timedSequence 0.05 frames >>> arr (uncurry (>.=))
  where
    flashes = PPhoneFlash1 : PPhoneFlash2 : PPhoneFlash3 : flashes
