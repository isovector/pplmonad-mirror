{-# LANGUAGE Arrows, FlexibleContexts, NoMonomorphismRestriction #-}

module Battle.Moves.SuckerPunch (
    suckerPunchEnemy,
    suckerPunchFriend
) where

import Control.Monad.Reader
import Control.Monad.State
import FRP.Yampa
import Data.Vector2
import Data.Point2
import Data.AffineSpace

import Activity
import Battle.Activity
import Battle.Output
import Battle.Parameters
import LabelName
import Lightarrow
import OfflineData
import Output
import Ppmn.Output
import Ppmn.Parameters
import SoundName
import SpriteName

suckerPunchFriend = suckerPunch { mpCont = move animation }
suckerPunchEnemy = suckerPunch { mpCont = move animation }

suckerPunch = MoveParameters {
    mpAccuracy = 0.95,
    mpCont = return (),
    mpElement = Ppmn.Parameters.Normal,
    mpName = SuckerPunch,
    mpPower = 45
}

move animation = do
    scene <- gets sceneDefault
    summary <- gets sceneSummary
    let embed = Embedding (>>> first (arr (>.= (scene >.= summary))))
    local (const embed) (localMove animation)

localMove animation = do
    lastEffect <- gets moveEffectiveness
    scene <- gets sceneDefault
    summary <- gets sceneSummary
    let report = do
            scene <- gets sceneDefault
            summary <- gets sceneSummary
            let embed = Embedding (>>> first (arr (>.= (scene >.= summary))))
            local (const embed) stdEffectReport
        success = stdMoveExecute effect report
        effect out = do
            local (const $ embedArr (>.= (summary >.= out))) $ animation
            damage <- gets moveDamage
            let embed = Embedding (>>> first (arr (scene >.=)))
            local (const embed) $ stdDamageEffect out damage
        failure = stdMoveMiss missProse
    stdMoveAttempt success failure

animation = do
    sub <- gets actionSubject
    obj <- gets actionObject
    scene <- gets sceneDefault
    let xSub = ppmnPosition sub
        xObj = ppmnPosition obj
        drawS = drawPpmn sub
        drawO = drawPpmn obj
        shakeL = drawPpmn $ sub { ppmnPosition = xSub .+^ vector2 (-5) 0 }
        shakeR = drawPpmn $ sub { ppmnPosition = xSub .+^ vector2 5 0 }
    momentary (scene >.= playSound Shake)
    over 0.25 $ constant 0.05 >>> flasher shakeL shakeR >>> arr (>.= drawO)
    momentary (scene >.= playSound Punch)
    over 0.1 $ constant (drawS >.= drawO >.= drawSprite SparkBig (xObj .+^ vector2 12 12))
    momentary (scene >.= playSound Falter)
    over 1.0 $ constant 0.15 >>> flasher nullOut drawO >>> arr (>.= drawS)
