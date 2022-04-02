{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Battle.Moves.EyePoke (
    eyePokeFriend,
    eyePokeEnemy
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

eyePokeFriend = eyePoke { mpCont = move }
eyePokeEnemy = eyePoke { mpCont = move }

eyePoke = MoveParameters {
    mpAccuracy = 0.75,
    mpCont = return (),
    mpElement = Ppmn.Parameters.Normal,
    mpName = EyePoke,
    mpPower = 3
}

move = do
    scene <- gets sceneDefault
    summary <- gets sceneSummary
    let addStatic = first (arr ((scene >.= summary) >.=))
        embed1 = embedArr (summary >.=)
        effect out = do
            local (const $ embed1 `compEmbed` embedArr (>.= out)) animation
            damage <- gets moveDamage
            local (const $ embedArr (scene >.=)) $ stdDamageEffect out damage
            summary' <- gets sceneSummary
            local (const $ embedArr ((scene >.= summary') >.=)) $ stdAccuracyDebuffEffect
        execute = stdMoveExecute effect (return ())
    let embed2 = Embedding (>>> addStatic)
    local (const embed2) $ stdMoveAttempt execute stdMoveFail

animation = do
    sub <- gets actionSubject
    obj <- gets actionObject
    let xO    = ppmnPosition obj
        drawS = drawPpmn sub
        drawO = drawPpmn obj
    local (`compEmbed` embedArr ((drawO >.= drawS) >.=)) $ do
        momentary $ playSound Poke
        over 0.1 $ constant (drawSprite SparkSmall (xO .+^ vector2 24 0))
        over 0.1 $ constant nullOut
        momentary $ playSound Poke
        over 0.1 $ constant (drawSprite SparkSmall (xO .+^ vector2 16 8))
        over 0.1 $ constant nullOut
        momentary $ playSound Poke
        over 0.1 $ constant (drawSprite SparkSmall (xO .+^ vector2 32 16))
        over 0.1 $ constant nullOut
        momentary (playSound Falter)
    local (`compEmbed` embedArr (drawS >.=)) $ do
        over 1.0 $ constant 0.15 >>> flasher nullOut drawO
