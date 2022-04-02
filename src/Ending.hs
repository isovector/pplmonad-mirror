{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Ending (gameEnd) where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Text as T
import FRP.Yampa
import Data.Vector2
import Data.Point2
import Data.AffineSpace

import Activity
import Battle
import Field.Parameters
import LabelName
import Lightarrow
import MusicName
import OfflineData
import Output
import ProseName
import Ppmn.Species
import SpriteName
import StateClass

gameEnd fp = do
    steppingBackTrainer Donald
    modify (\bp -> bp { bpEnemies = [battlePpmnEnemy $ ppmnByName You 99] })
    n1 <- stdCommentary (const (sentence '!' ["The DONALD sent out ", fpAvatarName fp]))
    local (`compEmbed` embedArr (>.= n1)) $ advancingEnemy
    scene <- gets sceneDefault
    summary <- gets sceneSummary
    n2 <- local (const $ embedArr ((scene >.= summary >.= n1 >.= drawText (fpAvatarName fp) (Point2 8 0)) >.=)) $ do
        momentary $ stopMusic
        longHesitation nullOut
        momentary $ restartRepeatingMusic TitleTheme
        n2 <- stdLecture (("WOKE: " `T.append`) . prose HadYouFiguredItOut)
        arrowWait n2
        return n2
    enemy <- gets sceneSecond
    enemySummary <- gets sceneSecondSummary
    let avatar = drawSprite BattleAvatarBack (Point2 8 40)
    local (const $ embedArr ((enemy >.= enemySummary >.= drawText (fpAvatarName fp) (Point2 8 0)) >.=)) $ do
        local (`compEmbed` embedArr (n2 >.=)) $ do
            retreating
            steppingForwardAvatar
        local (`compEmbed` embedArr (avatar >.=)) $ do
            n3 <- stdLecture (("WOKE: " `T.append`) . prose IveBeenWatchingYouAll)
            stdWait n3
            local (`compEmbed` embedArr (n3 >.=)) $ exitFade
    local (const (Embedding id)) $ do
        over 1.0 $ constant (clearScreen White)
        lift . swont $ constant (drawText "FIN" (Point2 100 100), NoEvent)

