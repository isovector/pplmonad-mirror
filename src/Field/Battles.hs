{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, OverloadedStrings #-}

module Field.Battles where

import Control.Monad.Cont
import Control.Monad.RWS
import Data.Bool
import qualified Data.Map as M
import FRP.Yampa

import Activity
import Battle.Activity
import Battle.Anchor
import Battle.Vignette
import Battle.Parameters
import Ending
import qualified Field.Anchor
import Field.Character
import Field.MapName
import Field.Parameters
import Inventory.Parameters
import Inventory.Items.Booch
import Inventory.Items.PPhone
import LabelName
import Lightarrow
import MusicName
import OfflineData
import Output
import Ppmn.Parameters
import Ppmn.Species
import ProseName
import SpriteName
import StateClass

wildBattle enemy = do
    momentary $ playMusicWithIntro WildBattleIntro 12.38 WildBattleTheme
    wildBattleWarning
    fp <- get
    let bp = (makeBattleParameters fp) {
        bpEnemies = [battlePpmnEnemy enemy],
        bpRun = flee,
        bpWin = do
            momentary (playMusicWithIntro VictoryIntro 1.1 VictoryTheme)
            n1 <- stdLecture (prose AndDontItFeelGood)
            stdWait n1
            n2 <- pickUpItem booch 1
            local (`compEmbed` embedArr (n2 >.=)) $ exitFade
    }
    (bp', _) <- lift $ execRWST k (Embedding id) bp
    let ppmn' = map resetStages (bpPpmn bp')
    modify (\fp -> fp {
        fpPpmn = ppmn',
        fpItems = bpItems bp',
        fpCounters = bpCounters bp',
        fpRandomGenerator = bpRandomGenerator bp' 
        })
    fadeFrom White 0.5
  where
    k = wildBattleIntro >> callCC anchor

trainerBattle trainerLabel trainerSprite trainerProse enemies = do
    momentary (restartRepeatingMusic BattleTheme)
    wildBattleWarning
    fp <- get
    let bp0 = makeBattleParameters fp
        bp = bp0 {
            bpEnemies = map battlePpmnEnemy enemies,
            bpEnemyTrainer = Just (EnemyTrainer trainerLabel trainerProse trainerSprite),
            bpItems = M.map disablePPhone (bpItems bp0),
            bpRun = stdCommentary (prose YouAreNowCrossingThe) >>= stdHesitation >> gets bpExit >>= anchor,
            bpWin = do
                momentary (playMusicWithIntro VictoryIntro 1.1 VictoryTheme)
                stdLecture (\s -> sentence '!' [fpAvatarName fp, prose Defeated s, label trainerLabel s]) >>= arrowWait
                embed <- trainerBattleDenouement
                local (const embed) $ exitFade
        }
    (bp', _) <- lift $ execRWST k (Embedding id) bp
    let ppmn' = map resetStages (bpPpmn bp')
    modify (\fp -> fp {
        fpPpmn = ppmn',
        fpItems = M.map enablePPhone (bpItems bp'),
        fpCounters = bpCounters bp',
        fpRandomGenerator = bpRandomGenerator bp'
        })
    fadeFrom White 0.5
  where
    k = trainerBattleIntro >> callCC anchor

finalBattle = do
    momentary (restartRepeatingMusic BattleTheme)
    wildBattleWarning
    fp <- get
    let bp0 = makeBattleParameters fp
        bp = bp0 {
            bpEnemies = map battlePpmnEnemy enemies,
            bpEnemyTrainer = Just (EnemyTrainer TheDonald TheGreatestWitchHuntIn Donald),
            bpItems = M.map disablePPhone (bpItems bp0),
            bpRun = stdCommentary (prose YouAreNowCrossingThe) >>= stdHesitation >> gets bpExit >>= anchor,
            bpWin = do
                momentary (playMusicWithIntro VictoryIntro 1.1 VictoryTheme)
                stdLecture (\s -> sentence '!' [fpAvatarName fp, prose Humiliated s, label TheDonald s]) >>= arrowWait
                trainerBattleDenouement
                gameEnd fp
        }
    (bp', _) <- lift $ execRWST k (Embedding id) bp
    return ()
  where
    k = trainerBattleIntro >> callCC anchor
    enemies = [ppmnByName Blamotage 7, ppmnByName Incub 10]

makeBattleParameters fp = BattleParameters {
    bpAvatarName = fpAvatarName fp,
    bpCounters = counters',
    bpEnemies = [],
    bpEnemyIndex = 0,
    bpEnemyTrainer = Nothing,
    bpExit = return,
    bpFleeAttempts = 0,
    bpFriendIndex = 0,
    bpItems = fpItems fp,
    bpLose = restart fp,
    bpOfflineData = fpOfflineData fp,
    bpPpmn = map battlePpmnFriend ppmn,
    bpRandomGenerator = fpRandomGenerator fp,
    bpRun = return (),
    bpWin = return ()
}
  where
    ppmn = fpPpmn fp
    counters = fpCounters fp
    plays = pcPlays counters
    plays' = M.insertWith (+) (ppmnName (head ppmn)) 1 plays
    counters' = counters { pcPlays = plays' }

disablePPhone = bool <$> id <*> disable <*> isPPhone
  where
    disable item = item { itemBattleCont = do
        modify (\iu -> iu { iuParams = itemSubtract (iuParams iu) })
        return totalFailure }

enablePPhone = bool <$> id <*> enable <*> isPPhone
  where
    enable item = item { itemBattleCont = itemBattleCont pPhone }

restart fp = do
    narration <- stdLecture (prose YouFinallyLostYourMind)
    stdWait narration
    fadeTo Black 1.0 
    over 1.0 $ constant (clearScreen Black)
    let fp' = fp {
        fpLocale = (fpWorld fp) M.! FamilyHouse1F,
        fpMap = FamilyHouse1F,
        fpAvatar = (fpAvatar fp) { cPosition = (0, 0) }
    }
    lift $ execRWST (Field.Anchor.anchor Field.Anchor.explore) (Embedding id) fp'
    return ()

wildBattleWarning = do
    fadeTo (Translucent White) 0.05 >> fadeFrom (Translucent White) 0.05 
    fadeTo (Translucent Black) 0.05 >> fadeFrom (Translucent Black) 0.05 
    fadeTo (Translucent White) 0.05 >> fadeFrom (Translucent White) 0.05 
    fadeTo (Translucent Black) 0.05 >> fadeFrom (Translucent Black) 0.05 
    fadeTo (Translucent White) 0.05 >> fadeFrom (Translucent White) 0.05 
    fadeTo (Translucent Black) 0.05 >> fadeFrom (Translucent Black) 0.05 
    fadeTo (Translucent White) 0.05 >> fadeFrom (Translucent White) 0.05 
    fadeTo (Translucent Black) 0.05 >> fadeFrom (Translucent Black) 0.05 
    fadeTo (Translucent White) 0.05 >> fadeFrom (Translucent White) 0.05 
    fadeTo (Translucent Black) 0.05 >> fadeFrom (Translucent Black) 0.05 
    fadeTo White 0.5
