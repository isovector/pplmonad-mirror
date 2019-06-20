{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings #-}

module Inventory.Items.Booch where

import Control.Arrow
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Text as T

import Activity
import Battle.Activity
import Battle.Anchor
import Battle.Parameters
import Inventory.Parameters
import LabelName
import Lightarrow
import OfflineData
import Output
import Ppmn.Output
import Ppmn.Parameters
import SoundName
import StateClass

booch = ItemParameters {
    itemBattleCont = battleEffect,
    itemFieldCont = fieldEffect,
    itemName = Booch,
    itemStock = 1
}

fieldEffect = callCC (\cc -> do
    takeStock cc
    momentary (playSound Drink)
    initial  <- gets (ppmnHitPoints . actionObject)
    maximum  <- gets (ppmnMaxHitPoints . actionObject)
    let effect = stdHPEffect (maximum / 2)
    final    <- withEmbedding (`compEmbed` Embedding embed) effect
    review (final - initial))
  where
    embed changer = proc controls -> do
        (_, done) <- changer -< ()
        returnA -< (nullOut, done)

battleEffect = callCC (\cc -> do
    summary  <- gets (statusSummary . actionObject)
    let e1     = embedArr (summary >.=)
        escape = cc . (>> (gets bpExit >>= anchor)) . return
    local (`compEmbed` e1) $ takeStock escape
    initial  <- gets (ppmnHitPoints . actionObject)
    maximum  <- gets (ppmnMaxHitPoints . actionObject)
    nameA    <- gets iuAvatarName
    nameI    <- gets (itemName . iuParams)
    n1       <- local (`compEmbed` e1) $ stdCommentary (\s -> sentence '!' [nameA, "used a", label nameI s])
    let e2 = embedArr ((summary >.= n1) >.=)
    local (`compEmbed` e2) $ momentary (playSound Drink)
    e3       <- subjectSummaryEmbedding
    let effect = stdHPEffect (maximum / 2)
    final    <- withEmbedding (`compEmbed` compEmbed e2 e3) effect
    summary2 <- gets (statusSummary . actionObject)
    let e4 = embedArr (summary2 >.=)
    local (`compEmbed` e4) $ review (final - initial)
    return (return ()))

-- this should be in a State context of FieldParameters or BattleParameters, not itemUse, for the sake of embedding the narrations differently than the effect
takeStock escape = do 
    obj <- gets actionObject
    name <- gets actionObjectName
    let initial = ppmnHitPoints obj
        max     = ppmnMaxHitPoints obj
    if initial == max
        then do
            stdLecture (const $ sentence '!' [name, "is already in perfect health"]) >>= stdHesitation
            escape ()
        else do
            modify (\iu -> iu { iuParams = itemSubtract (iuParams iu) })

review delta = do
    nameObj <- gets actionObjectName
    n1 <- stdCommentary (const $ sentence '!' [nameObj, "recovered", T.pack (show (round delta)), "HP"])
    stdHesitation n1
