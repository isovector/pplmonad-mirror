{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings #-}

module Inventory.Items.PPhone where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Text as T
import FRP.Yampa
import Data.Vector2
import Data.Point2
import Data.AffineSpace
import System.Random

import Activity
import Battle.Activity
import Battle.Output
import Battle.Parameters
import Battle.Vignette
import Inventory.Parameters
import LabelName
import Lightarrow
import OfflineData
import Output
import Ppmn.Output
import Ppmn.Parameters
import ProseName
import SoundName
import SpriteName
import StateClass

pPhone = ItemParameters {
    itemBattleCont = do
        modify (\iu -> iu { iuParams = itemSubtract (iuParams iu) })
        return battleEffect,
    itemFieldCont = fieldEffect,
    itemName = LabelName.PPhone,
    itemStock = 1
}

fieldEffect = do
    name <- gets iuAvatarName
    let possessive = (name `T.append`) . prose S
        s1 iu = sentence '.' [name, stared iu, phone iu]
        s2 iu = sentence '.' [possessive iu, reflect iu]
    n1 <- stdLecture (\iu -> s1 iu `T.append` " " `T.append` s2 iu)
    stdWait n1
  where
    stared = prose StaredDeepIntoTheDark
    phone = label LabelName.PPhone
    reflect = prose ReflectionStaredBackOminously

battleEffect = do
    summary <- gets sceneSummary
    f       <- gets sceneFirst
    let embed1 = embedArr (summary >.=)
    n1      <- local (compEmbed embed1) announceToss
    let embed2 = embedArr ((f >.= summary >.= n1) >.=)
        embed3 = embedArr ((f >.= summary >.= landedPhone) >.=)
    local (const embed2) attemptCapture
    caught <- testEnemy
    if caught
        then local (const embed3) catchPerson
        else local (const embed2) personBreaksOut

totalFailure = do
    summary <- gets sceneSummary
    let embed1 = embedArr (summary >.=)
    local (compEmbed embed1) $ do
        bouncePast
        p <- gets bpEnemy
        nameA <- gets bpAvatarName
        n1 <- stdCommentary (\s -> sentence '!' [prose Enemy s, label (ppmnName p) s, "loves its job"])
        stdHesitation n1
        n2 <- stdCommentary (\s -> sentence '.' [nameA `T.append` prose S s, label LabelName.PPhone s, "did not appeal to", prose Enemy s, label (ppmnName p) s])
        stdHesitation n2

announceToss = do
    name <- gets bpAvatarName
    stdCommentary (\s -> sentence '!' [name, prose ThrewAPphone s])

attemptCapture = do
    p <- gets bpEnemy
    local (`compEmbed` embedArr (drawPpmn p >.=)) toss
    local (`compEmbed` embedArr (landedPhone >.=)) $ do
        capture p (vector2 0 0)
        over 3 $ constant nullOut

toss = do
    momentary (playSound Throw)
    throwPhone (Point2 (-16) 80) (Point2 124 48) 60 (5/8)

testEnemy = do
    p <- gets bpEnemy
    x <- state (randomR (0.0, 1.0))
    return (x > ppmnHitPoints p / ppmnMaxHitPoints p)

catchPerson = do
    person <- gets bpEnemy
    ppmn <- gets bpPpmn
    counters <- gets bpCounters
    modify (\bp -> bp {
        bpCounters = catch counters,
        bpPpmn = ppmn ++ [person]
        })
    nameP <- gets (label (ppmnName person))
    nameA <- gets bpAvatarName
    n <- stdLecture (\s -> sentence '!' [nameA, "caught", nameP])
    local (`compEmbed` embedArr (n >.=)) $ do
        momentary (playSoloSound Jingle)
        arrowWait n
        exitFade
    exit <- gets bpExit
    exit ()

personBreaksOut = do
    p <- gets bpEnemy
    name <- gets (label (ppmnName p))
    local (`compEmbed` embedArr (landedPhone >.=)) $ release p (vector2 0 0)
    local (`compEmbed` embedArr (drawPpmn p >.=)) $ stdCommentary (\s -> sentence '!' [prose Enemy s, name, "protested"]) >>= stdHesitation

bouncePast = do
    n1 <- announceToss
    local (compEmbed (embedArr (n1 >.=))) $ do
        toss
        momentary (playSound Poke)
        throwPhone (Point2 124 48) (Point2 159 40) 15 (5/32)
        momentary (playSound Poke)
        throwPhone (Point2 159 38) (Point2 190 31) 5 (1/8)
        momentary (playSound Poke)

isPPhone item@(ItemParameters { itemName = LabelName.PPhone }) = True
isPPhone _ = False
