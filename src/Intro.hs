{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Intro (intro) where

import Control.Monad.RWS
import Control.Monad.Reader
import qualified Data.Text as T
import FRP.Yampa
import Data.Vector2
import Data.Point2
import Data.AffineSpace

import Activity
import Controls
import ControlsMaps
import Field
import LabelName
import Lightarrow
import Menu
import MusicName
import OfflineData
import Output
import ProseName
import SoundName
import SpriteName
import StateClass

intro :: OfflineData -> Swont Controls OfflineIO T.Text
intro od = fmap (\(x, _, ()) -> x) $ runRWST k (Embedding id) od
  where
    k = do
        titleDrop
        titleShake
        titleScreen
        instructionScreen
        local (const (embedArr (clearScreen Black >.=))) $ wakeup
        local (const (embedArr (drawSprite Woke (Point2 54 34) >.=))) $ welcome
        explanation
        narration <- local (const (embedArr (drawSprite AvatarPortrait (Point2 54 34) >.=))) $ lookAtYou
        over 0.25 $ arr (>.= narration) <<< slide (Point2 54 34) (Point2 80 34) 0.125 AvatarPortrait
        name <- local (const (embedArr ((narration >.= drawSprite AvatarPortrait (Point2 80 34)) >.=))) $ pickName
        over 0.25 $ arr (>.= narration) <<< slide (Point2 80 34) (Point2 54 34) 0.125 AvatarPortrait
        narration <- local (const (embedArr (drawSprite AvatarPortrait (Point2 54 34) >.=))) $ letsGo (label name od)
        local (const (Embedding id)) $ enter narration
        lift $ return (label name od)

titleDrop = do  momentary $ playSound Drop
                over 1.0 $ time >>> arr drop >>> arr draw
    where  drop t  = Point2 20 (98 * t * t - 66)
           draw    = drawSprite Logo --drawText "PEOPLEMON"

titleShake = do  momentary $ draw (shake 0) >.= play
                 over 0.25 $ time >>> arr shake >>> arr draw
    where  shake t  = Point2 20 (32 + 3 * 1 / (1 + 8 * t) * sin (32 * pi * t))
           draw     = drawSprite Logo --drawText "PEOPLEMON"
           play     = playSound Crash >.= restartRepeatingMusic TitleTheme

titleScreen = do  stdWait initialDraw
                  momentary $ initialDraw >.= playSound SoundName.Accept
                  over 0.5 $ time >>> arr raise >>> arr drawTitle
  where  raise t      = Point2 20 (32 - 64 * t)
         initialDraw  = drawTitle (Point2 20 32) >.= drawCreator
         drawTitle    = drawSprite Logo --drawText "PEOPLEMON"
         drawCreator  = drawText "by" (Point2 72 80)               >.=
                        drawText "Alex Stuart" (Point2 40 96)      >.=
                        drawText "PRESS Z TO BEGIN" (Point2 16 124)

instructionScreen = do  stdWait out
                        momentary $ out >.= playSound SoundName.Accept
                        local (const (embedArr (out >.=))) $ fadeTo Black 1.0
                        momentary $ clearScreen Black >.= fadeOutMusic
  where out =  drawSprite Logo (Point2 20 0)              >.=
               --drawText "PEOPLEMON" (Point2 48 16)        >.=
               drawText "- control keys -" (Point2 16 48)  >.=
               drawText "Return:  Open menu" (Point2 8 64) >.=
               drawText "Arrows:  Move" (Point2 8 76)      >.=
               drawText "Z:       Confirm" (Point2 8 88)   >.=
               drawText "X:       Cancel" (Point2 8 100)   >.=
               drawText "PRESS Z (AGAIN!)" (Point2 16 124)

wakeup = do
    n1 <- stdLecture (prose IWokeUpAndIts)
    arrowWait n1
    n2 <- stdLecture (prose WhereAmI)
    arrowWait n2
    n3 <- stdLecture (prose AmIDreaming)
    arrowWait n3

welcome = do
    fadeFrom Black 0.5
    momentary $ restartRepeatingMusic CenterTheme
    n1 <- stdLecture (prose WelcomeToTheWorldOf)
    arrowWait n1
    n2 <- stdLecture (prose MyNameIsWokeI)
    arrowWait n2
    fadeTo White 0.5

explanation = do
    n1 <- stdLecture (prose InThisWorldThereAre)
    arrowWait n1
    n2 <- stdLecture (prose AndThenThereArePeople)
    over 0.25 $ arr (>.= n2) <<< slide (Point2 160 34) (Point2 54 34) 0.25 IgnoloofFront
    local (const (embedArr (drawSprite IgnoloofFront (Point2 54 34) >.=))) $ do
        momentary $ n2 >.= playSoloSound SoundName.Ignoloof
        arrowWait n2
        n3 <- stdLecture (prose PeopleUsePeopleForFights)
        arrowWait n3
        fadeTo White 0.5

lookAtYou = do
    fadeFrom White 0.5
    n1 <- stdLecture (prose LookAtYouYouLook)
    arrowWait n1
    n2 <- stdLecture (prose DoYouRememberYourName)
    arrowWait n2
    return n2

pickName = do
    (next, k) <- choiceOverlay (menuControl >>> nameMenu (const pickName))
    next

nameMenu cancel = backgroundMenu 80 96 (Point2 0 0) menu
  where
    menu = columnMenu (map (\n -> (n, return n)) names) (cancel (), 0) 0

names = [ Jeff, Larry, Mark, Sergey, Steve ]

letsGo name = do
    n1 <- stdLecture (\s -> sentence '!' ["Good,", name] `T.append` " " `T.append` prose IKnowIveHeardThat s)
    arrowWait n1
    n2 <- stdLecture (prose ButPinchYourselfBitchYoure)
    return n2

enter n2 = do
    over 1.0 $ arr (>.= n2) <<< slideStretch 1 0.45 (Point2 54 34) (Point2 60 52) 1.0 AvatarPortrait
    over 0.5 $ constant (drawSprite AvatarFront ((origin .+^ screenCenterOffset) .+^ (vector2 0 (-8))) >.= n2)
    over 0.5 $ constant (clearScreen White)
