{-# LANGUAGE OverloadedStrings #-}

module Field.Scripts.BossRoom where

import qualified Data.Text as T

import Activity
import Field.Battles
import Field.CardinalDirection
import Field.Character
import Field.Locale
import Field.Personae
import Message
import MusicName
import ProseName
import StateClass

thedonald = act c0 (reactingForever react)
  where  react c t m  = maybe standing id (reactSpeak speech c t m)
         speech       = do  posting (FieldAction action)
                            standing
         c0           = (donald (-6, -5)) { cDirection = North }

action = do  n1 <- stdLecture (\s -> "DONALD: " `T.append` prose AmNotSurprisedToSee s)
             arrowWait n1
             n2 <- stdLecture (prose WillNotBeHereTo)
             arrowWait n2
             n3 <- stdLecture (const "...")
             arrowWait n3
             n4 <- stdLecture (prose WantToStopMeYou)
             arrowWait n4
             n5 <- stdLecture (prose TheFactIsWereNot)
             stdWait n5
             finalBattle

bossRoom = stdLocale [thedonald] MainTheme
