{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings #-}

module Field.Scripts.Courtyard (courtyard) where

import Control.Monad.Cont
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T

import Activity
import Field.Activity
import Field.CardinalDirection
import Field.Character
import Field.Locale (stdLocale)
import Field.Parameters
import Field.Personae
import Inventory.Items.Booch
import Inventory.Items.PPhone
import LabelName
import Message
import MusicName
import Output
import Ppmn.Menu
import Ppmn.Parameters
import ProseName
import StateClass

signalGuy = act c0 (offering >> spent)
  where  offering  = do  (m, t)  <- glancing 0.5
                         c       <- get
                         maybe offering id (reactSpeak offer c t m)
         spent     = do  (m, t)  <- glancing 0.5
                         c       <- get
                         maybe (return ()) id (reactSpeak advice c t m)
                         spent
         offer     = posting (FieldAction action)
         advice    = speaking (prose FullyHealthyPeopleWontLet)
         action    = do  n <- stdLecture (prose IAmJustGettingSwarmed)
                         arrowWait n
                         pickUpItem booch 1
         c0        = (leaner (-2, -8)) {  cDirection = East,
                                          cIndex = 1 }

cautiousGuy = act c0 loop
  where  loop    = do  greeting speech
                       looking West 1
                       loop
         speech  = prose IHateDealingWithWild
         c0      = (man (4, -7)) {  cDirection = West,
                                    cIndex = 2 }

newsboy = act c0 loop
  where  loop    = do  greeting speech
                       looking East 1
                       loop
         speech  = prose TheHeadHonchoAtDitfy 
         c0      = (kid (-3, -14)) {  cDirection = East,
                                      cIndex = 5 }

advisor = act c0 (offering >> spent)
  where  offering  = do  (m, t)  <- looking South 1
                         c       <- get
                         maybe offering id (reactSpeak offer c t m)
         spent     = do  greeting (prose DontTryToUseYour)
                         spent
         offer     = posting (FieldAction action)
         action    = do  n <- stdLecture (prose BeCarefulInThereYou)
                         arrowWait n
                         pickUpItem booch 1
         c0        = (kid (2, -23)) {  cDirection = South,
                                       cIndex = 3 }

phoneMonger = act c0 (proposing >> business)
  where  proposing  = do  (m, t)  <- glancing 1
                          c       <- get
                          maybe proposing id (reactSpeak propose c t m)
         business   = do  (m, t)  <- looking South 1
                          c       <- get
                          maybe business id (reactSpeak sell c t m)
         propose    = posting (FieldAction proposal)
         proposal   = do  n1  <- stdLecture (prose SighImTiredOfCatching)
                          arrowWait n1
                          pickUpItem pPhone 5
                          n2  <- stdLecture (prose ComeBackWhenYouveGot)
                          stdWait n2
         sell       = do  posting (FieldAction sale)
                          business
         c0         = (man (0, -20)) {  cDirection = West,
                                        cIndex = 4 }

sale = do  n1     <- stdLecture (prose HiAgainIllGiveYou)
           arrowWait n1
           ppmn   <- gets fpPpmn
           items  <- gets fpItems
           if length ppmn < 2
               then  do  n2 <- stdLecture (prose AhYouOnlyHaveOne)
                         arrowWait n2
                         unless (M.member PPhone items) $ do
                             n3 <- stdLecture (prose JebusYoureOutOfPphones)
                             arrowWait n3
                             pickUpItem pPhone 5
               else  callCC (\cc -> do
                         let options k = [  (Sell, sell k >> cc ()),
                                            (Cancel, return ()) ]
                         selectPpmn ChooseAPerson fpPpmn (personMenu options) 0 cc)
           n4     <- stdLecture (prose ComeBackWhenYouveGot)
           stdWait n4

sell k = do  ps <- gets fpPpmn
             modify (\fp -> fp { fpPpmn = take k ps ++ drop (k + 1) ps })
             n1 <- stdLecture (\s -> sentence '!' [  prose AllRight s,
                                                     label (ppmnName (ps !! k)) s `T.snoc` ',',
                                                     prose CommaMyFavorite s ])
             arrowWait n1
             n2 <- stdLecture (prose HereYouGo)
             arrowWait n2
             pickUpItem pPhone 5

beings = [ signalGuy, phoneMonger, cautiousGuy, advisor, newsboy ]
courtyard t = stdLocale beings FieldTheme t
