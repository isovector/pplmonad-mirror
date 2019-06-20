{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings #-}

module Field.Scripts.Ditfy (ditfy) where

import Control.Monad.Cont
import Control.Monad.State
import qualified Data.Text as T
import FRP.Yampa
import System.Random

import Activity
import Field.Activity
import Field.Anchor
import Field.Battles
import Field.CardinalDirection
import Field.Character
import Field.Trainer
import Field.Locale
import Field.Parameters
import Field.Personae
import Field.Terrain
import Inventory.Items.Booch
import LabelName
import Lightarrow
import Message
import MusicName
import OfflineData
import Output
import Ppmn.Parameters
import Ppmn.Species
import ProseName
import SpriteName
import StateClass

receptionist index rg = act c0 activity
  where  activity      = do  (m, t)  <- rest
                             c       <- get
                             react0 c t m
         react0 c t m  = maybe activity id (reactSpeak speech0 c t m)
         speech0       = do  posting (FieldAction action0)
                             reactingForever react1
         action0       = do  welcome
                             introduction
                             offer
         react1 c t m  = maybe rest id (reactSpeak speech1 c t m)
         speech1       = do  posting (FieldAction action1)
                             rest
         action1       = do  welcome
                             offer
         welcome       = stdLecture (prose WelcomeToDitfy) >>= arrowWait
         introduction  = stdLecture (prose ThisIsANewOffice) >>= arrowWait
         offer         = do  n1  <- stdLecture (prose WeOfferOnSiteWellness)
                             ps  <- gets fpPpmn
                             if any ((<) <$> ppmnHitPoints <*> ppmnMaxHitPoints) ps
                                then do
                                    arrowWait n1
                                    n2  <- stdLecture (prose OneMoment)
                                    stdHesitation n2
                                    fadeTo White 0.5
                                    modify (\fp -> fp { fpPpmn = heal (fpPpmn fp) })
                                    fadeFrom White 0.5
                                    n3  <- stdLecture (prose ITookCareOfIt)
                                    stdWait n3
                                else
                                    stdWait n1
         heal          = map (\p -> p { ppmnHitPoints = ppmnMaxHitPoints p })
         rest          = looking South 1
         c0            = (leaner (1, -7)) {  cDirection        = South,
                                             cIndex            = index,
                                             cRandomGenerator  = rg }

veteran index rg = act c0 activity
  where  activity  = do  (m, t)  <- wandering 0.5 ((-5, -5), (-2, -1))
                         c       <- get
                         maybe (return ()) id (react c t m)
                         activity
         react     = reactSpeak speech
         speech    = speaking (prose PeopleAtAHighLevel)
         c0        = (man (-4, -2)) {  cDirection        = East,
                                       cSpeed            = 2,
                                       cIndex            = index,
                                       cRandomGenerator  = rg }

client index rg = act c0 (reactingForever react)
  where  react c t m  = maybe rest id (reactSpeak speech c t m)
         speech       = do  speaking (prose ThisCompanyIsOnHard)
                            rest
         rest         = looking West 0.8
         c0           = (leaner (7, -3)) {  cDirection        = West,
                                            cIndex            = index,
                                            cRandomGenerator  = rg }


coffeeBreak index rg = act c0 activity
  where  activity  = do  (m, t)  <- wandering 0.5 ((-2, -10), (3, -9))
                         c       <- get
                         maybe (return ()) id (react c t m)
                         activity
         react     = reactSpeak speech
         speech    = speaking (prose IUsedToThinkMy)
         c0        = (man (0, -10)) {  cDirection        = East,
                                       cSpeed            = 2,
                                       cIndex            = index,
                                       cRandomGenerator  = rg }

digger index rg = act c0 (reactingForever react)
  where  react c t m  = maybe rest id (reactSpeak speech c t m)
         speech       = do  speaking (prose DamnIPickedTheWrong)
                            rest
         rest         = looking East 1
         c0           = (man (-6, -18)) {  cDirection        = East,
                                           cIndex            = index,
                                           cRandomGenerator  = rg }

interviewee index rg = act c0 (reactingForever react)
  where  react c t m  = maybe rest id (reactSpeak speech c t m)
         speech       = do  speaking (prose AmIInTheWrong)
                            rest
         rest         = do  x <- glancing 0.8
                            modify notWest
                            return x
         c0           = (man (-2, -28)) {  cDirection        = East,
                                           cIndex            = index,
                                           cRandomGenerator  = rg }
         notWest c
            | cDirection c == West  = c { cDirection = East }
            | otherwise             = c

data Trainer = Trainer {  tChallenge  :: FieldParameters -> T.Text,
                          tConcede    :: ProseName,
                          tPpmn       :: [Ppmn],
                          tName       :: LabelName,
                          tReconcile  :: FieldParameters -> T.Text,
                          tSight      :: Double,
                          tSprite     :: SpriteName }

trainerActivity :: Trainer -> CharacterActivity (Message, Terrain)
trainerActivity t = do  antagonizing (tSight t) provoked challenging
                        defeated (tReconcile t)
  where  provoked x ter  = do  posting (FieldAction provocation)
                               approaching x ter
                               waiting 0.1 Done
                               return ()
         provocation     = do  momentary (restartRepeatingMusic MainTheme)
                               anchor observe
         challenging     = posting (FieldAction challenge)
         challenge       = do  n <- stdLecture (tChallenge t)
                               stdWait n
                               trainerBattle (tName t) (tSprite t) (tConcede t) (tPpmn t)
                               anchor explore

headcount index rg = act c0 (trainerActivity t >> return ())
  where  t   = Trainer {  tChallenge  = prose YourHeadcountIsMeaninglessIf,
                          tConcede    = WhatTheFuckPeople,
                          tPpmn       = [  ppmnByName Blamotage 3,
                                           ppmnByName Incub 5 ],
                          tName       = LabelName.Leaner,
                          tReconcile  = prose ApparentlyThisIsJustA,
                          tSight      = 5,
                          tSprite     = SpriteName.Leaner }
         c0  = (leaner (4, -17)) { cDirection        = East,
                                   cIndex            = index,
                                   cRandomGenerator  = rg }

mazedude index rg = second commander >>> act c0 activity
  where  activity       = trainerActivity t >> return ()
         t              = Trainer {  tChallenge  = prose ThisIsAWildPlace,
                                     tConcede    = WellPoop,
                                     tPpmn       = [  ppmnByName Unner 2,
                                                      ppmnByName Ignoloof 3,
                                                      ppmnByName Unner 4 ],
                                     tName       = LabelName.Grublord,
                                     tReconcile  = prose IGuessYouNeedTo,
                                     tSight      = 4,
                                     tSprite     = SpriteName.Grublord }
         c0             = (man (-6, -35)) { cDirection        = South,
                                            cIndex            = index,
                                            cRandomGenerator  = rg }
         commander      = identity &&& turner rs0 >>> arr (uncurry lMerge)
         turner (r:rs)  = after 1 (CharacterTurn r) `switchE` const (turner rs)
         rs0            = South : East : rs0

champion index rg = act c0 (trainerActivity t >> return ())
  where  t   = Trainer {  tChallenge  = prose TheTopPeopleCatchersDont,
                          tConcede    = WowMaybeIllTakeA,
                          tPpmn       = [ppmnByName Slidek 10],
                          tName       = LabelName.Leaner,
                          tReconcile  = prose IfIEmulateTheGreats,
                          tSight      = 5,
                          tSprite     = SpriteName.Leaner }
         c0  = (leaner (7, -39)) {  cDirection        = South,
                                    cIndex            = index,
                                    cRandomGenerator  = rg }

matchup index rg = act c0 (trainerActivity t >> return ())
  where  t   = Trainer {  tChallenge  = prose YourTeamMustBeGetting,
                          tConcede    = BrutalManIWonderWhy,
                          tPpmn       = [  ppmnByName Incub 3,
                                           ppmnByName Incub 4,
                                           ppmnByName Slidek 5 ],
                          tName       = LabelName.Grublord,
                          tReconcile  = prose MyPeopleWereTooSoft,
                          tSight      = 5,
                          tSprite     = SpriteName.Grublord }
         c0  = (man (4, -34)) {  cDirection        = West,
                                 cIndex            = index,
                                 cRandomGenerator  = rg }

acts = [  receptionist,
          client,
          veteran,
          digger,
          interviewee,
          coffeeBreak,
          headcount,
          mazedude,
          champion,
          matchup ]

uniqueCharacters rg0 = (rgN, map apply (zip acts (zip indices rgs)))
  where  apply             = uncurry (uncurry . ($))
         indices           = [1 .. length acts]
         nextRg _ (r, rs)  = let (r1, r2) = split r in (r2, r1:rs)
         (rgN, rgs)        = foldr nextRg (rg0, []) acts

itemHolders = map (uncurry ($)) (items `zip` [1 .. length items])

items = [ holdItem booch (6, -18) nullOut False ]

talkOverDesk = adjustElem talk 1 (-6)
  where
    index = 1
    inspect North = return (Event $ CharacterSpeech index North)
    inspect _     = return NoEvent
    talk te = te { teInspect = inspect }

encounterer :: RandomGen r =>  r ->
                               SF (Character, Event Message) (Event Message)
encounterer rg = runCont (runStateT (loop 0) rg) final
  where  loop s          = do  a   <- listening
                               s'  <- posting a s
                               loop s'
         listening       = lift (swont listen)
         posting a s
            | s < 10     = return (s + 1)
            | otherwise  = do  let  x   = cPosition a
                               m  <- encounter (min ((s - 10) / 100) (1/4)) (risks x)
                               let  sf  = constant m &&& (now ())
                               lift (dSwont sf)
                               return (event (s + 1) (const 0) m)
         listen          = proc (a, m) -> do
                             m' <- notYet -< filterE isTraverse m
                             returnA -< (m', m' `tag` a)
         final           = error "encounterer terminated"

risks (x, y)
    | y > (-10)  = map risk [Incub]
    | y > (-20)  = map risk [Incub, Unner]
    | otherwise  = map risk [Incub, Unner, Slidek, Ignoloof, Blamotage]
  where  risk name = (ppmnByName name (round (sqrt ((-1) * y))), 1)

isTraverse  AvatarTraverse  = True
isTraverse  _               = False

ditfy t0 rg0 = proc (avatar, news) -> do
    ((d, s, t), report)          <- main                -< (avatar, news)
    (outsI, updatesI, reportsI)  <- parBZ3 itemHolders  -< news
    e                            <- encounterer rg1     -< (avatar, news)
    let  t'  = talkOverDesk . foldl (.) id updatesI $ t
         r'  = foldl lMerge NoEvent reportsI `lMerge` report `lMerge` e
         d'  = foldl (>.=) nullOut outsI >.= d
    returnA -< ((d', s, t'), r')
  where
    main        = stdLocale cs DitfyTheme t0
    (rg1, rg2)  = split rg0
    (rg3, cs)   = uniqueCharacters rg2
