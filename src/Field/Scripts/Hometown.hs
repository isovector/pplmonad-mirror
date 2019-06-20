{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings #-}

module Field.Scripts.Hometown where

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Map as M
import FRP.Yampa

import Activity
import Field.Activity
import Field.Anchor
import Field.CardinalDirection
import Field.Character
import Field.Locale
import Field.MapName
import Field.Parameters
import Field.Personae
import Field.Scripts.WeWoke
import Field.Terrain
import Lightarrow
import Message
import MusicName
import OfflineData
import Output
import ProseName
import StateClass

wanderer dt bounds speech c0 = second (arr (filterE isSpeech)) >>> act c0 activity
  where  activity = do  (m, t)  <- wandering dt bounds
                        c       <- get
                        maybe (return ()) id (reaction c t m)
                        activity
         reaction = reactSpeak (speaking speech)

isSpeech (CharacterSpeech _ _)  = True
isSpeech _                      = False

technologyGuy = wanderer 0.25 ((-1, 2), (3, 4)) speech c0
  where  c0      = (man (3, 3)) {  cIndex = 1,
                                   cSpeed = 2 }
         speech  = prose TechnologyIsIncredibleEverSince

kiddo = wanderer 0.25 ((12, -6), (16, -4)) speech c0
  where  c0      = (kid (15, -5)) { cIndex = 2 }
         speech  = prose MyParentsArePushingMe

denizens = [ technologyGuy, kiddo ]

hometown = hometownInitial

hometownNormal = stdLocale denizens TownTheme

hometownInitial t = hometownNormal t'
  where
    t' = adjustElem addSceneTrigger 5 (-7) $ adjustElem addSceneTrigger 6 (-7) t
    addSceneTrigger te@(TerrainElement { teTraverse = k }) = te { teTraverse = return (Event (FieldAction trigger)) }
    trigger = do
        momentary (restartRepeatingMusic MainTheme)
        narration <- stdLecture (("WOKE: " `T.append`) . prose DontGoIntoTheWifi)
        arrowWait narration
        aPos <- gets (cPosition . fpAvatar)
        modify (\fp -> fp { fpLocale = keep $ hometownWokeScene aPos denizens t })
        local (const $ Embedding id) $ anchor (sceneWith aTiming id observe)
    aTiming = (second (constant (Event $ CharacterTurn South)) >>>)

hometownWokeScene (xA, yA) characters t0 = proc (avatar, news) -> do
    (cs, cMsgs) <- parBZ (woke:characters) -< (cOccupy avatar t0, news)
    done        <- after 1.75 next   -< ()
    let draw  = charactersDraw cs xlate 
        xlate = characterXlate avatar
        sound = restartMusic news
    returnA -< ((draw, sound, t0), done)
  where
    woke      = act woke0 (activity >> return ())
    woke0     = msWoke (7, -2)
    activity  = approaching (xA, yA + 1) t0
    next = FieldAction $ do
        narration <- stdLecture (("WOKE: " `T.append`) . prose WildPeopleAreInThere)
        arrowWait narration
        modify (\fp -> fp { fpLocale = keep $ hometownWokeScene2 (xA, yA) characters t0 })
        local (const $ Embedding id) $ anchor (sceneWith aTiming id observe)
    aTiming = (second controller >>>)
    controller = runCont k (const (constant NoEvent))
    k = do
        cont . switchE $ (after 0.1 (CharacterMove South))
        cont . switchE $ (after 2.25 (CharacterMove East))
        cont . switchE $ (after eastInterval (CharacterMove North))
    eastInterval = (12 - xA) / (cSpeed woke0)
    restartMusic (Event Restart) = playRepeatingMusic TownTheme
    restartMusic _               = nullOut

hometownWokeScene2 (xA, yA) characters t0 = proc (avatar, news) -> do
    (cs, _) <- parBZ (woke:characters) -< (cOccupy avatar t0, news)
    done    <- after 4.2 next   -< ()
    let draw  = charactersDraw cs xlate 
        xlate = characterXlate avatar
        sound = restartMusic news
    returnA -< ((draw, sound, t0), done)
  where
    t0' = adjustElem addPortalToScene 12 0 t0
    addPortalToScene te@(TerrainElement { teTraverse = k }) = te { teTraverse = return (Event (FieldAction trigger)) }
    trigger = do
        modify (\fp -> fp {
            fpLocale = keep $ hometownNormal t0,
            fpWorld = M.insert WeWoke (keep $ weWokeEntranceScene (odGetTerrain (fpOfflineData fp) WeWoke) (fpRandomGenerator fp)) (fpWorld fp)
            })
        enterNewMap observe WeWoke (0, 0) North True
        return ()
    woke = act woke0 (activity >> return ())
    activity = do  turning South
                   sequence (replicate 8 walking)
                   turning East
                   sequence (replicate (12 - round xA) walking)
                   turning North
                   sequence (replicate 3 walking)
    woke0 = msWoke (round xA, -6)
    next = FieldAction $ do
        modify (\fp -> fp { fpLocale = keep $ stdLocale characters TownTheme t0' })
        local (const $ Embedding id) $ anchor (sceneWith aTiming passAvatarReport observe)
    aTiming = (second (constant (Event (CharacterMove North))) >>>)
    restartMusic (Event Restart) = playRepeatingMusic TownTheme
    restartMusic _               = nullOut
