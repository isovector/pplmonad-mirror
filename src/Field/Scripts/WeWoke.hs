{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings #-}

module Field.Scripts.WeWoke (weWoke, weWokeInitial, weWokeEntranceScene) where

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import FRP.Yampa hiding (left, right)
import Data.Vector2
import Data.Point2
import Data.AffineSpace

import Activity
import ControlsMaps
import Field.Anchor
import Field.CardinalDirection
import Field.Character
import Field.Locale
import Field.MapName
import Field.Output
import Field.Parameters
import Field.Personae
import Field.Personae
import Field.Scripts.FamilyHouse1F
import Field.Terrain
import LabelName
import Lightarrow
import Menu
import Message
import MusicName
import OfflineData
import Output
import Ppmn.Menu
import Ppmn.Parameters
import Ppmn.Species
import ProseName
import SoundName (SoundName(Jingle))
import SpriteName
import StateClass

weWokeInitial t0 rg = proc (avatar, news) -> do
    ((d, s, t), lMsg) <- stdLocale npcs CenterTheme t0 -< (avatar, news)
    let xlate = characterXlate avatar
        d' = d >.= drawAllPhones xlate
    returnA -< ((d', s, t), lMsg)
  where (_, npcs) = uniqueCharacters rg

weWokeEntranceScene t0 rg = localeStarters starters selectFirst woke MainTheme t0 rg >>> (second sceneTrigger)
  where
    woke          = act c0 (let loop = walking >> loop in loop)
    c0            = pivot North (msWoke (1, (-2)))
    sceneTrigger  = after 1 (FieldAction trigger)
    trigger = do
        modify (\fp -> fp { fpLocale = keep (weWoke t0 rg) })
        local (const $ Embedding id) $ anchor (sceneWith id lockLocale explore)

lockLocale (KeepSF kept) = KeepSF $ kept >>> arr (\(((d, s, t), lMsg), kept') -> (((d, s, locked t), lMsg), kept'))

locked = adjustElem lock 0 1 . adjustElem lock 1 1
  where
    lock te = te { teTraverse = return $ Event (FieldAction call) }
    call = do
        narration <- stdLecture (const "WOKE: Hey, don't leave yet! Come up here and let's keep talking!")
        stdWait narration
        local (const $ Embedding id) $ anchor (sceneWith aTiming lTiming observe)
    aTiming = (second (constant (Event $ CharacterMove North)) >>>)
    lTiming (KeepSF sf) = KeepSF $ sf >>> first (second (after 0.25 (FieldAction trigger)))
    trigger = local (const $ Embedding id) $ anchor (sceneWith id lockLocale explore)

weWoke t0 rg = localeStarters starters selectFirst woke1 MainTheme t0 rg
  where
    woke1         = act woke0 (reactingForever react1)
    react1 c t m  = maybe standing id (reaction c t m)
    reaction c t  = reactSpeak (welcoming >> return (Done, t)) c t
    welcoming     = posting (FieldAction welcome)
    welcome      = do
        out <- stdLecture (T.append "WOKE: " . prose LookOnTheOtherSide)
        stdWait out
        modify (\fp -> fp { fpLocale = keep $ localeStarters starters selectStarter woke2 MainTheme t0 rg })
    woke2         = act woke0 loop2
    loop2         = greeting insist >> looking West 0.5 >> loop2
    insist = T.append "WOKE: ". prose WhatDontLookAtMe

restartMusic music (Event Restart) = playRepeatingMusic music
restartMusic _     _               = nullOut

(x, y) = (2, -10)
woke0 = (msWoke (x, y)) { cDirection = West }
names = [Ignoloof, Slidek, Blamotage]
demeanors = ["apathetic", "witty", "spiteful"]
positions = [(x - 2, y - 1), (x - 2, y), (x - 2, y + 1)]
starters = zip3 names demeanors positions
ppmn = [let p = ppmnByName n 10 in p { ppmnMoves = take 2 (ppmnMoves p) } | n <- names]

characters = [greeter, peoplethon1, peoplethon2, strategist, elated, lounger, facingNorth, confidant]

uniqueCharacters rg0 = (rgN, map apply (zip characters (zip indices rgs)))
  where  apply             = uncurry (uncurry . ($))
         indices           = [1 .. length characters]
         nextRg _ (r, rs)  = let (r1, r2) = split r in (r2, r1:rs)
         (rgN, rgs)        = foldr nextRg (rg0, []) characters

freak persona x dir speech index rg = act c0 loop
  where  loop    = do  (m, t)  <- looking dir 0.5
                       c       <- get
                       maybe (return ()) id (reactSpeak (speaking speech) c t m)
                       loop
         c0      = (persona x) {  cDirection        = dir,
                                  cIndex            = index,
                                  cRandomGenerator  = rg }


peoplethon1 index rg = act c0 loop
  where  loop    = do  dt      <- state (randomR (1, 2))
                       indexD  <- state (randomR (0, 3))
                       let dir = case (toEnum indexD) of
                                    South -> North
                                    West  -> East
                                    d     -> d
                       (m, t)  <- looking dir dt
                       c       <- get
                       maybe (return ()) id $ reactSpeak speech c t m
                       loop
         speech  = speaking (prose WerePlanningAPeoplethon)
         c0      = (leaner (5, -6)) {  cDirection        = North,
                                       cIndex            = index,
                                       cRandomGenerator  = rg }

peoplethon2 index rg = act c0 loop
  where  loop    = do  dt      <- state (randomR (1, 2))
                       indexD  <- state (randomR (0, 3))
                       let dir = case (toEnum indexD) of
                                    South -> North
                                    East  -> West
                                    d     -> d
                       (m, t)  <- looking dir dt
                       c       <- get
                       maybe (return ()) id $ reactSpeak speech c t m
                       loop
         speech  = speaking (prose WereGonnaSpendAllDay)
         c0      = (man (6, -6)) {  cDirection        = North,
                                    cIndex            = index,
                                    cRandomGenerator  = rg }

greeter = freak kid (-3, -4) South (prose EveryoneHereIsWorkingOn)

strategist = freak leaner (-7, -5) West (prose CatchingPeopleIsBecomingMore)

elated = freak leaner (3, -8) South (prose MyPersonWonItsFirst)

lounger = freak man (-4, -11) West (prose WhewIveBeenWorkingAll)

facingNorth = freak man (-2, -2) North (prose CanIReallyHackIt)

confidant = freak kid (8, -3) East (prose YouLookLikeASmart)

phoneOrientations = [Original, Original, Original, TurnL, FlipBoth, FlipBoth, TurnR]
phonePositions = [(-28, -44), (84, -108), (100, -108), (-124, -80), (52, -112), (-44, -48), (148, -48)]

drawPhone xlate orient (x, y) = withXlation xlate draw position
  where
    position = Point2 (fromIntegral x) (fromIntegral y)
    draw = drawSpriteOriented orient SpriteName.PPhone

drawAllPhones xlate = foldl ((. uncurry (drawPhone xlate)) . (>.=)) nullOut (zip phoneOrientations phonePositions)

localeStarters starters select woke theme t0 rg = proc (avatar, news) -> do
    rec
        t        <- arr (uncurry occupation)     -< (c:cs, t0)
        t'       <- arr (uncurry startersOccupy) -< (c, t)
        t''      <- iPre t0                      -< t'
        (c, msg) <- woke                         -< (cOccupy avatar t'', news)
        (cs, msgs) <- parBZ npcs      -< (cOccupy avatar t, news)
    let drawPhones = drawAllPhones xlate
        draw   = charactersDraw (c:cs) xlate
        sDraw  = startersDraw starters xlate
        xlate  = characterXlate avatar
        report = msg `lMerge` (mergeEvents msgs) `lMerge` news
        music  = restartMusic theme news
    returnA -< ((draw >.= drawPhones >.= sDraw, music, t'), report)
  where
    (names, demeanors, positions) = unzip3 starters
    startersOccupy c = foldl' (.) id [addStarter i p | (i, p) <- zip (inspections c) positions]
    inspections c = [select p d r | (p, d, r) <- zip3 ppmn demeanors (receipts c)]
    receipts c = [receive1 p os t0 c rg' | (p, os) <- zip ppmn others]
    others = [delete s starters | s <- starters]
    (rg', npcs) = uniqueCharacters rg

startersDraw starters xlate = foldl' (>.=) nullOut (map drawStarter points)
  where
    drawPhone = drawSpriteOriented TurnR SpriteName.PPhone
    drawStarter p = withXlation xlate drawPhone p
    points = [Point2 (fromIntegral x * 16 + 6) (fromIntegral y * 16 + 2) | (_, _, (x, y)) <- starters]

addStarter inspect (x, y) t = setElem (update elem) x y t
  where
    update e = e { teCollides = True, teInspect = const $ return $ Event $ FieldAction inspect }
    elem = maybe teDefault id (getElem x y t)

selectStarter ppmn demeanor receive = do
    personDetails ppmn
    narration <- stdLecture (message . label (ppmnName ppmn))
    confirmation <- popUpMenu confirmMenu narration
    if confirmation
        then receive
        else return ()
  where
    message label = sentence '?' ["So! You want the ", demeanor, "PERSON, ", label]

receive1 p remaining t0 woke0 rg = do
    modify (\fp -> fp { fpPpmn = p : fpPpmn fp, fpLocale = keep locale })
    local (const $ Embedding id) (anchor (sceneWith id lTiming observe))
  where
    woke = act woke0 (reactingForever react)
    react c t m = maybe standing id (reaction c t m)
    reaction c t = reactSpeak (inspiring >> return (Done, t)) c t
    inspiring = posting (FieldAction (inspire remaining t0 woke0 rg))
    locale = localeStarters remaining selectRemaining woke MainTheme t0 rg
    action = FieldAction (receive2 p)
    lTiming (KeepSF frozen) = KeepSF $ frozen >>> first (second (after 0.25 action))

receive2 p = do
    narration <- stdCommentary ((\name -> sentence '!' ["Received a", name]) . label (ppmnName p))
    local (`compEmbed` (embedArr (>.= narration))) $ momentary (playSoloSound Jingle)
    arrowWait narration
    local (const $ Embedding id) (anchor (sceneWith aTiming lTiming observe))
  where
    msg = CharacterSpeech (cIndex woke0) East
    aTiming = (second (after 0.25 (CharacterTurn East)) >>>)
    lTiming (KeepSF frozen) = KeepSF $ second (after 0.25 msg) >>> frozen

popUpMenu menu narration = do
    Embedding embed <- ask
    (next, k) <- lift . swont $ embed (menuControl >>> menu >>> (first $ arr (narration >.=)))
    return next

confirmMenu = backgroundMenu 64 48 (Point2 80 44) menu
  where
    menu = columnMenu [(Yes, True), (No, False)] (False, 0) 0

selectFirst _ _ _ = stdLecture (const "Looks like a phone...") >>= stdWait
selectRemaining _ _ _ = stdLecture (const "Don't be greedy.") >>= stdWait

inspire remaining t0 woke0 rg = do
    narration <- stdLecture (T.append "WOKE: " . prose GoodIKnewYoudDo)
    arrowWait narration
    longHesitation narration
    narration2 <- stdLecture (prose ButFuckItYourDestiny)
    arrowWait narration2
    name <- gets fpAvatarName
    narration3 <- stdLecture (const (name `T.snoc` '!'))
    arrowWait narration3
    narration4 <- stdLecture (prose FulfillYourDestinyWithYour)
    stdWait narration4
    modify (\fp -> fp {
        fpLocale = keep $ localeStarters remaining selectRemaining woke CenterTheme t0 rg,
        fpWorld = M.insert FamilyHouse1F (keep $ familyHouse1FAfterWoke (odGetTerrain (fpOfflineData fp) FamilyHouse1F)) (fpWorld fp)
        })
    local (const $ Embedding id) (anchor explore)
  where
    woke = act woke0 (let loop = greeting valediction >> loop in loop)
    valediction = T.append "WOKE: " . prose YoullDoGreatHeadNorth
