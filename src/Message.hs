{-# LANGUAGE Arrows #-}

module Message where

import Data.List
import FRP.Yampa

import Controls
import Field.CardinalDirection
import {-# SOURCE #-} Field.Parameters
import Lightarrow
import OfflineData

data Message = AvatarInspect
             | AvatarTraverse
             | CharacterMove CardinalDirection
             | CharacterSpeech Int CardinalDirection
             | CharacterStop
             | CharacterTurn CardinalDirection
             | CursorDown
             | CursorLeft
             | CursorNext
             | CursorPrevious
             | CursorRight
             | CursorUp
             | Done
             | EditorClick
             | EditorClose
             | EditorOpen
             | EditorOptionNext
             | EditorOptionPrevious
             | EditorToolNext
             | EditorToolPrevious
             | EditorUnclick
             | FieldAction (FlatEmbeddedActivity FieldParameters Controls OfflineIO ())
             | ItemAcquisition Int
             | MarginBell
             | MenuCancel
             | MenuConsideration
             | MenuSelection
             | Pause
             | PendingText String
             | ProseScroll
             | ProseFast
             | ProseSlow
             | ProseBegin String
             | ProseEnd
             | Restart
             | TerrainElementSelect Int Int
             | TrainerAlert Int (Double, Double)

instance Show Message where
    show AvatarInspect = "AvatarInspect"
    show (CharacterMove dir) = "CharacterMove " ++ show dir
    show (CharacterSpeech _ _) = "CharacterSpeech"
    show CharacterStop = "CharacterStop"
    show (CharacterTurn _) = "CharacterTurn"
    show CursorDown = "CursorDown"
    show CursorLeft = "CursorLeft"
    show CursorNext = "CursorNext"
    show CursorPrevious = "CursorPrevious"
    show CursorRight = "CursorRight"
    show CursorUp = "CursorUp"
    show Done = "Done"
    show EditorClick = "EditorClick"
    show EditorClose = "EditorClose"
    show EditorOpen = "EditorOpen"
    show EditorOptionNext = "EditorOptionNext"
    show EditorOptionPrevious = "EditorOptionPrevious"
    show EditorToolNext = "EditorToolNext"
    show EditorToolPrevious = "EditorToolPrevious"
    show EditorUnclick = "EditorUnclick"
    show (FieldAction _) = "FieldAction"
    show (ItemAcquisition k) = "ItemAcquisition " ++ show k
    show MarginBell = "MarginBell"
    show MenuCancel = "MenuCancel"
    show MenuConsideration = "MenuConsideration"
    show MenuSelection = "MenuSelection"
    show Pause = "Pause"
    show (PendingText _) = "PendingText"
    show ProseScroll = "ProseScroll"
    show ProseFast = "ProseFast"
    show ProseSlow = "ProseSlow"
    show (ProseBegin _) = "ProseBegin"
    show ProseEnd = "ProseEnd"
    show Restart = "Restart"
    show (TerrainElementSelect _ _) = "TerrainElementSelect"
    show (TrainerAlert k x) = "TrainerAlert " ++ show k ++ " " ++ show x

say :: SF a b -> Message -> SF a (b, [Message])
sf `say` m = sf &&& constant [m]

sayE :: SF a (b, Event c) -> Message -> SF a ((b, [Message]), Event c)
sf `sayE` m = sf >>> first (identity &&& constant [m])

mute :: SF a b -> SF a (b, [Message])
mute sf = sf &&& constant []

muteE :: SF a (b, Event c) -> SF a ((b, [Message]), Event c)
muteE sf = sf >>> first (identity &&& constant [])

listen :: (Message -> Bool) -> SF a b -> SF (a, [Message]) (b, Event Message)
listen p = (*** (arr (find p) >>> edgeJust))

sayUpon :: Message -> Event a -> [Message]
sayUpon m = event [] (const [m])
