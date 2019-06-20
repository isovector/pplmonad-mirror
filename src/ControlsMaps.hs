{-# LANGUAGE Arrows #-}

module ControlsMaps where

import FRP.Yampa

import Controls
import Field.CardinalDirection
import Lightarrow
import Message

menuControl = proc controls -> do
    select <- edgeTag [MenuSelection]  -< ctlA controls 
    cancel <- edgeTag [MenuCancel]     -< ctlB controls
    next   <- edgeTag [CursorNext]     -< ctlDown controls
    prev   <- edgeTag [CursorPrevious] -< ctlUp controls
    returnA -< event [] concat (catEvents [select, cancel, next, prev])

boxMenuControl = proc controls -> do
    select <- edgeTag [MenuSelection] -< ctlA controls 
    cancel <- edgeTag [MenuCancel]    -< ctlB controls
    down   <- edgeTag [CursorDown]    -< ctlDown controls
    left   <- edgeTag [CursorLeft]    -< ctlLeft controls
    right  <- edgeTag [CursorRight]   -< ctlRight controls
    up     <- edgeTag [CursorUp]      -< ctlUp controls
    returnA -< event [] concat (catEvents [select, cancel, down, left, right, up])

proseControl = proc controls -> do
    lowControls <- aFalse -=> identity -< controls
    fast        <- edgeTag ProseFast   -< ctlA lowControls
    slow        <- edgeTag ProseSlow   -< not (ctlA lowControls)
    scroll      <- edgeTag ProseScroll -< ctlA controls
    returnA -< event [] id (catEvents [scroll, slow, fast])
  where
    aFalse c = c { ctlA = False }

pageControl = arr ((||) <$> ctlA <*> ctlB) >>> edge

avatarControl = proc controls -> do
    controls' <- dirFalse -=> identity -< controls
    inspect <- edgeTag AvatarInspect         -< ctlA controls'
    pause   <- edgeTag Pause                 -< ctlStart controls'
    north   <- edgeTag (CharacterMove North) -< ctlUp controls' 
    west    <- edgeTag (CharacterMove West)  -< ctlLeft controls'
    south   <- edgeTag (CharacterMove South) -< ctlDown controls'
    east    <- edgeTag (CharacterMove East)  -< ctlRight controls'
    stop    <- edgeTag CharacterStop         -< not . or $ sequence [ctlUp, ctlLeft, ctlDown, ctlRight] controls'
    returnA -< mergeEvents [pause, stop, north, west, south, east, inspect]

dirFalse c = c { ctlUp = False, ctlLeft = False, ctlDown = False, ctlRight = False }

editorControl = editorControlClosed `dSwitch` const (editorControlOpen `dSwitch` const editorControl)

editorControlClosed = arr ctlTab >>> edgeTag EditorOpen >>> arr dup

editorControlOpen = proc c -> do
    click   <- edgeTag EditorClick          -< ctlLMB c || ctlMMB c
    unclick <- edgeTag EditorUnclick        -< not (ctlLMB c)
    close   <- edgeTag EditorClose          -< ctlTab c
    optN    <- edgeTag EditorOptionNext     -< ctlW c
    optP    <- edgeTag EditorOptionPrevious -< ctlQ c
    toolN   <- edgeTag EditorToolNext       -< ctlR c
    toolP   <- edgeTag EditorToolPrevious   -< ctlE c
    returnA -< (mergeEvents [close, optN, optP, toolN, toolP, click, unclick], close)

isEditorClose EditorClose = True
isEditorClose _           = False
