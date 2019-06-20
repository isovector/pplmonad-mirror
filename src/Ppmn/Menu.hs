{-# LANGUAGE AllowAmbiguousTypes, Arrows, FlexibleContexts, OverloadedStrings #-}

module Ppmn.Menu (selectPpmn, personMenu, releasePpmn, reorderPpmn, personDetails, personStatus) where

import Control.Monad.Reader
import Control.Monad.State
import FRP.Yampa
import FRP.Yampa.Geometry

import Activity
import ControlsMaps
import Lightarrow
import Menu
import OfflineData
import Output
import Ppmn.Output
import Ppmn.Parameters
import ProseName
import SoundName
import SpriteName
import StateClass
import TextBox as TB
import TextUtil

peopleMenu run cancel ppmn k0 = fullscreenMenu menu
  where
    menu = scrollMenu 5 presenter ((map (id *** run) (ppmn `zip` [0 .. length ppmn - 1]))) cancel k0
    presenter ppmn t b = ((par zip $ map (arr . (. fst) . (uncurry summary)) range) &&& extra) >>> arr (uncurry (flip (:))) 
      where range = take (b - t + 1) $ drop t (ppmn `zip` rotations)
            extra = constant $ if b + 1 < length ppmn then summary (ppmn !! (b + 1)) (rotations !! (b + 1)) (Point2 8 88) else nullOut
    summary p orient x od = do
        drawSpriteOriented orient PPhone x od
        drawLevel p (x .+^ vector2 92 (-12)) od
        drawHpMeter p (x .+^ vector2 40 7) od
        drawHpNumerals p (x .+^ vector2 96 4) od
        drawName p (x .+^ vector2 16 (-4)) od
    rotations = [Original, TurnR, FlipBoth, TurnL] ++ rotations

personMenu options position = do
    (next, _) <- choiceReplace (menuControl >>> showMenu)
    next
  where
    showMenu  = backgroundMenu 80 80 (Point2 80 64) menu
    menu      = columnMenu (options position) (return (), 0) 0

selectPpmn instruction getPpmn run k0 cancel = do
    ppmn <- gets getPpmn
    text <- gets (prose instruction)
    let embed = embedArr (>.= TB.drawConstant text)
        inlay out = (`compEmbed` embedArr ((out >.= TB.drawConstant text) >.=))
    (action, k) <- local (embed `compEmbed`) $ choiceInlay inlay (menuControl >>> peopleMenu run (cancel (), 0) ppmn k0)
    action
    ppmn' <- gets getPpmn
    selectPpmn instruction getPpmn run (min k (length ppmn' - 1)) cancel

reorderPpmn getPpmn putPpmn index1 = do
    ppmn <- gets getPpmn
    text <- gets (prose MovePersonWhere)
    let embed = embedArr (>.= TB.drawConstant text)
    (next, k) <- local (const embed) $ choiceOverlay (menuControl >>> peopleMenu run (return (), 0) ppmn index1)
    next
  where
    run index2 = do
        ppmn <- gets getPpmn
        let person1 = ppmn !! index1
            person2 = ppmn !! index2
            ppmn'   = take index1 ppmn ++ (person2 : drop (index1 + 1) ppmn)
            ppmn''  = take index2 ppmn' ++ (person1 : drop (index2 + 1) ppmn')
        modify (putPpmn ppmn'')

releasePpmn getPpmn putPpmn position = do
    ppmn <- gets getPpmn
    modify (putPpmn $ take position ppmn ++ drop (position + 1) ppmn)
    name <- gets (label (ppmnName (ppmn !! position)))
    stdCommentary (const $ sentence '!' ["Let", name, "go"]) >>= stdHesitation

personStatus p = do 
    lift . swont $ constant (number >.= status >.= params >.= element) &&& after 0.75 ()
    local (const $ embedArr ((number >.= status >.= params >.= element) >.=)) (playCry p)
    local (const $ embedArr ((picture >.= number >.= status) >.=)) $ do
        lastPage (params >.= element)
        lastPage moves
        return ()
  where
    picture  = drawPortrait FlipX p (Point2 8 0)
    status   = drawStatus p
    number   = drawNumber p (Point2 8 56)
    params   = drawParams p (Point2 0 64)
    element  = drawElement p (Point2 80 72)
    moves    = drawMoves p (Point2 0 64)
 
personDetails p = do
    t <- gets (prose (ppmnDescription p))
    let lines = wrap 17 t
        gs    = groups lines
        pages = mapM_ (page . describe) (init gs) >> lastPage (describe (last gs))
    lift . swont $ constant (bars >.= number >.= name >.= epithet) &&& after 0.75 ()
    local (const $ embedArr ((bars >.= picture >.= number >.= name >.= epithet) >.=)) $ do
        playCry p
        pages
        return ()
  where
    bars = drawRectangle 160 4 (Dark (Dark White)) (Point2 0 0) >.=
           drawRectangle 160 1 (Dark White) (Point2 0 79)
    picture = drawPortrait FlipX p (Point2 8 8)
    number = drawNumber p (Point2 8 64)
    name = drawName p (Point2 64 16)
    epithet = drawEpithet p (Point2 64 32)
    groups (x:y:z:rest) = [x,y,z] : (groups rest)
    groups [x,y] = [[x,y,""]]
    groups [x] = [[x,"",""]]
    groups [] = []

page output = do
    Embedding embed <- ask
    lift . dSwont $ embed $ proc controls -> do
        command <- pageControl  -< controls
        wait    <- TB.waitArrow -< 0.5
        sound   <- soundTrigger SoundName.Accept -< command
        returnA -< (output >.= wait >.= sound, command)

lastPage output = do
    Embedding embed <- ask
    lift . swont $ embed $ proc controls -> do
        command <- pageControl -< controls
        returnA -< (output, command)

describe [l1, l2, l3] = 
    drawText l1 (Point2 8 88) >.=
    drawText l2 (Point2 8 104) >.= 
    drawText l3 (Point2 8 120)
