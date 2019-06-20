{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings, NoMonomorphismRestriction #-}

module Field.Menu where

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.RWS
import qualified Data.Map as M
import qualified Data.Text as T
import FRP.Yampa
import FRP.Yampa.Geometry
import System.Random

import Activity
import Controls
import ControlsMaps
import Field.Parameters
import Inventory.Menu
import Inventory.Parameters
import LabelName
import Lightarrow
import Menu
import Message
import OfflineData
import Output
import Ppmn.Menu
import Ppmn.Output
import Ppmn.Parameters
import Ppmn.Species
import ProseName
import SoundName
import SpriteName
import StateClass
import TextUtil

selecting k0 cancel = do
    fp <- get
    (next, k) <- choiceOverlay (fieldMenuControl >>> fieldMenu fp k0 cancel)
    next
    selecting k cancel

fieldMenuControl = proc controls -> do
    commands <- menuControl          -< controls
    unpause  <- edgeTag [MenuCancel] -< ctlStart controls
    returnA -< event commands id unpause

fieldMenu fp k0 cancel = backgroundMenu 80 128 (Point2 80 0) menu
  where
    menu = columnMenuEx presenter (options fp cancel) (cancel (), 0) k0
    presenter = par zip . map (arr . (. fst) . drawText)

options fp cancel =
    [ (label People fp, callCC (people 0)),
      (label Item fp, callCC (items 0)),
      (fpAvatarName fp, avatar),
      (label Exit fp, cancel ()) ]

people position cancel = selectPpmn ChooseAPerson fpPpmn (personMenu (personOptions cancel)) position cancel

personOptions cancel position =
    [ (Data, get >>= \fp -> personDetails (fpPpmn fp !! position)),
      (Status, gets fpPpmn >>= \ppmn -> personStatus (ppmn !! position)),
      (Switch, reorderPpmn fpPpmn (\p fp -> fp { fpPpmn = p }) position),
      (Cancel, cancel () >> return ()) ]

items k0 cancel = do
    inventory <- gets (M.elems . fpItems)
    embedding <- ask
    let run item     = callCC $ \cc -> selectPpmn UseItemOnWhichPerson fpPpmn (run' cc item) 0 cancel
        run' continue item n  = do
            fp <- get
            object <- gets ((!! n) . fpPpmn)
            iu <- gets (itemUse item object)
            (result, _) <- lift $ execRWST (itemFieldCont item) embedding iu
            continue (result, n)
    (next, k) <- choiceOverlay (menuControl >>> itemMenu run (cancel (), 0) inventory k0)
    (result@(ItemUse { iuParams = params }), n) <- next
    let newInventory fp = if itemStock params == 0
                            then M.delete (itemName params) (fpItems fp)
                            else M.insert (itemName params) params (fpItems fp)
        newPpmn fp = (do  t <- take n
                          d <- drop (n + 1)
                          pure (t ++ (actionObject result : d))) (fpPpmn fp)
    modify (\fp -> fp { fpItems = newInventory fp,
                        fpPpmn = newPpmn fp,
                        fpRandomGenerator = mkStdGen $ fst (random result) })
    inventory' <- gets (M.elems . fpItems)
    items (max 0 $ min k (length inventory' - 1)) cancel

avatar = do
    fp <- get
    lift . swont $ constant (draw fp) &&& pageControl
    momentary (playSound SoundName.Accept)
  where
    draw fp = bg >.= portrait >.= (summary fp) >.= (favorite fp)
    bg od = do
        clearScreen (Dark White) od
        drawRectangle 152 64 Black (Point2 6 6) od
        drawRectangle 152 64 White (Point2 4 4) od
        drawRectangle 152 64 Black (Point2 6 74) od
        drawRectangle 152 64 White (Point2 4 72) od
    portrait = drawSprite AvatarPortrait (Point2 108 8)
    summary fp@(FieldParameters { fpCounters = pc }) od = do
        drawText (label Name fp `T.append` ": ") (Point2 8 8) od
        drawText (fpAvatarName fp) (Point2 56 8) od
        drawText "CATCHES: " (Point2 8 24) od
        drawText (padIntWith ' ' 4 (pcCatches pc)) (Point2 80 24) od
        drawText "KILLS: " (Point2 8 40) od
        drawText (padIntWith ' ' 4 (pcKills pc)) (Point2 80 40) od
        drawText "LOSSES: " (Point2 8 56) od
        drawText (padIntWith ' ' 4 (pcLosses pc)) (Point2 80 56) od
    favorite fp@(FieldParameters { fpCounters = pc }) od = do
        drawText "FAVORITE" (Point2 8 80) od
        drawText "SPECIES:" (Point2 20 96) od
        if null (pcPlays pc)
            then return ()
            else do
                let favoriteName = mostPlayed pc
                case favoriteName of
                    Just name -> do
                        let favorite = ppmnByName name 1
                        drawText (label name fp) (Point2 8 120) od
                        drawPortrait Original favorite (Point2 100 76) od
                    _ -> return ()
