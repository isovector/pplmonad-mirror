{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Field.Activity where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import FRP.Yampa

import Activity
import Field.Anchor
import Field.Battles
import Field.Character
import Field.Parameters
import Field.Terrain
import Inventory.Parameters
import Lightarrow
import Message
import OfflineData
import Output
import SoundName
import StateClass

pickUpItem item number
    | number < 1 = return ()
    | otherwise = do
        modify (\fp -> fp { fpItems =
            let items = fpItems fp
                newItem = item { itemStock = number }
            in
                M.insertWith (const increment) name newItem items
            })
        narration <- stdCommentary ((\t -> sentence '!' ["Got", t]) . plural . label name)
        local (`compEmbed` (embedArr (>.= narration))) $ momentary (playSoloSound Jingle)
        arrowWait narration
  where
    name = itemName item
    plural = case number of
        1 -> T.append "a "
        _ -> T.append (T.pack (show number ++ " ")) . flip T.snoc 's'
    increment = (!! number) . iterate itemAdd

encounter probability risks = do
    s1 <- state (randomR (0, 1))
    if s1 > (probability :: Double)
        then return NoEvent
        else do
            s2 <- state (randomR (0, totalPopulation))
            return $ fmap (FieldAction . wildBattle) (mergeEvents $ map (tryEncounter s2) accumulatedPopulations)
  where
    totalPopulation = sum populations
    accumulatedPopulations = zip ppmn (scanl1 (+) populations)
    ppmn = map fst risks
    populations = map snd risks
    tryEncounter s (ppmn, population) = if s <= (population :: Int) then Event ppmn else NoEvent

enterNewMap scene destination (x, y) direction fade = do
    if fade
        then do
            momentary (playSound Steps)
            fadeTo Black 0.5
        else return ()
    fp <- get
    let embedding = embedArr (>.= clearScreen Black)
        realPos = (fromIntegral x, fromIntegral y)
        avatar = pivot direction $ (fpAvatar fp) { cOccupy = occupy x y, cPosition = realPos }
    modify (\fp -> fp { fpAvatar = avatar })
    local (const embedding) $ changeScene scene destination
