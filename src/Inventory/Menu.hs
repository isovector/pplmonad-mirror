{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Inventory.Menu where

import qualified Data.Text as T
import FRP.Yampa
import Data.Vector2
import Data.Point2
import Data.AffineSpace

import Inventory.Parameters
import LabelName
import Menu
import OfflineData
import Output

-- why the fuck is it the case that cancel :: (k, Int)
itemMenu run cancel inventory initial = backgroundMenu 128 88 (Point2 32 16) menu
  where
    menu = scrollMenu 4 itemPresenter (map (itemDraw &&& run) inventory ++ [(drawLabel Cancel, fst cancel)]) cancel initial

itemPresenter drawers t b = par zip $ map (arr . (. fst)) range
  where
    range = take (b - t + 1) $ drop t drawers

itemDraw item position = drawLabel (itemName item) position >.= drawText (T.pack $ "x " ++ show (itemStock item)) (position .+^ vector2 64 8)
