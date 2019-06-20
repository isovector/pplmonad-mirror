{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module MainAutomaton where

import Control.Monad.Cont
import Control.Monad.RWS
import qualified Data.Map as M
import FRP.Yampa hiding (left, right)
import FRP.Yampa.Geometry
import System.Random

import Activity
import Controls
import ControlsMaps
import Field
import Intro
import Inventory.Items.Booch
import Inventory.Items.PPhone
import Inventory.Parameters
import LabelName
import Lightarrow
import OfflineData
import Output
import Ppmn.Parameters
import Ppmn.Species
import ProseName
import SoundName
import SpriteName

--import Control.Monad.Base
--import Control.Monad.Trans.Reader
--import Data.Functor.Identity
--import Data.MonadicStreamFunction.Core (liftMStreamF)
--import FRP.BearRiver hiding (next)

mainAutomaton :: StdGen -> OfflineData -> SF Controls OfflineIO
mainAutomaton rgen od = runCont (intro od >>= field rgen od) final
  where
    final (_, _, ()) = constant $ drawText "FIN" (Point2 100 100)

field rgen od name = runRWST k (Embedding id) fp
  where
    k = --callCC (selecting 0)
        --personDetails ignoloofBase
        Field.anchor explore
        --wildBattle (atLevel 3 ignoloofLearnMove ignoloofBase)
        --trainerBattle TheDonald Donald TheGreatestWitchHuntIn [ppmnByName LabelName.Blamotage 1, ppmnByName LabelName.Unner 1]
        --finalBattle
    fp = FieldParameters { 
        fpAvatar = (protagonist (0, 4)) { cRandomGenerator = aRgen },
        fpAvatarName = name,
        fpCounters = PpmnCounters 0 0 0 (M.fromList []),
        fpItems = M.fromList [], {-
            (Booch, booch { itemStock = 3 }),
            (LabelName.PPhone, pPhone { itemStock = 3 })
            ],-}
        fpLocale = (fpWorld fp) M.! (fpMap fp),
        fpMap = FamilyHouse2F,
        fpOfflineData = od,
        fpPpmn = [],
        fpRandomGenerator = rgen',
        fpWorld = world rgen' od
    }
    (aRgen, rgen') = split rgen

world rgen od = M.fromList (zip maps scripts')
  where
    (_, scripts') = foldr setRgen (rgen, []) scripts
    setRgen s (r, ss) = let (r1, r2) = split r in (r2, s r1 : ss)
    scripts = [keep . scriptByMap m (odGetTerrain od m) | m <- maps]
    maps = [toEnum 0 ..]
