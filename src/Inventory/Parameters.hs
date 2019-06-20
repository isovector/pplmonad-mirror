module Inventory.Parameters where

import qualified Data.Text as T
import System.Random

import {-# SOURCE #-} Battle.Parameters
import Controls
import LabelName
import Lightarrow
import OfflineData
import Ppmn.Parameters
import StateClass

data ItemParameters = ItemParameters {
    itemFieldCont :: FlatEmbeddedActivity ItemUse Controls OfflineIO (),
    itemBattleCont :: FlatEmbeddedActivity ItemUse Controls OfflineIO (FlatEmbeddedActivity BattleParameters Controls OfflineIO ()),
    itemName :: LabelName,
    itemStock :: Int
}

itemAdd params@(ItemParameters { itemStock = s }) = params { itemStock = min 99 (s + 1) }
itemSubtract params@(ItemParameters { itemStock = s }) = params { itemStock = max 0 (s - 1) }

data ItemUse = ItemUse {
    iuAvatarName :: T.Text,
    iuParams :: ItemParameters,
    iuObject :: Ppmn,
    iuOfflineData :: OfflineData,
    iuRandomGenerator :: StdGen
}

instance TextSource ItemUse where
    prose name iu = odGetProse (iuOfflineData iu) name
    label name iu = odGetLabel (iuOfflineData iu) name

instance PpmnAction ItemUse where
    actionObject = iuObject
    actionObjectName iu = label (ppmnName (iuObject iu)) iu
    actionSubject = iuObject
    actionSubjectName iu = label (ppmnName (iuObject iu)) iu
    actionUpdateObject object iu = iu { iuObject = object }
    actionUpdateSubject = actionUpdateObject

instance RandomGen ItemUse where
    next iu = let (k, g) = next (iuRandomGenerator iu) in
                (k, iu { iuRandomGenerator = g })
    split iu = let (g1, g2) = split (iuRandomGenerator iu) in
                (iu { iuRandomGenerator = g1 }, iu { iuRandomGenerator = g2 })
    genRange = genRange . iuRandomGenerator
