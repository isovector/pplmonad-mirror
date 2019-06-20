module Field.Parameters where

import qualified Data.Map as M
import qualified Data.Text as T
import FRP.Yampa
import FRP.Yampa.Geometry
import System.Random

import {-# SOURCE #-} Field.Character
import Field.MapName
import Field.Terrain
import Inventory.Parameters
import LabelName
import Lightarrow
import Message
import OfflineData
import Ppmn.Parameters
import StateClass

data FieldParameters = FieldParameters {
    fpAvatar :: Character,
    fpAvatarName :: T.Text,
    fpCounters :: PpmnCounters,
    fpItems :: M.Map LabelName ItemParameters,
    fpLocale :: Locale,
    fpMap :: MapName,
    fpOfflineData :: OfflineData,
    fpPpmn :: [Ppmn],
    fpRandomGenerator :: StdGen,
    fpWorld :: M.Map MapName Locale
}

instance TextSource FieldParameters where
    prose name fp = odGetProse (fpOfflineData fp) name
    label name fp = odGetLabel (fpOfflineData fp) name

type Locale = KeepSF (Character, Event Message) ((OfflineIO, OfflineIO, Terrain), Event Message)

itemUse params object fp = ItemUse {
    iuAvatarName = fpAvatarName fp,
    iuParams = params,
    iuObject = battlePpmnFriend object,
    iuOfflineData = fpOfflineData fp,
    iuRandomGenerator = fpRandomGenerator fp
}

battlePpmnEnemy :: Ppmn -> Ppmn
battlePpmnEnemy = battlePpmn (Point2 100 0) (pssFront . ppmnSpriteSet)

battlePpmnFriend :: Ppmn -> Ppmn
battlePpmnFriend = battlePpmn (Point2 8 40) (pssBack . ppmnSpriteSet)

battlePpmn position sprite ppmn = ppmn {
    ppmnAccuracyStage    = 0,
    ppmnAttackStage      = 0,
    ppmnDefenseStage     = 0,
    ppmnEvasivenessStage = 0,
    ppmnPosition         = position,
    ppmnSprite           = sprite ppmn
}
