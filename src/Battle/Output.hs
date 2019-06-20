{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Battle.Output where

import qualified Data.Text as T
import FRP.Yampa.Geometry

import Battle.ElementalEffect
import Battle.Parameters
import Inventory.Parameters
import Ppmn.Output
import Ppmn.Parameters
import ProseName
import OfflineData
import Output
import StateClass
import SpriteName

itemUse params bp = ItemUse {
    iuAvatarName = bpAvatarName bp,
    iuParams = params,
    iuObject = bpFriend bp,
    iuOfflineData = bpOfflineData bp,
    iuRandomGenerator = bpRandomGenerator bp
}

class BattleScene s where
    sceneDefault :: s -> OfflineIO
    sceneDefault = (>.=) . sceneSecond >>= (. sceneFirst)
    sceneFirst :: s -> OfflineIO
    sceneFirstSummary :: s -> OfflineIO
    sceneSecond :: s -> OfflineIO
    sceneSecondSummary :: s -> OfflineIO
    sceneSummary :: s -> OfflineIO
    sceneSummary = (>.=) . sceneSecondSummary >>= (. sceneFirstSummary)

instance BattleScene BattleParameters where
    sceneFirst bp = let f = bpFriend bp in drawSprite (ppmnSprite f) (ppmnPosition f)
    sceneFirstSummary bp = let f = bpFriend bp in statusSummary f
    sceneSecond bp = let e = bpEnemy bp in drawSprite (ppmnSprite e) (ppmnPosition e)
    sceneSecondSummary bp = let e = bpEnemy bp in statusSummary e

instance BattleScene MoveState where
    sceneFirst ms = let s = msSubject ms in drawSprite (ppmnSprite s) (ppmnPosition s)
    sceneFirstSummary ms = let s = msSubject ms in statusSummary s
    sceneSecond ms = let o = msObject ms in drawSprite (ppmnSprite o) (ppmnPosition o)
    sceneSecondSummary ms = let o = msObject ms in statusSummary o

effectProse Weak s = prose ItsNotVeryEffective s
effectProse Basic s = prose ItsAsEffectiveAsYoud s
effectProse Strong s = prose ItsSuperEffective s
effectProse Futile s = sentence '!' [itDoesntAffect, name]
  where
    itDoesntAffect = prose ItDoesntAffect s
    name = actionObjectName s

missProse s = sentence '!' [name `T.append` possesive, attackMissed]
  where
    name = actionSubjectName s
    possesive = prose S s
    attackMissed = prose AttackMissed s

announceProse s = sentence '!' [name, used, move]
  where
    name = actionSubjectName s
    used = T.toLower $ prose Used s
    move = T.toUpper $ label (moveName s) s

buffProse recipient stat s = sentence '!' [recipient `T.append` possesive, statLabel, rose]
  where
    possesive = prose S s
    statLabel = label stat s
    rose = prose Rose s

greatBuffProse recipient stat s = sentence '!' [recipient `T.append` possesive, statLabel, greatly, rose]
  where
    possesive = prose S s
    statLabel = label stat s
    greatly = prose Greatly s
    rose = prose Rose s

debuffProse recipient stat s = sentence '!' [recipient `T.append` possesive, statLabel, fell]
  where
    possesive = prose S s
    statLabel = label stat s
    fell = prose Fell s

greatDebuffProse recipient stat s = sentence '!' [recipient `T.append` possesive, statLabel, greatly, fell]
  where
    possesive = prose S s
    statLabel = label stat s
    greatly = prose Greatly s
    fell = prose Fell s

wildAlertProse enemy s = sentence '!' ["Wild", enemyName, "appeared"]
  where
    enemyName = label (ppmnName (battleEnemy s)) s

trainerAlertProse nameLabel s = sentence '!' [name, "wants to fight"]
  where
    name = label nameLabel s

trainerDeployProse trainerLabel s = sentence '!' [trainerName, "sent out", enemyName]
  where
    trainerName = label trainerLabel s
    enemyName = label (ppmnName (battleEnemy s)) s

choosePpmnProse s = sentence '!' ["Go!", friendName]
  where
    friendName = label (ppmnName (battleFriend s)) s

recallPpmnProse s = sentence '!' [friendName, "OK! Come back"]
  where
    friendName = label (ppmnName (battleFriend s)) s

faintProse s = sentence '!' [name, "died"]
  where
    name = actionObjectName s

drawPhone orient position = drawSpriteOriented orient PPhone position

landedPhone = drawPhone TurnR (Point2 124 48)
