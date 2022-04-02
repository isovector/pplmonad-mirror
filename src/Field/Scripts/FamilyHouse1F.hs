{-# LANGUAGE Arrows, OverloadedStrings #-}

module Field.Scripts.FamilyHouse1F (
    familyHouse1FInitial,
    familyHouse1FAfterWoke
    ) where

import Control.Monad.State
import qualified Data.Text as T
import FRP.Yampa
import Data.Vector2
import Data.Point2
import Data.AffineSpace

import Activity
import Field.CardinalDirection
import Field.Character
import Field.Locale (stdLocale)
import Field.Output
import Field.Parameters
import Field.Personae
import Lightarrow
import Message
import MusicName
import OfflineData
import Output
import Ppmn.Parameters
import ProseName
import SpriteName
import StateClass

yourMom speechReaction = act c0 activity
  where  activity  = do  (m, t)  <- looking West 0.5
                         c       <- get
                         maybe (return ()) id (speechReaction c t m)
                         activity
         c0        = (mom (2, 2)) { cDirection = West }

initial = reactSpeak (speaking (T.append "MOM: " . prose OhHiSorryALittle))

afterWoke = reactSpeak (posting (FieldAction healing))

healing = do  n1  <- stdLecture (T.append "MOM: " . prose OhHiAreYouStill)
              ps  <- gets fpPpmn
              if any ((<) <$> ppmnHitPoints <*> ppmnMaxHitPoints) ps
                  then do
                      arrowWait n1
                      n2  <- stdLecture (prose OhYouNeedSomeHelp)
                      arrowWait n2
                      fadeTo White 0.5
                      modify (\fp -> fp { fpPpmn = heal (fpPpmn fp) })
                      fadeFrom White 0.5
                      n3  <- stdLecture (T.append "MOM: " . prose ITookCareOfIt)
                      stdWait n3
                  else
                      stdWait n1

heal = map (\p -> p { ppmnHitPoints = ppmnMaxHitPoints p })

familyHouse1F cs t0 = proc (avatar, news) -> do
    ((draw, sound, t), report) <- stdLocale cs TownTheme t0 -< (avatar, news)
    let xlate = characterXlate avatar
        drawPhone = drawSpriteOriented TurnL PPhone
        draw' = draw >.= withXlation xlate drawPhone (Point2 24 35)
    returnA -< ((draw', sound, t), report)

familyHouse1FInitial = familyHouse1F [yourMom initial]
familyHouse1FAfterWoke = familyHouse1F [yourMom afterWoke]
