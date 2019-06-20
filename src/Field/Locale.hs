{-# LANGUAGE Arrows, FlexibleContexts #-}

module Field.Locale where

import Data.Foldable
import FRP.Yampa

import Field.Activity
import Field.Character
import Field.Output
import Field.Terrain
import Lightarrow
import Message
import OfflineData
import Output

stdLocale characters music t0 = proc (avatar, news) -> do
    let  cnews   = filterE cValidNews news
    rec
        t           <- iPre t0          -< occupation cs t0
        (cs, cMsgs) <- parBZ characters -< (cOccupy avatar t, cnews)
    let  draw    = charactersDraw cs xlate
         xlate   = characterXlate avatar
         report  = foldl' rMerge NoEvent cMsgs
         sound   = restartMusic news
    returnA -< ((draw, sound, t), report)
  where  restartMusic (Event Restart)  = playRepeatingMusic music
         restartMusic _                = nullOut
         cValidNews AvatarTraverse     = False
         cValidNews Restart            = False
         cValidNews _                  = True

occupation :: [Character] -> Terrain -> Terrain
occupation = foldl' ((. cOccupy) . (.)) id

charactersDraw = (foldl' (>.=) nullOut .) . sequence . map drawCharacter

terrainDraw = drawTerrain

holdItem item x out collides index = terrainTrigger out hold triggered x
  where
    isItem (ItemAcquisition index') = index == index'
    isItem _                        = False
    triggered m
        | isItem m  = Just (FieldAction (pickUpItem item 1))
        | otherwise = Nothing
    hold te = te {
        teCollides = collides || teCollides te,
        teInspect = const $ return $ Event (ItemAcquisition index)
        }

alertTrainer index (x, y) = terrainTrigger nullOut alert triggered (x, y)
  where  triggered (TrainerAlert k _)
            | k == index  = Just Done
         triggered _      = Nothing
         alert te         = te { teTraverse = return (Event m) }
         m                = TrainerAlert index (fromIntegral x, fromIntegral y)

terrainTrigger out adjustment triggered (x, y) = initial `dSwitch` const final
  where
    initial = proc m -> do
        let m' = mapFilterE triggered m 
        returnA -< ((out, adjustElem adjustment x y, m'), m')
    final = constant (nullOut, id, NoEvent)
