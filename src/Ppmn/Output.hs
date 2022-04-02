{-# LANGUAGE OverloadedStrings #-}

module Ppmn.Output (
    drawElement,
    drawEpithet,
    drawHpMeter,
    drawHpNumerals,
    drawLevel,
    drawMoves,
    drawName,
    drawNumber,
    drawParams,
    drawPortrait,
    drawPpmn,
    drawStatus,
    statusSummary
) where

import qualified Data.Text as T
import Data.Vector2
import Data.Point2
import Data.AffineSpace

import {-# SOURCE #-} Battle.Moves (friendMoveByName)
import Battle.Parameters
import LabelName
import OfflineData
import Output
import Output.Window
import Ppmn.Parameters
import SpriteName
import TextUtil
import TileName

drawElement = drawElementPack . ppmnElement

drawElementPack (OneElement e) position = drawOneElement e position
drawElementPack (TwoElements e1 e2) position = drawTwoElements e1 e2 position

drawOneElement e position od = do
    drawText "TYPE1/" position od
    drawLabel (elementLabel e) (position .+^ vector2 8 8) od

drawTwoElements e1 e2 position od = do
    drawText "TYPE1/" position od
    drawLabel (elementLabel e1) (position .+^ vector2 8 8) od
    drawText "TYPE2/" (position .+^ vector2 0 16) od
    drawLabel (elementLabel e2) (position .+^ vector2 8 24) od

drawEpithet = drawLabel . ppmnEpithet

drawStatus p = statusSummary (p { ppmnPosition = Point2 0 8 })

drawName = drawLabel . ppmnName

drawNumber = drawText . ("No." `T.append`) . padInt 3 . ppmnNumber

drawPortrait orient = drawSpriteOriented orient . pssFront . ppmnSpriteSet

drawParams ppmn position = drawWindow position (5, 5) >.= attack >.= defense >.= speed
  where
    attack   = drawLabel Attack (position .+^ vector2 8 8) >.=
               drawText aNumeral (position .+^ vector2 (fromIntegral $ 72 - 8 * T.length aNumeral) 16)
    defense  = drawLabel Defense (position .+^ vector2 8 24) >.=
               drawText dNumeral (position .+^ vector2 (fromIntegral $ 72 - 8 * T.length dNumeral) 32)
    speed    = drawLabel Speed (position .+^ vector2 8 40) >.=
               drawText sNumeral (position .+^ vector2 (fromIntegral $ 72 - 8 * T.length sNumeral) 48)
    aNumeral = T.pack . show $ round (ppmnAttack ppmn)
    dNumeral = T.pack . show $ round (ppmnDefense ppmn)
    sNumeral = T.pack . show $ round (ppmnSpeed ppmn)

drawMoves ppmn position = drawWindow position (10, 5) >.= draw
  where
    draw = foldl (>.=) nullOut $ map (uncurry drawMove) (zip movesPadded offsets)
    moves = map (mpName . friendMoveByName) (ppmnMoves ppmn)
    movesPadded = moves ++ replicate (4 - length moves) Hyphen
    offsets = [ vector2 16 (8 + fromIntegral n * 16) | n <- [0 .. length movesPadded - 1] ]
    drawMove name offset = drawLabel name (position .+^ offset)

drawHpBar position maxHp 0   = drawRectangle 0 2 (Dark (Dark White)) position
drawHpBar position maxHp hp  = drawRectangle (max 1 $ 46 * max 0 hp / maxHp) 2 (Dark (Dark White)) position

statusSummary p od = do
    summaryArrow (y > 0) arrowPos od
    hpSummary (y > 0) hpPos p od
    drawLevel p levelPos od
    drawName p namePos od
  where
    hpPos = Point2 (fromIntegral ((x + 72 + 5) `mod` 165)) (fromIntegral y * 1.4 + 6)
    namePos = Point2 (fromIntegral ((x + 72 + 5) `mod` 165)) (fromIntegral y * 1.4)
    levelPos = Point2 (fromIntegral ((x + 96 + 5) `mod` 165)) (fromIntegral y * 1.4)
    arrowPos = case y of
                0 -> Point2 (fromIntegral ((x + 64 + 5) `mod` 165)) 16
                _ -> Point2 (fromIntegral ((x + 64 + 5) `mod` 165)) (fromIntegral y * 1.4 + 24)
    x = round (point2X position)
    y = round (point2Y position)
    position = ppmnPosition p

drawHpMeter p position od = do
    drawSprite HpLabel (position .+^ labelPos) od
    drawHpBar (position .+^ barPos) maxHp hp od
    drawSprite HpBorderSide (position .+^ borderPosL) od
    drawSprite HpBorderMiddle (position .+^ borderPosM) od
    drawSpriteOriented FlipX HpBorderSide (position .+^ borderPosR) od
  where
    hp = ppmnHitPoints p
    maxHp = ppmnMaxHitPoints p
    labelPos = vector2 (-16) (-13)
    barPos = vector2 1 0
    borderPosL = vector2 0 (-13)
    borderPosM = vector2 16 (-13)
    borderPosR = vector2 32 (-13)

drawHpNumerals p position = drawText t position
  where
    t = hp `T.append` "/" `T.append` maxHp
    hp = padIntWith ' ' 3 (round (ppmnHitPoints p))
    maxHp = padIntWith ' ' 3 (round (ppmnMaxHitPoints p))

hpSummary showNumerals position p od = do
    drawHpMeter p (position .+^ barPos) od
    if showNumerals
        then drawHpNumerals p (position .+^ numeralPos) od
        else return ()
  where
    barPos = vector2 16 13
    numeralPos = vector2 8 18

drawLevel p position od = do
    drawSprite LevelLabel position od
    drawText (T.pack $ show (ppmnLevel p)) (position .+^ numeralPos) od
  where
    numeralPos = vector2 16 8


summaryArrow flip position = drawTiledBg position (5, 1) tiles
  where
    tiles = if flip then [(t, FlipX) | t <- reverse tileNames] else [(t, Original) | t <- tileNames]
    tileNames = SummaryArrowBack : (replicate 3 SummaryArrowMiddle) ++ [SummaryArrowFront]

drawPpmn p = drawPpmnExplicit p (vector2 0 0)
drawPpmnExplicit p v = drawSpriteExplicit Original 1.0 White v (ppmnSprite p) (ppmnPosition p)
