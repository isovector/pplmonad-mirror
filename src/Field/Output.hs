{-# LANGUAGE FlexibleContexts #-}

module Field.Output where

import FRP.Yampa.Geometry

import Data.Foldable
import Field.Terrain
import Field.TerrainElementExpression
import Field.Character
import OfflineData
import Output
import SpriteName

screenCenterOffset = vector2 64 64 

drawCharacter (Character { cDraw = draw, cPosition = (x, y) }) = draw (Point2 (x * 16) (y * 16 - 4))

drawTerrainElement TerrainElement { teTile = (tile, orientation), teExpr = Habitat _ _ _ } p x = drawXlatedTile tile orientation p x >.= drawXlatedSprite Wifi p x
drawTerrainElement TerrainElement { teTile = (tile, orientation), teExpr = Portal _ _ _ (Habitat _ _ _) } p x = drawXlatedTile tile orientation p x >.= drawXlatedSprite Wifi p x
drawTerrainElement TerrainElement { teTile = (tile, orientation) } p x = drawXlatedTile tile orientation p x

drawTerrain terrain xlation = foldl' (>.=) clear $ mapWithIndices (uncurry tiler) elements
  where
    clear = clearScreen (tBackgroundColor terrain)
    elements = tElements terrain
    tiler r c element = drawTerrainElement element (translate r c) xlation
    translate r c = Point2 (fromIntegral $ 16 * c) (fromIntegral $ 16 * r)

withXlation xlation f = f . (.-^ (xlation ^-^ screenCenterOffset))

drawXlatedSprite sprite position xlation = withXlation xlation (drawSprite sprite) position
drawXlatedTile tile orientation position xlation = withXlation xlation (drawTile tile) position orientation

tileUnderPixel xlate x y = (floor $ vector2X indexVector, floor $ vector2Y indexVector)
  where
    screenOffset = vector2 (fromIntegral x) (fromIntegral y)
    indexVector = (xlate ^-^ screenCenterOffset ^+^ screenOffset) ^/ 16
