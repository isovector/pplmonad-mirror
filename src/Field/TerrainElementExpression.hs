module Field.TerrainElementExpression where

import Data.Map

import Field.CardinalDirection
import Field.MapName
import LabelName
import Output
import ProseName
import TileName

data TerrainElementExpression = Basic TileName DrawOrientation Bool
                              | Habitat Double [((LabelName, Int), Int)] TerrainElementExpression
                              | Portal MapName (Int, Int) Bool TerrainElementExpression
                              | Exhibit (Map CardinalDirection ProseName) TerrainElementExpression
    deriving (Read, Show)
