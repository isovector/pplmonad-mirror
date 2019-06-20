module Field.Terrain where

import Control.Monad.Cont
import Control.Monad.State
import Control.Arrow
import qualified Data.Map as M
import FRP.Yampa

import Field.CardinalDirection
import {-# SOURCE #-} Field.Character
import {-# SOURCE #-} Field.TerrainElements
import Field.TerrainElementExpression
import Message
import Output
import TileName

data TerrainElement = TerrainElement {
    teCollides :: Bool,
    teInspect :: CardinalDirection ->
                 StateT  Character
                         (Cont (SF  (Terrain, Event Message)
                                    (Character, Event Message)))
                         (Event Message),
    teExpr :: TerrainElementExpression,
    teTile :: (TileName, DrawOrientation),
    teTraverse :: StateT  Character
                          (Cont (SF  (Terrain, Event Message)
                                     (Character, Event Message)))
                          (Event Message)
}

teDefault = TerrainElement {
    teCollides = False,
    teInspect = const (return NoEvent),
    teExpr = Basic Blank Original False,
    teTile = (Blank, Original),
    teTraverse = return NoEvent
} 

instance Show TerrainElement where
    show = show . teExpr

instance Read TerrainElement where
    readsPrec = (map (first evaluate) .) . readsPrec

data Terrain = Terrain {
    tBackgroundColor :: Color,
    tElements :: M.Map (Int, Int) TerrainElement
}
    deriving (Read, Show)

mapWithIndices = M.mapWithKey

empty :: Terrain
empty = Terrain White M.empty

getElem :: Int -> Int -> Terrain -> Maybe TerrainElement
getElem x y = (M.!? (y, x)) . tElements

setElem :: TerrainElement -> Int -> Int -> Terrain -> Terrain
setElem elem x y t@(Terrain _ es) = t { tElements = M.insert (y, x) elem es }

adjustElem :: (TerrainElement -> TerrainElement) -> Int -> Int -> Terrain -> Terrain
adjustElem f x y t@(Terrain _ es) = t { tElements = M.adjust f (y, x) es }

deleteElem :: Int -> Int -> Terrain -> Terrain
deleteElem x y t@(Terrain _ es) = t { tElements = M.update (const Nothing) (y, x) es }

occupy :: Int -> Int -> Terrain -> Terrain
occupy x y t = setElem (solidify (maybe teDefault id (getElem x y t))) x y t
  where
    solidify te = te { teCollides = True }
