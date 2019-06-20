{-# LANGUAGE FlexibleContexts #-}

module Field.TerrainElements.Portal where

import Control.Monad.State
import FRP.Yampa

import Field.Activity
import Field.Anchor
import Field.Character
import Field.Terrain
import Field.TerrainElementExpression as TEExpr
import Message

portal destination position fade te@(TerrainElement { teExpr = Portal _ _ _ expr }) = te {
    teExpr = Portal destination position fade expr,
    teTraverse = traversal destination position fade
}

portal destination position fade te = te {
    teExpr = Portal destination position fade (teExpr te),
    teTraverse = traversal destination position fade
}

traversal xDest (x, y) fade = do  dir <- gets cDirection
                                  let m = FieldAction (activity dir)
                                  return (Event m)
  where activity dir = enterNewMap explore xDest (x, y) dir fade
