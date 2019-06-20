{-# LANGUAGE FlexibleContexts #-}

module Field.TerrainElements.Exhibit where

import qualified Data.Map as M
import FRP.Yampa

import Activity
import Field.Parameters ()
import Field.Terrain
import Field.TerrainElementExpression as TEExpr
import Message
import StateClass

exhibit directions te@(TerrainElement { teExpr = Exhibit directions' expr }) = te {
    teExpr = Exhibit directions'' expr,
    teInspect = inspection directions''
}
  where
    directions'' = M.union directions directions'

exhibit directions te = te {
    teExpr = Exhibit directions (teExpr te),
    teInspect = inspection directions
}

inspection directions d = return $ maybeToEvent (M.lookup d directions >>= (return . FieldAction . (>>= stdWait) . stdLecture . prose))
