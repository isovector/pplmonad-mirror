{-# LANGUAGE ViewPatterns #-}

module Field.TerrainElements.Habitat where

import Field.Activity
import Field.Terrain
import Field.TerrainElementExpression
import Ppmn.Parameters
import Ppmn.Species

habitat rate risks' te@(teExpr -> Habitat _ risks expr) = te {
    teExpr = Habitat rate (risks ++ map riskExpr risks') expr,
    teTraverse = encounter rate (map evalRisk risks ++ risks')
}
habitat rate risks te = te {
    teExpr = Habitat rate (map riskExpr risks) (teExpr te),
    teTraverse = encounter rate risks
}

evalRisk ((name, level), population) = (ppmnByName name level, population)
riskExpr (Ppmn { ppmnName = name, ppmnLevel = level }, population) = ((name, level), population)
