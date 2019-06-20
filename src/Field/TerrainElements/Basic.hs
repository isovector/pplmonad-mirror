module Field.TerrainElements.Basic where

import FRP.Yampa

import Field.Terrain
import Field.TerrainElementExpression as TEExpr

basic tile orientation collides = TerrainElement {
    teCollides = collides,
    teInspect = const $ return NoEvent,
    teExpr = TEExpr.Basic tile orientation collides,
    teTile = (tile, orientation),
    teTraverse = return NoEvent
}
