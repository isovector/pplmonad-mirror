module Field.TerrainElements (
    module Field.TerrainElements.Basic,
    module Field.TerrainElements.Exhibit,
    module Field.TerrainElements.Habitat,
    module Field.TerrainElements.Portal,
    evaluate
) where

import Field.TerrainElementExpression
import Field.TerrainElements.Basic
import Field.TerrainElements.Exhibit
import Field.TerrainElements.Habitat
import Field.TerrainElements.Portal

evaluate (Basic tile orientation collides) = basic tile orientation collides
evaluate (Habitat encounterRate riskExprs expr) = habitat encounterRate encounterRisks (evaluate expr)
  where
    encounterRisks = map evalRisk riskExprs
evaluate (Portal name position fade expr) = portal name position fade (evaluate expr)
evaluate (Exhibit directions expr) = exhibit directions (evaluate expr)
