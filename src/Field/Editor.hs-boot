module Field.Editor where

import FRP.Yampa
import FRP.Yampa.Geometry

import Controls
import Field.Terrain
import OfflineData

fieldEditor :: SF (Controls, Vector2 Double, Terrain) (OfflineIO, Terrain)
