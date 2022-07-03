-- | Default values for some types from Outrun.Data
module Outrun.Data.Defaults (module Outrun.Data.Defaults) where
import           Outrun.Data.Camera
import           Outrun.Data.RoadLine
import           Palettes

defaultRoadSegmentWidth :: Float
defaultRoadSegmentWidth  = 1000

defaultRoadSegmentLength :: Float
defaultRoadSegmentLength = 120

defaultRoadLine :: RoadLine
defaultRoadLine =
  RoadLine
    0 (0, 0, 0) 0 0
    Straight
    defaultRoadSegmentWidth
    defaultRoadSegmentLength
    afr32_darkdorado
    []

defaultWindowResolution :: (Int, Int)
defaultWindowResolution = (600, 400)

defaultCamera :: Camera
defaultCamera =
  Camera
    (0, 500, 0)
    defaultWindowResolution
    0.325 100 0.8 0
