module Outrun.Data.RoadObject (module Outrun.Data.RoadObject) where
import           Graphics.Gloss
import           Outrun.Data

data RoadObject = RoadObject
  { roadObjectPosition :: PointR3
  , roadObjectPicture  :: Picture
  }

shiftRoadObject :: PointR3 -> RoadObject -> RoadObject
shiftRoadObject delta ro =
  ro { roadObjectPosition = pos }
  where
    pos = shiftPos3D delta (roadObjectPosition ro)

data DynamicRoadObject = Dynamic
  { getRoadObject      :: RoadObject
  , roadObjectVelocity :: (Float, Float)
  }
