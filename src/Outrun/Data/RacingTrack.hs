module Outrun.Data.RacingTrack (module Outrun.Data.RacingTrack) where
import           Outrun.Data
import           Outrun.Data.RoadLine
import           Utils

newtype RacingTrack =
  RacingTrack { getTrack :: [RoadLine] }

trackMap :: ([RoadLine] -> [RoadLine]) -> RacingTrack -> RacingTrack
trackMap f (RacingTrack track) = RacingTrack (f track)

trackMapWith :: (RoadLine -> RoadLine) -> RacingTrack -> RacingTrack
trackMapWith f = trackMap (map f)

instance Semigroup RacingTrack where
  (<>) (RacingTrack track) =

    RacingTrack
      . (track ++)
      . shiftTrack (0, dy, dz)
      . shiftTrackIndices (length track)
      . getTrack

    where
      dy = maybe 0 (getYR3 . roadLinePosition) (lastMaybe track)
      dz = sum (map roadLineLength track)

instance Monoid RacingTrack where
  mempty = RacingTrack []

getTrackLength :: RacingTrack -> Float
getTrackLength = sum . map roadLineLength . getTrack

-- | Shifts the track by the given amount
shiftTrack :: PointR3 -> [RoadLine] -> [RoadLine]
shiftTrack delta = map (shiftRoadLinePosition delta)

shiftTrackIndices :: Int -> [RoadLine] -> [RoadLine]
shiftTrackIndices delta = map (shiftRoadLineIndex delta)
