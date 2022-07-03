module Outrun.Building (module Outrun.Building) where
import           Data.List
import           Graphics.Gloss
import           Outrun.Data.Custom
import           Outrun.Data.Defaults
import           Outrun.Data.RacingTrack
import           Outrun.Data.RoadLine
import           Palettes
import           Utils

-- | Makes a track given its length, curve rate and a list of alternating colors
makeTrack :: TrackLength -> [Color] -> RacingTrack
makeTrack trackLength = makeTrackCustom (Common trackLength)

-- | Makes a track with customizable length and list of alternating colors
makeTrackCustom
  :: WithCustom TrackLength Int
  -> [Color]
  -> RacingTrack
makeTrackCustom trackLength colors = trackFrom coloredLines
  where
    len = fromCustom trackLengthValue trackLength

    colors' = case colors of
      [] -> [afr32_sungrass, afr32_olive]
      _  -> colors

    coloredLines = take len
      ( cycle
      ( map (`setRoadLineColor` defaultRoadLine)
        colors'
      ))

    trackFrom =
      mconcat . map (RacingTrack . singleton)

-- | Generates infinite racing track from the given one
infRacingTrack :: RacingTrack -> RacingTrack
infRacingTrack track =
  track <> infRacingTrack track

-- | Adds a curve to a track
addCurve :: TrackChangeRate -> TrackCurveDirection -> RacingTrack -> RacingTrack
addCurve = addCurveCustom . Common

-- | Adds a custom curve to a track
addCurveCustom
  :: WithCustom TrackChangeRate Float
  -> TrackCurveDirection
  -> RacingTrack
  -> RacingTrack
addCurveCustom curve dir = trackMap (map func)
  where
    func = setRoadLineCurveRate
      ( trackCurveDir dir
      ( fromCustom trackCurveValue curve))

-- | Adds a pitch to a track
-- Positive pitch should be followed by negative pitch
-- and vice versa to create a hill, otherwise the track
-- will just go upwards or downwards!
addPitch :: TrackChangeRate -> TrackPitchDirection -> RacingTrack -> RacingTrack
addPitch = addPitchCustom . Common

-- | Adds a custom pitch to a track
addPitchCustom
  :: WithCustom TrackChangeRate Float
  -> TrackPitchDirection
  -> RacingTrack
  -> RacingTrack
addPitchCustom pitch dir = trackMap (map func)
  where
    func = setRoadLinePitchRate
      ( trackPitchDir dir
      ( fromCustom trackPitchValue pitch))

-- | Adds a hill to a track
addHill :: TrackChangeRate -> TrackDirection -> RacingTrack -> RacingTrack
addHill = addHillCustom . Common

-- | Adds a custom hill to a track
addHillCustom
  :: WithCustom TrackChangeRate Float
  -> TrackDirection
  -> RacingTrack
  -> RacingTrack
addHillCustom hill dir racingTrack = case dir of
  Straight -> racingTrack
  _        -> trackMap (getTrack trackA ++) trackB
  where
    directedTrack = trackMapWith
      (setRoadDirection dir) racingTrack

    (a, b) = splitHalves (getTrack directedTrack)

    dirs = case dir of
      GoingUp -> (PitchIncreasing, PitchDecreasing)
      _       -> (PitchDecreasing, PitchIncreasing)

    trackA = addPitchCustom hill (fst dirs) (RacingTrack a)
    trackB = addPitchCustom hill (snd dirs) (RacingTrack b)
