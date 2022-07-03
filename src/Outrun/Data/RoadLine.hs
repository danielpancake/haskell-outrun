module Outrun.Data.RoadLine (module Outrun.Data.RoadLine) where
import           Data.Function
import           Data.List
import           Graphics.Gloss
import           Outrun.Data
import           Outrun.Data.RoadObject

-- | Data type used to construst racing tracks
data RoadLine = RoadLine
  { roadLineIndex     :: Int     -- Index of the road line
  , roadLinePosition  :: PointR3 -- World position of the line

    -- Both curve and pitch values make the camera follow the movement,
    -- whereas roadline height does not
  , roadLineCurveRate :: Float -- Curve rate
  , roadLinePitchRate :: Float -- Pitch rate
  , roadDirection     :: TrackDirection -- Direction of the road line

  , roadLineWidth     :: Float -- Width of the road segment (horizontally)
  , roadLineLength    :: Float -- Length of the road segment (vertically)
  , roadLineColor     :: Color -- Color of the road segment

    -- Position of the static road objects is relative
    -- to the road line position
    -- Z-coordinate cannot be negative or exceed the road line length!
  , roadObjects       :: [RoadObject] -- Objects on the road segment
  }

setRoadLineIndex :: Int -> RoadLine -> RoadLine
setRoadLineIndex i rl = rl { roadLineIndex = i }

setRoadLinePosition :: PointR3 -> RoadLine -> RoadLine
setRoadLinePosition pos rl = rl { roadLinePosition = pos }

setRoadLineCurveRate :: Float -> RoadLine -> RoadLine
setRoadLineCurveRate c rl = rl { roadLineCurveRate = c }

setRoadLinePitchRate :: Float -> RoadLine -> RoadLine
setRoadLinePitchRate p rl = rl { roadLinePitchRate = p }

setRoadDirection :: TrackDirection -> RoadLine -> RoadLine
setRoadDirection d rl = rl { roadDirection = d }

setRoadLineColor :: Color -> RoadLine -> RoadLine
setRoadLineColor c rl = rl { roadLineColor = c }

shiftRoadLineIndex :: Int -> RoadLine -> RoadLine
shiftRoadLineIndex delta rl =
  setRoadLineIndex index rl
  where
    index = roadLineIndex rl + delta

shiftRoadLinePosition :: PointR3 -> RoadLine -> RoadLine
shiftRoadLinePosition delta rl =
  setRoadLinePosition pos rl
  where
    pos = shiftPos3D delta (roadLinePosition rl)

-- | Adds a road object to the road line
-- | and perform sorting by z-coordinate
addRoadObject :: RoadObject -> RoadLine -> RoadLine
addRoadObject obj rl =
  rl { roadObjects = sortRoadObjects (obj : roadObjects rl) }

-- | Sorts a list of road objects by their z-coordinate
sortRoadObjects :: [RoadObject] -> [RoadObject]
sortRoadObjects =
  sortBy (compare `on` (getZR3 . roadObjectPosition))

-- | Modificator types for the road line
data TrackLength         = ShortTrack | NormalTrack | LongTrack
data TrackChangeRate     = Gently | Moderately | Steeply
data TrackCurveDirection = TurningLeft | TurningRight
data TrackPitchDirection = PitchDecreasing | PitchIncreasing
data TrackDirection      = Straight | GoingUp | GoingDown

-- Value bindings for the types above
trackLengthValue :: TrackLength -> Int
trackLengthValue length = case length of
  ShortTrack  -> 200
  NormalTrack -> 450
  LongTrack   -> 600

trackCurveValue :: TrackChangeRate -> Float
trackCurveValue curve = case curve of
  Gently     -> 0.02
  Moderately -> 0.05
  Steeply    -> 0.1

trackPitchValue :: TrackChangeRate -> Float
trackPitchValue pitch = case pitch of
  Gently     -> 5
  Moderately -> 8
  Steeply    -> 10

trackCurveDir :: Num a => TrackCurveDirection -> (a -> a)
trackCurveDir dir = case dir of
  TurningLeft  -> negate
  TurningRight -> id

trackPitchDir :: Num a => TrackPitchDirection -> (a -> a)
trackPitchDir dir = case dir of
  PitchDecreasing -> negate
  PitchIncreasing -> id
