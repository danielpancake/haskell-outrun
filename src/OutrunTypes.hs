module OutrunTypes (module OutrunTypes) where

import           Data.Tuple.HT
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

shiftPoint :: Point -> Point -> Point
shiftPoint (x, y) (x', y') = (x + x', y + y')

type Pos3D = (Float, Float, Float)

getX :: Pos3D -> Float
getX = fst3

getY :: Pos3D -> Float
getY = snd3

getZ :: Pos3D -> Float
getZ = thd3

shiftPosition :: Pos3D -> Pos3D -> Pos3D
shiftPosition (x, y, z) (x', y', z') = (x + x', y + y', z + z')

-- | Data type for the projected objects
data Projected a = Projected
  { fromProjected     :: a
  , projectedPosition :: Point
  , projectedScale    :: Float
  }

shiftProjected :: Point -> Projected a -> Projected a
shiftProjected delta (Projected a p s) =
  Projected a (shiftPoint p delta) s

data RoadLine = RoadLine
  { roadLineIndex     :: Int   -- Index of the road line
  , roadLinePosition  :: Pos3D -- World position of the line

    -- Both curve and pitch values make the camera follow the movement,
    -- whereas roadline height does not
  , roadLineCurveRate :: Float -- Curve rate
  , roadLinePitchRate :: Float -- Pitch rate

  , roadLineWidth     :: Float -- Width of the road segment (horizontally)
  , roadLineColor     :: Color -- Color of the road segment
  }

setRoadLineIndex :: Int -> RoadLine -> RoadLine
setRoadLineIndex i rl = rl { roadLineIndex = i }

shiftRoadLineIndex :: Int -> RoadLine -> RoadLine
shiftRoadLineIndex delta rl = setRoadLineIndex newIndex rl
  where
    newIndex = roadLineIndex rl + delta

defaultRoadSegmentLength = 250  :: Float
defaultRoadSegmentWidth  = 2000 :: Float

shiftRoadLine :: Pos3D -> RoadLine -> RoadLine
shiftRoadLine delta roadline =
  roadline { roadLinePosition = newPos }
  where
    newPos = shiftPosition delta (roadLinePosition roadline)

type RacingTrack = [RoadLine]

-- | Shifts the track by the given amount
shiftTrack :: Pos3D -> RacingTrack -> RacingTrack
shiftTrack delta = map (shiftRoadLine delta)

shiftTrackIndices :: Int -> RacingTrack -> RacingTrack
shiftTrackIndices delta = map (shiftRoadLineIndex delta)

data Camera = Camera
  { cameraPosition       :: Pos3D      -- Position in world space
  , cameraResolution     :: (Int, Int) -- Screen resolution
  , cameraDepth          :: Float      -- Camera depth. Basically, has
    -- an inversely proportional relation to FOV. Should be increased while
    -- moving up, decreased while moving down

  , cameraRenderDistance :: Int   -- Render distance
  , cameraSmoothness     :: Float -- How fast camera moves towards target position
  , cameraParalax        :: Float -- Paralax factor. How much the camera is
    -- offset from the road. Should be increased while turning right, decreased
    -- while turning left
  }

changeRenderDistance :: Int -> Camera -> Camera
changeRenderDistance renderDist cam = cam {cameraRenderDistance = renderDist}

shiftCamera :: Pos3D -> Camera -> Camera
shiftCamera delta cam =
  cam { cameraPosition = newPos }
  where
    newPos = shiftPosition (cameraPosition cam) delta

data TrackLength = ShortTrack | NormalTrack | LongTrack

trackLengthValue :: TrackLength -> Int
trackLengthValue length = case length of
  ShortTrack  -> 50
  NormalTrack -> 150
  LongTrack   -> 300

data TrackChangeRate = Gently | Moderately | Steeply

trackCurveValue :: TrackChangeRate -> Float
trackCurveValue curve = case curve of
  Gently     -> 0.1
  Moderately -> 0.5
  Steeply    -> 1

changeRoadLineCurveRate :: Float -> RoadLine -> RoadLine
changeRoadLineCurveRate curve roadLine =
  roadLine { roadLineCurveRate = curve }

trackPitchValue :: TrackChangeRate -> Float
trackPitchValue pitch = case pitch of
  Gently     -> 1
  Moderately -> 5
  Steeply    -> 10

changeRoadLinePitchRate :: Float -> RoadLine -> RoadLine
changeRoadLinePitchRate pitch roadLine =
  roadLine { roadLinePitchRate = pitch }

data TrackCurveDirection = TurningLeft | TurningRight

trackCurveDir :: Num a => TrackCurveDirection -> (a -> a)
trackCurveDir dir = case dir of
  TurningLeft  -> negate
  TurningRight -> id

data TrackPitchDirection = GoingUp | GoingDown

trackPitchDir :: Num a => TrackPitchDirection -> (a -> a)
trackPitchDir dir = case dir of
  GoingDown -> negate
  GoingUp   -> id

-- | Hills are elevations of the road segments height
data TrackHill = SmallHill | MediumHill | LargeHill

trackHillHeight :: TrackHill -> Float
trackHillHeight hill = case hill of
  SmallHill  -> 1000
  MediumHill -> 5000
  LargeHill  -> 8000

trackHillDir :: Num a => TrackPitchDirection -> (a -> a)
trackHillDir dir = case dir of
  GoingDown -> (1 -) -- InterpolationFunc takes values from 0 to 1
                     -- For that reason, inversion is done that way
  GoingUp   -> id

data OutrunGameState = GameState
  { gameInput       :: [Key]
  , gameCamera      :: Camera
  , gameRacingTrack :: RacingTrack
  , gamePlayer      :: DynamicRoadObject
  }

data RoadObject = RoadObject
  { roadObjectPosition :: Pos3D
  , roadObjectPicture  :: Picture
  }

shiftRoadObject :: Pos3D -> RoadObject -> RoadObject
shiftRoadObject delta roadObject =
  roadObject { roadObjectPosition = newPos }
  where
    newPos = shiftPosition delta (roadObjectPosition roadObject)

data DynamicRoadObject = Dynamic
  { roadObject         :: RoadObject
  , roadObjectVelocity :: Point
  }
