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

data TrackDirection = Straight | GoingUp | GoingDown

data RoadLine = RoadLine
  { roadLineIndex     :: Int   -- Index of the road line
  , roadLinePosition  :: Pos3D -- World position of the line

    -- Both curve and pitch values make the camera follow the movement,
    -- whereas roadline height does not
  , roadLineCurveRate :: Float -- Curve rate
  , roadLinePitchRate :: Float -- Pitch rate
  , roadDirection     :: TrackDirection -- Direction of the road line

  , roadLineWidth     :: Float -- Width of the road segment (horizontally)
  , roadLineColor     :: Color -- Color of the road segment
  }

defaultRoadLine :: RoadLine
defaultRoadLine =
  RoadLine 0 (0, 0, 0) 0 0 Straight defaultRoadSegmentWidth green

setRoadLineIndex :: Int -> RoadLine -> RoadLine
setRoadLineIndex i rl = rl { roadLineIndex = i }

shiftRoadLineIndex :: Int -> RoadLine -> RoadLine
shiftRoadLineIndex delta rl = setRoadLineIndex newIndex rl
  where
    newIndex = roadLineIndex rl + delta

setRoadDirection :: TrackDirection -> RoadLine -> RoadLine
setRoadDirection d rl = rl { roadDirection = d }

setRoadLineColor :: Color -> RoadLine -> RoadLine
setRoadLineColor c rl = rl { roadLineColor = c }

defaultRoadSegmentLength :: Float
defaultRoadSegmentLength = 120
defaultRoadSegmentWidth :: Float
defaultRoadSegmentWidth  = 1000

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

setCameraDepth :: Float -> Camera -> Camera
setCameraDepth depth cam = cam { cameraDepth = depth }

setRenderDistance :: Int -> Camera -> Camera
setRenderDistance dist cam = cam {cameraRenderDistance = dist }

shiftCamera :: Pos3D -> Camera -> Camera
shiftCamera delta cam =
  cam { cameraPosition = newPos }
  where
    newPos = shiftPosition (cameraPosition cam) delta

shiftParalax :: Float -> Camera -> Camera
shiftParalax delta cam =
  cam { cameraParalax = newParalax }
  where
    newParalax = cameraParalax cam + delta

setParalax :: Float -> Camera -> Camera
setParalax paralax cam = cam { cameraParalax = paralax }

data TrackLength = ShortTrack | NormalTrack | LongTrack

trackLengthValue :: TrackLength -> Int
trackLengthValue length = case length of
  ShortTrack  -> 200
  NormalTrack -> 450
  LongTrack   -> 600

data TrackChangeRate = Gently | Moderately | Steeply

trackCurveValue :: TrackChangeRate -> Float
trackCurveValue curve = case curve of
  Gently     -> 0.02
  Moderately -> 0.05
  Steeply    -> 0.1

setRoadLineCurveRate :: Float -> RoadLine -> RoadLine
setRoadLineCurveRate c rl = rl { roadLineCurveRate = c }

trackPitchValue :: TrackChangeRate -> Float
trackPitchValue pitch = case pitch of
  Gently     -> 5
  Moderately -> 8
  Steeply    -> 10

setRoadLinePitchRate :: Float -> RoadLine -> RoadLine
setRoadLinePitchRate p rl = rl { roadLinePitchRate = p }

data TrackCurveDirection = TurningLeft | TurningRight

trackCurveDir :: Num a => TrackCurveDirection -> (a -> a)
trackCurveDir dir = case dir of
  TurningLeft  -> negate
  TurningRight -> id

data TrackPitchDirection
  = PitchDecreasing | PitchIncreasing

trackPitchDir :: Num a => TrackPitchDirection -> (a -> a)
trackPitchDir dir = case dir of
  PitchDecreasing -> negate
  PitchIncreasing -> id

data OutrunGameState = GameState
  { gameInput       :: [Key]
  , gameCamera      :: Camera
  , gameRacingTrack :: RacingTrack
  , gamePlayer      :: DynamicRoadObject
  , gameTime        :: Float
  , gameBackground  :: Picture
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
