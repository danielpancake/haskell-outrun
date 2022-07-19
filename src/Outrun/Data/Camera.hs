module Outrun.Data.Camera (module Outrun.Data.Camera) where
import           Outrun.Data

data Camera = Camera
  { cameraPosition       :: PointR3    -- ^ Position in world space.
  , cameraResolution     :: (Int, Int) -- ^ Camera resolution
  , cameraDepth          :: Float      -- ^ Camera depth. Basically, has
    -- an inversely proportional relation to FOV. May be increased while
    -- moving up, decreased while moving down for the accelerating effect

  , cameraRenderDistance :: Int   -- ^ Render distance
  , cameraSmoothness     :: Float -- ^ How fast camera moves towards target position
  , cameraParallax        :: Float -- ^ Parallax value. Offset of the background
    -- Should be increased when turning right, decreased when turning left
  }

setCameraPosition :: PointR3 -> Camera -> Camera
setCameraPosition p c = c { cameraPosition = p }

setCameraDepth :: Float -> Camera -> Camera
setCameraDepth d cam = cam { cameraDepth = d }

setRenderDistance :: Int -> Camera -> Camera
setRenderDistance d cam = cam { cameraRenderDistance = d }

setCameraParallax :: Float -> Camera -> Camera
setCameraParallax p cam = cam { cameraParallax = p }

shiftCameraPosition :: PointR3 -> Camera -> Camera
shiftCameraPosition delta cam =
  setCameraPosition pos cam
  where
    pos = shiftPos3D (cameraPosition cam) delta

shiftCameraParallax :: Float -> Camera -> Camera
shiftCameraParallax delta cam =
  setCameraParallax parallax cam
  where
    parallax = cameraParallax cam + delta
