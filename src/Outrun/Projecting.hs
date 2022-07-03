module Outrun.Projecting (module Outrun.Projecting) where
import           Outrun.Data
import           Outrun.Data.Camera
import           Outrun.Data.Projected
import           Outrun.Data.RoadLine
import           Outrun.Data.RoadObject

-- | Projects a point from the world to the screen
project
  :: Camera           -- Camera to use for projection
  -> PointR3          -- Position of the projected object
  -> (PointR2, Float) -- On screen coordinates and projection scale factor
project cam (lx, ly, lz') = (screenPos, scale * halfWidth)
  where
    (cx, cy, cz) = cameraPosition cam

    -- Snapping Z's behind the camera to the relative zero
    lz = if lz' < cz then cz else lz'

    (viewWidth, viewheight) = cameraResolution cam
    depth = cameraDepth cam

    halfWidth = fromIntegral viewWidth / 2
    halfHeight = fromIntegral viewheight / 2

    -- Project the road line onto the screen
    scale = depth / (lz - cz)
    sx = (1 + scale * (lx - cx)) * halfWidth
    sy = (1 + scale * (ly - cy)) * halfHeight

    -- Translate coordinates to the bottom center of the screen
    screenPos = (sx - halfWidth, sy - halfHeight)

-- | Projects road line onto the screen
projectRoadLine :: Camera -> RoadLine -> Projected RoadLine
projectRoadLine cam rl = uncurry
  (Projected rl) (project cam (roadLinePosition rl))

-- | Project whole track onto the screen,
-- | taking into account curve and pitch values
projectRoadLines :: Camera -> [RoadLine] -> [Projected RoadLine]
projectRoadLines = projectRoadLines' (0, 0, 0, 0)
  where
    projectRoadLines' _ _ [] = []
    projectRoadLines' (dx, ddx, dy, ddy) cam (line : rest) =
      case cameraRenderDistance cam of

        0 -> [] -- Zero render distance

        _ -> projectRoadLines' (dx', ddx', dy', ddy') cam' rest ++ [lineProj]
          where
            -- Applying change in curve and pitch values
            dx' = dx + ddx
            dy' = dy + ddy

            ddx' = ddx + roadLineCurveRate line
            ddy' = ddy + roadLinePitchRate line

            -- Projecting road line
            lineProj = shiftProjected (dx, 0)
              (projectRoadLine cam (shiftRoadLinePosition (0, dy, 0) line))

            -- Reducing number of road lines to render
            rendDist = cameraRenderDistance cam
            cam' = setRenderDistance (rendDist - 1) cam

-- | Projects road object placed between two road lines onto the screen
projectRoadObject
  :: Projected RoadLine -- Nearest road line behind the object
  -> Projected RoadLine -- Nearest road line after the object
  -> RoadObject         -- Road object to project
  -> Projected RoadObject
projectRoadObject near far roadObject =
  Projected roadObject projPoint scale
  where
    -- | Local function for getting world position
    -- of the road line
    getPos = roadLinePosition . fromProjected

    -- Getting the position of the road object
    (objX, objY, objZ) = roadObjectPosition roadObject

    nearZ = getZR3 (getPos near)
    farZ  = getZR3 (getPos far)

    -- Calculating the projection ratio
    -- It shows how far the road object is from
    -- the nearest road line behind it (near)
    -- 0 - object is on the near road line
    -- 1 - object is on the far road line
    projRatio = (objZ - nearZ) / (farZ - nearZ)

    nearScale = projectedScale near
    farScale  = projectedScale far

    -- Interpolating the scale of the road object
    -- based on the projection ratio
    scale = nearScale + projRatio * (farScale - nearScale)

    -- Getting the position of the two nearest road lines
    -- ahead and behind the road object
    (nearX, nearY) = projectedPosition near
    (farX , farY ) = projectedPosition far

    -- Point on the screen where the road object is
    projPoint = (
        nearX + projRatio * (farX - nearX) + objX * scale,
        nearY + projRatio * (farY - nearY) + objY * scale
      )
