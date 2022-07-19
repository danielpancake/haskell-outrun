module Outrun.Rendering (module Outrun.Rendering) where
import           Data.List.HT
import           Graphics.Gloss
import           Outrun.Data
import           Outrun.Data.Camera
import           Outrun.Data.Projected
import           Outrun.Data.RacingTrack
import           Outrun.Data.RoadLine
import           Outrun.Data.RoadObject
import           Outrun.Projecting
import           Utils

-- | Draws polygon
drawPolygonSegment
  :: PointR2 -- Bottom point of the road segment
  -> Float   -- and half of its width
  -> PointR2 -- Top point of the road segment
  -> Float   -- and half of its width
  -> Color   -- Color of the road segment
  -> Picture
drawPolygonSegment (x1, y1) w1 (x2, y2) w2 col =
  color col (polygon points)
  where
    points =
      [ (x1 - w1, y1), (x1 + w1, y1),
        (x2 + w2, y2), (x2 - w2, y2)
      ]

-- | Type for functions that draw polygon
-- between two road lines
type SegmentPic
  =  Projected RoadLine
  -> Projected RoadLine
  -> Picture

-- | Draws a road segment
drawRoadSegment :: SegmentPic
drawRoadSegment near = drawRoadSegmentExt 1 col near
  where
    col = roadLineColor (fromProjected near)

-- | Draws a road segment with custom scale and color
drawRoadSegmentExt :: Float -> Color -> SegmentPic
drawRoadSegmentExt scale color near far =
  drawPolygonSegment p1 w1 p2 w2 color
  where
    projectedWidth x =
      scale *
      roadLineWidth (fromProjected x) *
      projectedScale x

    p1  = projectedPosition near
    w1  = projectedWidth near

    p2 = projectedPosition far
    w2 = projectedWidth far

drawTerrainSegment
  :: (Int, Int) -- Draw resolution
  -> Float      -- Player x position
  -> Color
  -> SegmentPic
drawTerrainSegment (screenW, screenH) px col near far =
  drawPolygonSegment (px, p1Y) ww (px, p2Y) ww col
  where
    (_, p1Y) = projectedPosition near
    (_, p2Y) = projectedPosition far

    ww = fromIntegral screenW / 2
    hh = fromIntegral screenH / 2

-- | Draws projected raod object
drawProjectedObject :: Projected RoadObject -> Picture
drawProjectedObject (Projected obj point factor) =
  uncurry translate point $
  scale factor factor $
  roadObjectPicture obj

-- | Draws a road track
drawRacingTrack
  -- How to draw:
  :: Camera -- Camera to use for projection
  -> [SegmentPic] -- List of segment drawer functions
  -- with the index of the segment as first argument

  -- What to draw:
  -> RacingTrack         -- The track to draw
  -> [DynamicRoadObject] -- List of road objects to draw

  -> Picture
drawRacingTrack cam segmentDrawers track roadObjs =
  pictures (mapAdjacent drawAllBetween trackProj)
  where
    getPos = roadLinePosition . fromProjected

    trackProj = projectRoadLines cam (getTrack track)

    -- Returns a list of dynamic road objects between two lines
    objsBetween near far =
      filter (\obj -> objZ obj `inRangeOf` (nearZ, farZ)) roadObjs
      where
        nearZ = getZR3 (getPos near)
        farZ  = getZR3 (getPos far)

        objZ = getZR3 . roadObjectPosition . getRoadObject

    -- | Road list starts with the farthest segments,
    --   since Painter's algorithm is applied
    drawAllBetween far near = roadSegment <> drawnRoadObjects
      where
        -- Drawing road segment between two road lines (could include terrain segement)
        -- Thanks to @fizruk for the advice
        roadSegment = mconcat segmentDrawers near far

        -- Projecting static objects
        staticObjs =
          map (shiftRoadObject (0, 0, zz)) (roadObjects (fromProjected near))
          where
            (_, _, zz) = getPos near

        -- Projecting dynamic objects
        dynamicObjs =
          map getRoadObject (objsBetween near far)

        -- Draw only those objects that are in the viewport
        filteredObjs =
          filter (\obj ->
            getXR2 (projectedPosition obj) `inRangeOf` (-halfCW, halfCW))
          where
            halfCW = fromIntegral (fst (cameraResolution cam)) / 2 + off
            off = 100

        -- Drawing all road objects between two road lines
        drawnRoadObjects = pictures (
            map drawProjectedObject
              ( filteredObjs -- Draw only objects in the camera view
              ( map (projectRoadObject near far)
              ( staticObjs ++ dynamicObjs)))
          )

-- | Makes a triple strip from the image
tripleStripPic :: Float -> Picture -> Picture
tripleStripPic off pic =
  pictures [
    translate (-off) 0 pic,
    pic,
    translate off 0 pic
  ]
