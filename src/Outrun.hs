module Outrun (module Outrun) where

-- | This module contains all essential functions
-- | for the Outrun game

import           Data.List
import           Data.List.HT
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           OutrunTypes
import           Utils

makeColor8 :: Int -> Int -> Int -> Int -> Color
makeColor8 r g b a =
  makeColor
    (fromIntegral r / 255)
    (fromIntegral g / 255)
    (fromIntegral b / 255)
    (fromIntegral a / 255)

---- | Projecting Section |-------------------------------------

-- | Projecting a point from the world to the screen
project
  :: Camera -- Camera to use for projection
  -> Pos3D  -- Position of the projected object
  -> (Point, Float) -- On screen coordinates and project scale factor
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

projectRoadLine :: Camera -> RoadLine -> Projected RoadLine
projectRoadLine cam roadline = Projected roadline point scale
  where
    (point, scale) = project cam (roadLinePosition roadline)

projectRoadLines :: Camera -> [RoadLine] -> [Projected RoadLine]
projectRoadLines = projectRoadLines' (0, 0, 0, 0)
  where
    projectRoadLines' _ _ [] = []
    projectRoadLines' (x, dx, y, dy) cam (line : rest) =
      case cameraRenderDistance cam of

        0 -> [] -- Zero render distance

        _ -> projectRoadLines' (x', dx', y', dy') cam' rest ++ [lineProj]
          where
            -- Applying change in curve and pitch values
            x' = x + dx
            y' = y + dy

            dx' = dx + roadLineCurveRate line
            dy' = dy + roadLinePitchRate line

            -- Projecting road line
            lineProj = shiftProjected (x, 0)
              (projectRoadLine cam (shiftRoadLine (0, y, 0) line))

            -- Reducing number of road lines to render
            rendDist = cameraRenderDistance cam
            cam' = changeRenderDistance (rendDist - 1) cam

projectRoadObject
  :: Projected RoadLine -- Nearest road line behind the object
  -> Projected RoadLine -- Nearest road line after the object
  -> RoadObject
  -> Projected RoadObject
projectRoadObject near far roadObject =
  Projected roadObject projPoint scale
  where
    getPosRL = roadLinePosition . fromProjected

    -- Getting the position of the road object
    (objX, _, objZ) = roadObjectPosition roadObject

    -- Getting the position of the two nearest road lines
    -- ahead and behind the road object
    (nearX, nearY) = projectedPosition near
    (farX,  farY)  = projectedPosition far

    nearZ = getZ (getPosRL near)
    farZ  = getZ (getPosRL far)

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
    scale = nearScale + (farScale - nearScale) * projRatio

    -- Point on the screen where the road object is
    projPoint = (
        nearX + projRatio * (farX - nearX) + objX * scale,
        nearY + projRatio * (farY - nearY)
      )

----------------------------------------------------------------


---- | Drawing Section |----------------------------------------

type SegmentPic
  =  Point -- Bottom point of the road segment
  -> Float -- and half of its width
           -- ('cause segment is drawn symmetrically)
  -> Point -- Top point of the road segment
  -> Float -- and half of its width
  -> Color -- Color of the road segment
  -> Picture

type IndexedSegmentPic = Int -> SegmentPic

-- | Draws a road segment
drawRoadSegment :: SegmentPic
drawRoadSegment (x1, y1) w1 (x2, y2) w2 col =
  color col (polygon points)
  where
    points =
      [ (x1 - w1, y1), (x1 + w1, y1),
        (x2 + w2, y2), (x2 - w2, y2)
      ]

-- | Draws a road segment scaled
drawRoadSegmentScaled :: Float -> SegmentPic
drawRoadSegmentScaled scale p1 w1 p2 w2 =
  drawRoadSegment p1 (w1 * scale) p2 (w2 * scale)

drawTerrainSegment
  :: Point -- Bottom point of the terrain segment
  -> Point -- Top point of the terrain segment
  -> Color -- Color of the terrain segment
  -> Picture
drawTerrainSegment (x, y) p2 col =
  -- Drawing the sides of the terrain
  drawRoadSegment (x, -1000) 10000 p2 10000 (dark col)

  -- Drawing the terrain road segment
  <> drawRoadSegment (x, y) 10000 p2 10000 col

drawProjectedObject :: Projected RoadObject -> Picture
drawProjectedObject (Projected obj point factor) =
  uncurry translate point $
  scale factor factor $
  roadObjectPicture obj

-- | Draws a road track
drawRacingTrack
  -- How to draw:
  :: Camera -- Camera to use for projection
  -> [IndexedSegmentPic] -- List of segment drawer functions
  -- with the index of the segment as first argument

  -- What to draw:
  -> RacingTrack         -- The track to draw
  -> [DynamicRoadObject] -- List of road objects to draw

  -> Picture
drawRacingTrack cam segmentDrawers track roadObjs =
  pictures (mapAdjacent drawAllBetween trackProj)
  where
    trackProj = projectRoadLines cam track

    getIndex = roadLineIndex . fromProjected
    getCol   = roadLineColor . fromProjected
    getPos   = roadLinePosition . fromProjected
    getPoint = projectedPosition
    getScale = projectedScale
    getWidth = roadLineWidth . fromProjected

    objsBetween near far =
      filter (\obj -> objZ obj `inRangeOf` (nearZ, farZ)) roadObjs
      where
        objZ = getZ . roadObjectPosition . roadObject

        nearZ = getZ (getPos near)
        farZ  = getZ (getPos far)

    -- | Road list starts with the farthest segments,
    --   since Painter's algorithm  is applied
    drawAllBetween far near = roadSegment <> drawnRoadObjects
      where
        nearP = getPoint near
        farP  = getPoint far

        nearW = getWidth near * getScale near
        farW  = getWidth far * getScale far

        color = getCol near
        index = getIndex near

        -- Drawing road segment between two road lines (could include terrain segement)
        roadSegment =
          pictures (map (\f -> f index nearP nearW farP farW color) segmentDrawers)

        -- Drawing road objects between two road lines
        drawnRoadObjects = pictures $
          map
            ((drawProjectedObject . projectRoadObject near far) . roadObject)
            (objsBetween near far)

----------------------------------------------------------------


---- | Track Building Section |---------------------------------

-- | Makes a track given its length, curve rate and a list of alternating colors
makeTrack :: TrackLength -> [Color] -> RacingTrack
makeTrack trackLength = makeTrackCustom (Common trackLength)

-- | Makes a track with customizable length, curve rate and a list of alternating colors
makeTrackCustom
  :: WithCustom TrackLength Int
  -> [Color]
  -> RacingTrack
makeTrackCustom trackLength cols = take len (infRacingTrack track)
  where
    colors = case cols of
      [] -> [green, dark green]
      _  -> cols

    len = fromCustom trackLengthValue trackLength

    track = foldr
      (connectTracks
      . singleton
      . RoadLine 0 (0, 0, 0) 0 0 defaultRoadSegmentWidth
      ) [] colors

-- | Connects two tracks together
connectTracks :: RacingTrack -> RacingTrack -> RacingTrack
connectTracks track =
  (track ++) . shiftTrack (0, dy, dz) . shiftTrackIndices len
  where
    len = length track

    -- Getting the y coordinate of the last road line of the first track
    -- and lifting second track by this amount
    dy = case lastMaybe track of
      Nothing                             -> 0
      Just (RoadLine _ (_, y, _) _ _ _ _) -> y

    dz = fromIntegral len * defaultRoadSegmentLength

-- | Concatenates a list of tracks together
connectTracksMany :: [RacingTrack] -> RacingTrack
connectTracksMany = foldr connectTracks []

-- | Returns indexed track
indexedTrack :: RacingTrack -> RacingTrack
indexedTrack = (<*> [0..]) . map (flip setRoadLineIndex)

-- | Generates infinite racing track from the given one
infRacingTrack :: RacingTrack -> RacingTrack
infRacingTrack track = connectTracks track (infRacingTrack track)

-- | Adds a curve to a track
addCurve :: TrackChangeRate -> TrackCurveDirection -> RacingTrack -> RacingTrack
addCurve = addCurveCustom . Common

-- | Adds a custom curve to a track
addCurveCustom
  :: WithCustom TrackChangeRate Float
  -> TrackCurveDirection
  -> RacingTrack
  -> RacingTrack
addCurveCustom curve dir =
  map (changeRoadLineCurveRate (trackCurveDir dir (fromCustom trackCurveValue curve)))

-- | Adds a pitch to a track
addPitch :: TrackChangeRate -> TrackPitchDirection -> RacingTrack -> RacingTrack
addPitch = addPitchCustom . Common

-- | Adds a custom pitch to a track
addPitchCustom
  :: WithCustom TrackChangeRate Float
  -> TrackPitchDirection
  -> RacingTrack
  -> RacingTrack
addPitchCustom pitch dir =
  map (changeRoadLinePitchRate (trackPitchDir dir (fromCustom trackPitchValue pitch)))

----------------------------------------------------------------


---- | Input Proccessing Section |------------------------------

-- | Stores a list of the pressed keys
keyboardInput :: Event -> [Key] -> [Key]
keyboardInput ev keys = case ev of
  EventKey key Down _ _ -> if key `elem` keys
    then keys else key : keys -- Adding key to the list of pressed keys

  EventKey key Up   _ _ -> if key `elem` keys
    then delete key keys else keys -- Removing key from the list of pressed keys

  _                     -> keys -- No change

handleInput :: Event -> OutrunGameState -> OutrunGameState
handleInput ev state = case ev of
  EventKey key _ _ _ -> state { gameInput = keyboardInput ev (gameInput state) }
  _                  -> state -- No change

updateGame :: Float -> OutrunGameState -> OutrunGameState
updateGame dt state = updatedGameState
  where
    -- Returns numerical value of the key pressed
    -- If given key is pressed, returns 1, otherwise 0
    checkPressed x = fromEnum (Char x `elem` pressedkeys)

    iUp    = checkPressed 'w'
    iDown  = checkPressed 's'
    iRight = checkPressed 'd'
    iLeft  = checkPressed 'a'

    GameState pressedkeys cam track (Dynamic player (spdX, spdZ)) = state

    (cx, cy, cz) = cameraPosition cam
    (px, __, pz) = roadObjectPosition player

    -- Getting road line nearest to the player from behind
    (playerRL : _) = dropWhile ((< pz) . getZ . roadLinePosition) track

    curveRate = roadLineCurveRate playerRL
    pitchRate = roadLinePitchRate playerRL

    -- Shows whether the player is on the track or not
    isOnTrack = (roadLineWidth playerRL - abs px) >= 0

    -- Get x- and z-direcred movement scalar
    acc         = if isOnTrack then 0.0025 else 0.01
    centrifugal = 0.3
    maxZSpeed   = if isOnTrack then 320 else 50
    maxXSpeed   = 200

    ddx = maxXSpeed * fromIntegral (iRight - iLeft)
    ddz = maxZSpeed * fromIntegral (iUp - iDown)

    spdX' = approach spdX ddx 20 - ddz * curveRate * centrifugal
    spdZ' = max 0 (approachSmooth spdZ ddz acc - pitchRate / 100)
    --      ^^^ prohibits player from going backwards

    -- Retuns value
    dxClamp = let
        off = defaultRoadSegmentWidth * 2
        bound = roadLineWidth playerRL + off
      in clamp (-bound) bound px - px

    -- Update player position
    updatedPlayer =
      shiftRoadObject (spdX' + dxClamp, 0, spdZ') player

    -- Update camera position
    cameraSmoothness = 0.5
    cameraZOffset = 1000

    cdx = (px - cx) * cameraSmoothness
    cdz = (pz - cameraZOffset - cz) * cameraSmoothness

    updatedCamera = shiftCamera (cdx, 0, cdz) cam

    -- Drop all lines behind the camera
    updatedTrack = dropWhile ((< cz) . getZ . roadLinePosition) track

    updatedGameState = state
      { gameRacingTrack = updatedTrack
      , gameCamera      = updatedCamera
      , gamePlayer      = Dynamic updatedPlayer (spdX', spdZ')
      }

----------------------------------------------------------------


---- | Debug Section |------------------------------------------

sampleTrack :: RacingTrack
sampleTrack = connectTracksMany
  [
    makeTrack ShortTrack cols,
    addPitch Moderately GoingUp (makeTrack ShortTrack cols),
    addPitch Steeply GoingDown (makeTrack LongTrack cols),
    makeTrack ShortTrack cols
  ]
  where
    cols = [makeColor8 90 80 80 255, makeColor8 90 80 80 255,
            makeColor8 85 70 70 255, makeColor8 85 70 70 255]


outrunDisplayTest :: RacingTrack -> IO ()
outrunDisplayTest track =
  display window white (drawRacingTrack cam [] track [])
  where
    cam    = Camera (0, 1500, 0) (1024, 768) 0.3 100 0.75 0
    window = InWindow "Debug -- sample track" (1024, 768) (0, 0)


outrunPlayTest :: RacingTrack -> IO ()
outrunPlayTest track =
  play window white 60 initState drawGame handleInput updateGame
  where
    cam    = Camera (0, 1000, 0) (1024, 768) 0.4 100 0.75 0
    window = InWindow "Debug -- sample track ride" (1024, 768) (0, 0)

    initState = GameState [] cam (infRacingTrack track) player

    playerPic = translate 0 500 $ color red $ circle 500
    player = Dynamic (RoadObject (0, 0, 1500) playerPic) (0, 0)

    terrainPic index nearP nearW farP farW col =
      drawTerrainSegment nearP farP (
        if even index then dark green else green
      )

    roadSegment index nearP nearW farP farW col =
      drawRoadSegmentScaled 1.2 nearP nearW farP farW
          (
            if (index `mod` 8) `inRangeOf` (0, 3)
              then white
              else red
          )
      <>
      drawRoadSegment nearP nearW farP farW col
      <>
      if (index `mod` 16) `inRangeOf` (0, 8)
        then drawRoadSegmentScaled 0.05 nearP nearW farP farW white
        else blank

    drawGame :: OutrunGameState -> Picture
    drawGame (GameState _ cam track player) =
      drawRacingTrack cam [roadSegment] track [player]
      <>
      text (show (roadObjectVelocity player))
