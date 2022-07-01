module Outrun (module Outrun) where

-- | This module contains all essential functions
-- | for the Outrun game

import qualified Data.Bifunctor
import           Data.List
import           Data.List.HT
import           Fonts
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           OutrunTypes
import           Utils
import Palettes

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
              (projectRoadLine cam (shiftRoadLine (0, dy, 0) line))

            -- Reducing number of road lines to render
            rendDist = cameraRenderDistance cam
            cam' = setRenderDistance (rendDist - 1) cam

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
  :: (Int, Int) -- Draw resolution
  -> Point -- Bottom point of the terrain segment
  -> Point -- Top point of the terrain segment
  -> Color -- Color of the terrain segment
  -> Picture
drawTerrainSegment (screenW, screenH) p1 p2 col =
  drawRoadSegment p1 ww p2 ww col
  where
    ww = fromIntegral screenW / 2
    hh = fromIntegral screenH / 2

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
      [] -> [afr32_sungrass, afr32_olive]
      _  -> cols

    len = fromCustom trackLengthValue trackLength

    track = foldr
      (connectTracks
      . singleton
      . flip setRoadLineColor
        defaultRoadLine
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
      Nothing       -> 0
      Just roadline -> getY (roadLinePosition roadline)

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
  map (setRoadLineCurveRate (trackCurveDir dir (fromCustom trackCurveValue curve)))

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
addPitchCustom pitch dir =
  map (setRoadLinePitchRate (trackPitchDir dir (fromCustom trackPitchValue pitch)))

-- | Adds a hill to a track
addHill :: TrackChangeRate -> TrackDirection -> RacingTrack -> RacingTrack
addHill = addHillCustom . Common

-- | Adds a custom hill to a track
addHillCustom
  :: WithCustom TrackChangeRate Float
  -> TrackDirection
  -> RacingTrack
  -> RacingTrack
addHillCustom hill dir track =
  case dir of
    GoingUp   ->
         addPitchCustom hill PitchIncreasing a
      ++ addPitchCustom hill PitchDecreasing b
    GoingDown ->
         addPitchCustom hill PitchDecreasing a
      ++ addPitchCustom hill PitchIncreasing b

    _ -> track
  where
    (a, b) = splitHalves (map (setRoadDirection dir) track)

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
    -- Unpacking the state
    GameState pressedkeys cam track (Dynamic player (spdX, spdZ)) = state

    (cx, cy, cz) = cameraPosition cam
    (px, __, pz) = roadObjectPosition player

    -- Drop all lines behind the camera
    updateTrack = dropWhile ((< cz) . getZ . roadLinePosition)

    -- Update camera position
    cameraSmoothness = 0.5
    cameraZOffset = 300

    cdx = (px - cx) * cameraSmoothness
    cdy = 0
    cdz = (pz - cameraZOffset - cz) * cameraSmoothness

    updateCamera = shiftCamera (cdx, cdy, cdz)

    -- Update player position
    horizBound =
      roadLineWidth playerRL + defaultRoadSegmentWidth * 2.5

    outOfBound = abs px > horizBound

    -- Value needed to return player back
    -- on the playable area of the racing track
    horizClamp = clamp (-horizBound) horizBound px - px
    updatePosition = shiftRoadObject (spdX + 2*horizClamp, 0, spdZ)

    -- Returns whether the key is pressed
    checkPressed key = key `elem` pressedkeys

    iUp = fromEnum $
      checkPressed (Char 'w') ||
      checkPressed (SpecialKey KeyUp)
    iDown = fromEnum $
      checkPressed (Char 's') ||
      checkPressed (SpecialKey KeyDown)
    iRight = fromEnum $
      checkPressed (Char 'd') ||
      checkPressed (SpecialKey KeyRight)
    iLeft = fromEnum $
      checkPressed (Char 'a') ||
      checkPressed (SpecialKey KeyLeft)

    -- Getting road line nearest to the player from behind
    (playerRL : _) = dropWhile ((< pz) . getZ . roadLinePosition) track

    curveRate = roadLineCurveRate playerRL
    hillRate  = abs (roadLinePitchRate playerRL) *
      case roadDirection playerRL of
        GoingUp   ->  1
        GoingDown -> -1
        _         ->  0

    -- Shows whether the player is on the track or not
    isOnTrack = (roadLineWidth playerRL - abs px) >= 0

    -- Get x- and z-direcred movement scalar
    acc         = if isOnTrack then 0.0035 else 0.01
    friction    = if isOnTrack then 0.025  else 0.1
    centrifugal = 1
    maxZSpeed   = if isOnTrack then 300 else 120
    maxXSpeed   = 200

    ddx = maxXSpeed * fromIntegral (iRight - iLeft)
    ddz = maxZSpeed * fromIntegral (iUp - iDown)

    spdZRatio = min spdZ maxZSpeed / maxZSpeed

    centrifugalForce =
      1000 * spdZRatio^2 * curveRate * centrifugal

    spdX' = if outOfBound
      then 0
      else approachSmooth spdX (ddx - centrifugalForce) (
          case ddx of
            0 -> 0.1
            _ -> 0.01
        )

    spdZ' = max 0 (approachSmooth spdZ ddz acc - hillRate * friction)
    ------- ^^^ prohibits player from going backwards

    updatedGameState = state
      { gameRacingTrack = updateTrack track
      , gameCamera      = updateCamera cam
      , gamePlayer      =
        Dynamic (updatePosition player) (spdX', spdZ')
      }

drawGame
  :: (Int, Int)
  -> [IndexedSegmentPic]
  -> Font
  -> OutrunGameState
  -> Picture
drawGame screenRes segmentDrawers font state =
  scale scaleFactor scaleFactor (
    game <> stats <> blackFrame
  )
  where
    GameState _ cam track player = state
    cameraRes = cameraResolution cam

    (cw, ch) = fromIntegralPair cameraRes
    (sw, __) = fromIntegralPair screenRes

    game = drawRacingTrack cam segmentDrawers track [player]

    stats = drawStats font state

    scaleFactor = sw / cw

    blackRect = rectangleSolid cw 40
    blackFrame =
      translate 0 (ch/2 - 20) blackRect <>
      translate 0 (20 - ch/2) blackRect

drawStats :: Font -> OutrunGameState -> Picture
drawStats font (GameState _ cam track player) =
  speedometer
  where
    playerSpeed = round (snd (roadObjectVelocity player))

    speedStyle = textWithFontExt font white 1 1 0.5
    speedValue = speedStyle (show playerSpeed)
    speedLabel = translate 24 0 (speedStyle "km/h")
    speedTitle =
      translate 0 10 (
        textWithFontExt font afr32_cyan 1.51 1 1 "SPEED"
      )

    speedometer =
      translate 150 100 $
      scale 2 2 $
      speedValue <> speedLabel <> speedTitle

outrunPlay :: (Int, Int) -> Font -> RacingTrack -> IO ()
outrunPlay screenRes font track =
  play FullScreen afr32_hippieblue 60 initState
  (drawGame screenRes [terrainPic, roadSegment] font)
  handleInput updateGame
  where
    camRes = (600, 400)
    cam = Camera (0, 500, 0) camRes 0.325 100 0.8 0

    playerPic = translate 0 200 $ color afr32_red $ circle 200
    player = Dynamic (RoadObject (0, 0, 1500) playerPic) (0, 0)

    px = (getX . roadObjectPosition . roadObject) player

    initState = GameState [] cam (infRacingTrack track) player

    terrainPic index (_, nearY) _ (_, farY) _ _ =
      drawTerrainSegment (600, 400) (px, nearY) (px, farY) (
        if even index then afr32_olive else afr32_sungrass
      )

    roadSegment index nearP nearW farP farW col =
      drawRoadSegmentScaled 1.2 nearP nearW farP farW
          (
            if (index `mod` 8) `inRangeOf` (0, 3)
              then afr32_white
              else afr32_red
          )
      <>
      drawRoadSegment nearP nearW farP farW col
      <>
      if (index `mod` 16) `inRangeOf` (0, 8)
        then drawRoadSegmentScaled 0.05 nearP nearW farP farW afr32_white
        else blank

----------------------------------------------------------------


---- | Debug Section |------------------------------------------

sampleTrack :: RacingTrack
sampleTrack = connectTracksMany (
  map ($ trackRoadColors)
  [
    makeTrack LongTrack,
    addCurve Gently TurningRight . makeTrack LongTrack,
    makeTrack ShortTrack,
    addCurve Moderately TurningRight . makeTrack LongTrack,
    addCurve Moderately TurningLeft . makeTrack ShortTrack,
    addCurve Moderately TurningRight . makeTrack ShortTrack,
    makeTrack NormalTrack,
    addCurve Moderately TurningRight . makeTrack NormalTrack,
    addCurve Gently TurningLeft . makeTrack ShortTrack,
    makeTrack NormalTrack,
    addCurve Moderately TurningRight . makeTrack NormalTrack,
    addCurve Moderately TurningLeft . makeTrack ShortTrack,
    addCurve Moderately TurningRight . makeTrack ShortTrack,
    addCurve Moderately TurningLeft . makeTrack ShortTrack,
    makeTrack ShortTrack,
    addCurve Steeply TurningRight . makeTrack ShortTrack
  ])
  where
    trackRoadColors =
      [
        afr32_darkdorado
      ]

outrunDisplayTest :: RacingTrack -> IO ()
outrunDisplayTest track =
  display window white (drawRacingTrack cam [] track [])
  where
    cam    = Camera (0, 1500, 0) (600, 400) 0.3 100 0.75 0
    window = InWindow "Debug -- sample track" (960, 540) (0, 0)

drawGameDebug
  :: (Int, Int)
  -> [IndexedSegmentPic]
  -> Font
  -> OutrunGameState
  -> Picture
drawGameDebug (screenW, screenH) segmentDrawers font (GameState _ cam track player) =
  game <> outline
  where
    game = drawRacingTrack cam segmentDrawers track [player]
    (cw, ch) = cameraResolution cam
    outline = color afr32_red $
      rectangleWire (fromIntegral cw) (fromIntegral ch)
