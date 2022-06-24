module Outrun (module Outrun) where

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

        _ -> projectRoadLines' (x', dx', y', dy') cam' rest ++ [lineProj']
          where
            -- Applying change in curve and pitch values
            x' = x + dx
            y' = y + dy

            dx' = dx + roadLineCurveRate line
            dy' = dy + roadLinePitchRate line

            -- Projecting road line
            lineProj = projectRoadLine cam (shiftRoadLine (0, y, 0) line)
            lineProj' = lineProj {
              projectedPosition =
                shiftPoint (x, 0) (projectedPosition lineProj)
              }

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

    objZ  = getZ (roadObjectPosition roadObject)
    nearZ = getZ (getPosRL near)
    farZ  = getZ (getPosRL far)

    projRatio = (objZ - nearZ) / (farZ - nearZ)

    (nearX, nearY) = projectedPosition near
    (farX,  farY)  = projectedPosition far

    nearScale = projectedScale near
    farScale  = projectedScale far

    projPoint = (nearX + projRatio * (farX - nearX),
                 nearY + projRatio * (farY - nearY))

    scale = nearScale + (farScale - nearScale) * projRatio

----------------------------------------------------------------


---- | Drawing Section |----------------------------------------

-- | Draws a road segment
drawRoadSegment
  :: Point -- Bottom point of the road segment
  -> Float -- and its width
  -> Point -- Top point of the road segment
  -> Float -- and its width
  -> Color -- Color of the road segment
  -> Picture
drawRoadSegment (x1, y1) w1 (x2, y2) w2 col =
  color col (polygon points)
  where
    points =
      [ (x1 - w1, y1), (x1 + w1, y1),
        (x2 + w2, y2), (x2 - w2, y2)
      ]

-- | Draws a road segment scaled
drawRoadSegmentScaled
  :: Float -- Scale factor
  -> Point -- Bottom point of the road segment
  -> Float -- and its width
  -> Point -- Top point of the road segment
  -> Float -- and its width
  -> Color -- Color of the road segment
  -> Picture
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
  :: Camera       -- Camera to use for projection
  -> [Color]      -- List of colors to use for the terrain
  -> RacingTrack  -- The track to draw
  -> [DynamicRoadObject]
  -> Picture
drawRacingTrack cam _ track roadObjs = let

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
      objZ = (getZ . roadObjectPosition . roadObject)

      nearZ = getZ (getPos near)
      farZ  = getZ (getPos far)

  -- | Since Painter Method is applied, road list starts with the farthest segments
  drawAllBetween far near = terrain <> roadSegment <> drawnRoadObjects
    where
      nearP = getPoint near
      farP  = getPoint far

      nearW = getWidth near * getScale near
      farW  = getWidth far * getScale far

      col   = getCol near
      ind   = getIndex near

      -- Drawing terrain segment between two road lines
      terrain =
        drawTerrainSegment nearP farP (
          if even ind then dark green else green
          )

      -- Drawing road segment between two road lines
      roadSegment =
        drawRoadSegmentScaled 1.2 nearP nearW farP farW white
        <>
        drawRoadSegment nearP nearW farP farW col

      -- Drawing road objects between two road lines
      drawnRoadObjects = pictures $
        map (
          (drawProjectedObject . projectRoadObject near far) . roadObject
          ) (objsBetween near far)

  in pictures (mapAdjacent drawAllBetween trackProj)

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
makeTrackCustom trackLength colArr = take length (infRacingTrack track)
  where
    colors = case colArr of
      [] -> [green, dark green]
      _  -> colArr

    length = fromCustom trackLengthValue trackLength

    track = foldr
      (connectTracks
      . (:[])
      . RoadLine 0 (0, 0, 0) 0 0 defaultRoadSegmentWidth
      ) [] colors

-- | Connects two tracks together
connectTracks :: RacingTrack -> RacingTrack -> RacingTrack
connectTracks track =
  (track ++) . shiftTrack (0, dy, dz)
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
    GameState pressedkeys cam track (Dynamic player _) = state
    (_, cy, cz) = cameraPosition cam

    ddz = fromIntegral
      (fromEnum (Char 'w' `elem` pressedkeys) - fromEnum (Char 's' `elem` pressedkeys))

    -- Update camera position
    updatedCamera = shiftCamera (0, 0, ddz * 100) cam
    updatedPlayer = shiftRoadObject (0, 0, ddz * 100) player

    -- Drop all lines behind the camera
    updatedTrack = dropWhile ((< cz) . getZ . roadLinePosition) track

    updatedGameState = state
      { gameRacingTrack = updatedTrack
      , gameCamera      = updatedCamera
      , gamePlayer      = Dynamic updatedPlayer (0, 0)
      }

----------------------------------------------------------------


---- | Debug Section |------------------------------------------

sampleTrack :: RacingTrack
sampleTrack = connectTracksMany
  [
    makeTrack ShortTrack cols,
    addPitch Moderately GoingUp (makeTrack ShortTrack cols),
    addPitch Moderately GoingDown (makeTrack ShortTrack cols),
    makeTrack ShortTrack cols,
    addCurve Moderately TurningLeft (makeTrack ShortTrack cols),
    addCurve Moderately TurningRight (makeTrack ShortTrack cols),
    addCurve Steeply TurningRight (makeTrack ShortTrack cols),
    makeTrack ShortTrack cols
  ]
  where
    cols = [makeColor8 90 80 80 255, makeColor8 85 70 70 255]

outrunDisplayTest :: RacingTrack -> IO ()
outrunDisplayTest track =
  display window white (drawRacingTrack cam [] track [])
  where
    cam    = Camera (0, 1500, 0) (1024, 768) 0.3 100 0.75
    window = InWindow "Debug -- sample track" (1024, 768) (0, 0)

outrunPlayTest :: RacingTrack -> IO ()
outrunPlayTest track =
  play window white 60 initState drawGame handleInput updateGame
  where
    cam    = Camera (0, 1000, 0) (1024, 768) 0.4 200 0.75
    window = InWindow "Debug -- sample track ride" (1024, 768) (0, 0)

    playerPic = translate 0 500 $ color red $ circle 500
    player = Dynamic (RoadObject (0, 0, 1500) playerPic) (0, 0)

    initState = GameState [] cam (infRacingTrack track) player

    drawGame :: OutrunGameState -> Picture
    drawGame (GameState _ cam track player) =
      drawRacingTrack cam
      [makeColor8 0 228 54 1, makeColor8 0 135 81 255]
      track [player]
