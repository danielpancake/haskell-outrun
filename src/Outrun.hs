module Outrun (module Outrun) where
import           Fonts
import           Graphics.Gloss
import           Outrun.Building
import           Outrun.Data
import           Outrun.Data.Camera
import           Outrun.Data.Defaults
import           Outrun.Data.GameState
import           Outrun.Data.Projected
import           Outrun.Data.RacingTrack
import           Outrun.Data.RoadLine
import           Outrun.Data.RoadObject
import           Outrun.InputProcessing
import           Outrun.Rendering
import           Palettes
import           Utils

drawGame
  :: (Int, Int)
  -> [SegmentPic]
  -> Font
  -> OutrunGameState
  -> Picture
drawGame screenRes segmentDrawers font state =
  scale scaleFactor scaleFactor (
    back <> game <> stats <> blackFrame
  )
  where
    GameState _ cam track player _ background = state
    cameraRes = cameraResolution cam

    (cw, ch)   = fromIntegralPair cameraRes
    (sw, __)   = fromIntegralPair screenRes

    scaleFactor = sw / cw

    back = tripleStripPic cw (
      translate (cameraParalax cam) 80 background)
    game  = drawRacingTrack cam segmentDrawers track [player]
    stats = drawStats font state

    blackRect = rectangleSolid cw 40
    blackFrame =
      translate 0 (ch/2 - 20) blackRect <>
      translate 0 (20 - ch/2) blackRect

drawStats :: Font -> OutrunGameState -> Picture
drawStats font (GameState _ cam track player _ _) =
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

outrunPlay
  :: Picture
  -> (Int, Int)
  -> Font
  -> RacingTrack
  -> IO ()
outrunPlay background screenRes font track =

  play FullScreen afr32_hippieblue 60 initState
    (drawGame screenRes [terrainPic, roadSegment] font)
    handleInput updateGame

  where
    playerPic = translate 0 100 $ color afr32_red $ circle 200
    player = Dynamic (RoadObject (0, 0, 1500) playerPic) (0, 0)

    px = (getXR3 . roadObjectPosition . getRoadObject) player

    initState =
      GameState [] defaultCamera (infRacingTrack track) player 0 background

    terrainPic near far =
      drawTerrainSegment (600, 400) px
        ( if even index
          then afr32_olive
          else afr32_sungrass
        ) near far
      where
        index = roadLineIndex (fromProjected near)

    roadSegment near far =
      drawRoadSegmentExt 1.2
        ( if (index `mod` 8) `inRangeOf` (0, 3)
          then afr32_white
          else afr32_red
        ) near far
        <>
        drawRoadSegment near far
        <>
        if (index `mod` 16) `inRangeOf` (0, 8)
          then drawRoadSegmentExt 0.05 afr32_white near far
          else blank
      where
        index = roadLineIndex (fromProjected near)

sampleTrack :: RacingTrack
sampleTrack = mconcat (
  map ($ trackRoadColors)
  [
    addRoadObjectsTest . makeTrack LongTrack,
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
    trackRoadColors = [ afr32_darkdorado ]

addRoadObjectsTest :: RacingTrack -> RacingTrack
addRoadObjectsTest = trackMapWith
  (\rl ->
    case roadLineIndex rl `mod` 10 of
      0 -> addRoadObject testObj rl
      _ -> rl
  )
  where
    testObj = RoadObject (-2000, 250, 80) (rectangleSolid 500 500)
