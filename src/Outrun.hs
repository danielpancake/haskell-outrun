module Outrun (module Outrun) where
import           Data.Maybe
import           Fonts
import           Graphics.Gloss
import           Numeric
import           Outrun.Building
import           Outrun.Data
import           Outrun.Data.AssetLibrary
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
    background <> game <> stats <> blackFrame
  )
  where
    GameState assets _ cam track player _ = state
    cameraRes = cameraResolution cam

    (cw, ch)   = fromIntegralPair cameraRes
    (sw, __)   = fromIntegralPair screenRes

    scaleFactor = sw / cw

    backPic = getSprite (fetchSpriteFromLibrary "background" assets)

    background = tripleStripPic cw (
      translate (cameraParalax cam) 80 backPic)
    game  = drawRacingTrack cam segmentDrawers track [player]
    stats = drawStats font state

    blackRect = rectangleSolid cw 40
    blackFrame =
      translate 0 (ch/2 - 20) blackRect <>
      translate 0 (20 - ch/2) blackRect

-- | Draw the stats of the game
drawStats :: Font -> OutrunGameState -> Picture
drawStats font state =
  speedometer <> timer <> lapsMeter
  where
    player = gamePlayer state

    metrics = gameMetrics state
    initialTime = gameInitialTime metrics
    time = gameTime metrics
    laps = gameLaps metrics

    -- Drawing a speedometer
    playerSpeed = round (snd (roadObjectVelocity player))

    speedometer = translate 200 120 (scale 2 2 (
      translate   25  (-10) speedLabel <>
      translate (-25) (-10) speedValue <>
      speedTitle))
      where
        draw = labelWithFont font

        speedValue = getLabel (draw (show playerSpeed))

        speedLabel = getLabel (
          labelAlignment
            RightAlign
            BottomAlign
            (draw "km/h"))

        speedTitle = getLabel (
          labelAlignment
            CenterAlign
            BottomAlign
            (labelWithFontExt font afr32_cyan 1.55 1 1 "SPEED"))

    -- Drawing a timer
    timer = translate 0 120 (scale 2 2 (timerTitle <> timerValue))
      where
        draw = labelWithFontExt font afr32_turmeric

        timerTitle = getLabel (
          labelAlignment
            CenterAlign
            BottomAlign
            (draw 1.55 1 1 "TIME"))

        timerValue = getLabel (
          labelAlignment
            CenterAlign
            TopAlign
            (draw scale scale 0.5 timeCounter))

            where
              countDown = (initialTime - time) < 10

              scale = (fromIntegral (fromEnum countDown) + 2) / 2

              timeCounter = showFFloat
                ( Just (fromEnum countDown)
                ) (initialTime - time) ""

    -- Lap'o'meter
    lapsMeter = translate (-200) 120 (scale 2 2 (
      translate   25  (-10) lapsLabel <>
      translate    0  (-10) lapsSlash <>
      translate (-25) (-10) lapsValue <>
      lapsTitle))
      where
        draw = labelWithFontExt font afr32_olive 1 1 0.5

        lapsValue = getLabel (draw (show laps))

        lapsSlash = getLabel
          (labelAlignment CenterAlign BottomAlign (draw "/"))

        lapsLabel = getLabel (
          labelAlignment
            RightAlign
            BottomAlign
            (draw "10"))

        lapsTitle = getLabel (
          labelAlignment
            CenterAlign
            BottomAlign
            (labelWithFontExt font afr32_olive 1.55 1 1 "LAPS"))

outrunPlay
  :: (Int, Int)
  -> AssetLibrary
  -> Font
  -> RacingTrack
  -> IO ()
outrunPlay resolution assets font track =

  play FullScreen afr32_hippieblue 60 initState
    (drawGame resolution [terrainPic, roadSegment] font)
    handleInput updateGame

  where
    player = Dynamic (RoadObject (0, 0, 1500) blank) (0, 0)

    initState =
      GameState assets []
      defaultCamera (infRacingTrack track)
      player
      (Metrics 70 (getTrackLength track) 0 1)

    terrainPic near far =
      drawTerrainSegment (600, 400) px
        ( if even index
          then afr32_olive
          else afr32_sungrass
        ) near far
      where
        index = roadLineIndex (fromProjected near)
        px = (getXR3 . roadObjectPosition . getRoadObject) player

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
    trackRoadColors = [ afr32_darkdorado ]

addRoadObjectsTest :: Picture -> RacingTrack -> RacingTrack
addRoadObjectsTest pic = trackMapWith
  (\rl ->
    case roadLineIndex rl `mod` 10 of
      0 -> addRoadObject testObj rl
      _ -> rl
  )
  where
    testObj = RoadObject (-2000, 250, 80) pic
