module Outrun (module Outrun) where
import           Data.Fixed
import           Fonts
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Numeric
import           Outrun.Building
import           Outrun.Data
import           Outrun.Data.AssetLibrary
import           Outrun.Data.Camera
import           Outrun.Data.Custom
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
    clouds <> background <> game <> stats <> blackFrame
  )
  where
    GameState assets _ cam track player _ = state
    cameraRes = cameraResolution cam

    (cw, ch) = fromIntegralPair cameraRes
    (sw, __) = fromIntegralPair screenRes

    scaleFactor = sw / cw

    ((backW, backH), backPic) =
      fetchSpriteFromLibraryF "background" assets

    background = translate (mod' (cameraParallax cam) cw) (-5)
      (tripleStripPic cw
        (
          pictureAlign CenterAlign BottomAlign
          backW backH backPic
        )
      )

    ((cloudsW, cloudsH), cloudsPic) =
      fetchSpriteFromLibraryF "clouds" assets
    
    clouds = translate (mod' (cameraParallax cam / 2) cw) 50
      (tripleStripPic cw
        (
          pictureAlign CenterAlign BottomAlign
          cloudsW cloudsH cloudsPic
        )
      )

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

type OutrunGame world
  =  Display
  -> Color
  -> Int
  -> world
  -> (world -> Picture)
  -> (Event -> world -> world)
  -> (Float -> world -> world)
  -> IO ()

resettableGame :: OutrunGame OutrunGameState -> OutrunGame OutrunGameState
resettableGame game display color fps initState drawGame eventHandler updateHandler =
  game display color fps initState drawGame eventHandler resetHandler
  where
    resetHandler dt state =
      if gameTime (gameMetrics state) > gameInitialTime (gameMetrics state)
        then initState
        else updateHandler dt state

outrunPlay
  :: (Int, Int)
  -> AssetLibrary
  -> Font
  -> RacingTrack
  -> IO ()
outrunPlay resolution assets font track =

  resettableGame
    play FullScreen afr32_hippieblue 60 initState
      (drawGame resolution [terrainPic, roadSegment] font)
      handleInput updateGame

  where
    player = Dynamic (RoadObject (0, 0, 200) blank) (0, 0)

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

desertTrack :: AssetLibrary -> RacingTrack
desertTrack assets = addCacti $
  mconcat ( map ($ trackRoadColors)
  [
    makeTrackCustom (Custom 10),

    trackMapWith (addRoadObject finishLine) . oneLiner,

    rightTurn,
    addCurve Gently TurningRight . makeTrack LongTrack,
    makeTrack ShortTrack,
    rightTurn,
    addCurve Moderately TurningRight . makeTrack LongTrack,
    leftTurn,
    addCurve Moderately TurningLeft . makeTrack ShortTrack,
    rightTurn,
    addCurve Moderately TurningRight . makeTrack ShortTrack,
    makeTrack NormalTrack,
    rightTurn,
    addCurve Moderately TurningRight . makeTrack NormalTrack,
    leftTurn,
    addCurve Gently TurningLeft . makeTrack ShortTrack,
    makeTrack NormalTrack,
    rightTurn,
    addCurve Moderately TurningRight . makeTrack NormalTrack,
    leftTurn,
    addCurve Moderately TurningLeft . makeTrack ShortTrack,
    rightTurn,
    addCurve Moderately TurningRight . makeTrack ShortTrack,
    leftTurn,
    addCurve Moderately TurningLeft . makeTrack ShortTrack,
    makeTrack ShortTrack,
    rightTurn,
    addCurve Steeply TurningRight . makeTrack ShortTrack
  ])
  where
    trackRoadColors = [ afr32_darkdorado ]

    oneLiner = makeTrackCustom (Custom 1)
    rightTurn = trackMapWith (addRoadObject arrowRight) . oneLiner
    leftTurn = trackMapWith (addRoadObject arrowLeft) . oneLiner

    ((arrowW, arrowH), arrowPic) = fetchSpriteFromLibrary "right_arrow" assets

    arrowRight = RoadObject (2500, 0, 0)
      (pictureAlign CenterAlign BottomAlign
      (fromIntegral arrowW) (fromIntegral arrowH)
      (scale 20 20 arrowPic))

    arrowLeft = RoadObject (-2500, 0, 0)
      (pictureAlign CenterAlign BottomAlign
      (fromIntegral arrowW) (fromIntegral arrowH)
      (scale (-20) 20 arrowPic))

    ((finishW, finishH), finishPic) = fetchSpriteFromLibrary "finish" assets
    finishLine = RoadObject (0, 0, 0)
      (scale 20 20 (
        pictureAlign CenterAlign BottomAlign
        (fromIntegral finishW) (fromIntegral finishH)
        finishPic
      ))

    ((cactusW, cactusH), cactusPic) = fetchSpriteFromLibrary "cactus" assets
    cactus xx = RoadObject (xx, 0, 0)
      (scale 10 10 (
        pictureAlign CenterAlign BottomAlign
        (fromIntegral cactusW) (fromIntegral cactusH)
        cactusPic
      ))

    cactiDistribution rl = 3500 * (xx + off)
      where
        z  = getZR3 (roadLinePosition rl)
        xx = 3 * sin (z / 10)

        off = if xx < 0
          then -1
          else 1

    addCacti = trackMapWith
      (\rl -> addRoadObject (cactus (cactiDistribution rl)) rl)
