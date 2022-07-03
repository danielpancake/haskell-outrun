module Outrun.InputProcessing (module Outrun.InputProcessing) where
import           Data.Fixed
import           Data.List
import           Graphics.Gloss.Interface.IO.Game
import           Outrun.Data
import           Outrun.Data.Camera
import           Outrun.Data.Defaults
import           Outrun.Data.GameState
import           Outrun.Data.RacingTrack
import           Outrun.Data.RoadLine
import           Outrun.Data.RoadObject
import           Utils


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
    GameState pressedkeys cam track
      (Dynamic player (spdX, spdZ))
      time background = state

    (cx, cy, cz) = cameraPosition cam
    (px, __, pz) = roadObjectPosition player

    -- Drop all lines behind the camera
    updateTrack = dropWhile ((< cz) . getZR3 . roadLinePosition)

    -- Update camera position
    cameraSmoothness = 0.5
    cameraZOffset = 300

    cdx = (px - cx) * cameraSmoothness
    cdy = 0
    cdz = (pz - cameraZOffset - cz) * cameraSmoothness

    updateCamera = shiftCameraPosition (cdx, cdy, cdz) .
      setCameraParalax (mod' (cameraParalax cam - centrifugalForce / 50 - spdX / 100) 600)
      -- Kinda bad wau to make a parallax but whatever

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
    (playerRL : _) =
      dropWhile ((< pz) . getZR3 . roadLinePosition) (getTrack track)

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
      { gameRacingTrack = trackMap updateTrack track
      , gameCamera      = updateCamera cam
      , gamePlayer      =
        Dynamic (updatePosition player) (spdX', spdZ')
      }
