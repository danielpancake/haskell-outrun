module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Maybe

cameraHeight      = 1500
roadSegmentLength = 200
roadSegmentWidth  = 2000

data WithCustom a b = Common a | Custom b

fromCustom :: (a -> b) -> WithCustom a b -> b
fromCustom func val = case val of
    Common common -> func common
    Custom custom -> custom

type Position = (Float, Float, Float)

shiftPosition :: Position -> Position -> Position
shiftPosition (x, y, z) (x', y', z') = (x + x', y + y', z + z')

data Camera = Camera
    { cameraPosition       :: Position   -- Position in world space
    , cameraResolution     :: (Int, Int) -- Screen resolution
    , cameraDepth          :: Float      -- Camera depth. Basically, has
        -- an inversely proportional relation to FOV. Should be increased while
        -- moving up, decreased while moving down
    , cameraRenderDistance :: Int   -- Render distance
    , cameraSmoothness     :: Float -- How fast camera moves towards target position
    }

changeRenderDistance :: Int -> Camera -> Camera
changeRenderDistance renderDist cam = cam { cameraRenderDistance = renderDist }

shiftCamera :: Position -> Camera -> Camera
shiftCamera delta cam = cam { cameraPosition = newPos }
    where
        newPos = shiftPosition (cameraPosition cam) delta

data RoadLine = RoadLine
    { roadLinePosition  :: (Float, Float) -- (y, z) coordinates of the line

    -- Both curve and pitch values make the camera follow the movement,
    -- thereas roadline height does not
    , roadLineCurveRate :: Float -- Curve rate
    , roadLinePitchRate :: Float -- Pitch rate
    , roadLineColor     :: Color -- Color of the road segment
    }

roadLineNthMaybe :: (RoadLine -> a) -> Int -> [RoadLine] -> Maybe a
roadLineNthMaybe get offset track =
    case drop offset track of
        (r : _) -> Just (get r)
        _ -> Nothing

changeRoadLineCurveRate :: Float -> RoadLine -> RoadLine
changeRoadLineCurveRate curve roadLine = roadLine { roadLineCurveRate = curve }

changeRoadLinePitchRate :: Float -> RoadLine -> RoadLine
changeRoadLinePitchRate pitch roadLine = roadLine { roadLinePitchRate = pitch }

shiftRoadLine :: (Float, Float) -> RoadLine -> RoadLine
shiftRoadLine (dy, dz) rl = rl { roadLinePosition = newPos }
    where
        (y, z) = roadLinePosition rl
        newPos = (y + dy, z + dz)

data TrackLength = ShortTrack | NormalTrack | LongTrack
trackLengthValue :: TrackLength -> Int
trackLengthValue length = case length of
    ShortTrack  -> 50
    NormalTrack -> 150
    LongTrack   -> 300

data TrackChangeRate = Gently | Moderately | Steeply

trackCurveValue :: TrackChangeRate -> Float
trackCurveValue curve = case curve of
    Gently     -> 0.1
    Moderately -> 0.5
    Steeply    -> 0.8

trackPitchValue :: TrackChangeRate -> Float
trackPitchValue pitch = case pitch of
    Gently     -> 1
    Moderately -> 5
    Steeply    -> 8

data TrackCurveDirection = TurningLeft | TurningRight
trackCurveDir :: Num a => TrackCurveDirection -> (a -> a)
trackCurveDir dir = case dir of
    TurningLeft  -> negate
    TurningRight -> id

data TrackPitchDirection = GoingUp | GoingDown
trackPitchDir :: Num a => TrackPitchDirection -> (a -> a)
trackPitchDir dir = case dir of
    GoingDown -> negate
    GoingUp   -> id

-- | Hills are elevations of the road segments height
data TrackHill = SmallHill | MediumHill | LargeHill
trackHillHeight :: TrackHill -> Float
trackHillHeight hill = case hill of
    SmallHill  -> 1000
    MediumHill -> 5000
    LargeHill  -> 8000

trackHillDir :: Num a => TrackPitchDirection -> (a -> a)
trackHillDir dir = case dir of
    GoingDown -> (1 -) -- InterpolationFunc takes values from 0 to 1
                       -- For that reason, inversion is done that way
    GoingUp   -> id

type RacingTrack = [RoadLine]

data RoadObject = RoadObject
    { roadObjectPosition :: Position
    , roadObjectColor    :: Color
    }

shiftRoadObject :: Position -> RoadObject -> RoadObject
shiftRoadObject delta obj = obj { roadObjectPosition = newPos }
    where
        newPos = shiftPosition (roadObjectPosition obj) delta

data Player = Player
    { playerPosition :: Position
    , playerVelocity :: (Float, Float)
    , playerColor    :: Color
    }

shiftPlayer :: Position -> Player -> Player
shiftPlayer delta player = player { playerPosition = newPos }
    where
        newPos = shiftPosition (playerPosition player) delta

changePlayerVelocity :: (Float, Float) -> Player -> Player
changePlayerVelocity vec player = player { playerVelocity = vec }

data GameState = GameState
    Camera
    (Float, Float) -- Movement vector (x, z)
    RacingTrack
    [RoadObject]
    Player

---- | Interpolation functions |--------------------------------

-- | Approaches the target value with the given speed
approach :: Float -> Float -> Float -> Float
approach current target speed =
    if current < target then
        min (current + speed) target
    else
        max (current - speed) target

---- | All interpolation functions below (typed InterpolationFunc)
---- | take a value between 0 and 1
type InterpolationFunc = Float -> Float

easeInOutSine :: InterpolationFunc
easeInOutSine x = (1 - cos (x * pi)) / 2

easeInSine :: InterpolationFunc
easeInSine x = 1 - cos (x * pi / 2)

easeOutQuint :: InterpolationFunc
easeOutQuint x = 1 - ((1 - x) ** 5)

----------------------------------------------------------------


---- | Drawing Section |----------------------------------------

-- Draws a road segment
drawRoadSegment
    :: Color -- Color of the road segment
    -> Point -- Bottom point of the road segment
    -> Float -- Width of the bottom road segment
    -> Point -- Top point of the road segment
    -> Float -- Width of the top road segment
    -> Picture
drawRoadSegment col (x1, y1) w1 (x2, y2) w2 =
    color col (polygon points)
    where
        points = [
            (x1 - w1, y1), (x1 + w1, y1),
            (x2 + w2, y2), (x2 - w2, y2)
            ]

drawObject :: Camera -> RoadObject -> Picture
drawObject cam obj =
    drawRoadSegment (roadObjectColor obj)
    (x, y)  (w * 200)
    (x, y + (w * 400)) (w * 200)
    where
        ((x, y), w) = project cam (roadObjectPosition obj)

drawTrack :: Camera -> [RoadObject] -> RacingTrack -> Picture
drawTrack = drawTrack' (0, 0, 0, 0)
    where
        drawTrack' _ _ _ [] = blank     -- Cannot draw a segment from an empty list
        drawTrack' _ _ _ [line] = blank -- Cannot draw a segment from a single line ¯\_(ツ)_/¯

        drawTrack' (x, dx, y, dy) cam objects (line : nextLine : rest) =
            case cameraRenderDistance cam of
                0 -> blank -- Zero render distance
                _ -> let

                    RoadLine (_, _) curve pitch col = line

                    y'  = y + dy
                    x'  = x + dx
                    dx' = dx + curve
                    dy' = dy + pitch

                    (ly1, lz1) = roadLinePosition (shiftRoadLine (y,  0) line)
                    ((x1, y1), w1) = project cam (0, ly1, lz1)

                    (ly2, lz2) = roadLinePosition (shiftRoadLine (y', 0) nextLine)
                    ((x2, y2), w2) = project cam (0, ly2, lz2)

                    rendDist = cameraRenderDistance cam
                    cam' = changeRenderDistance (rendDist - 1) cam

                    (_, _, cz) = cameraPosition cam

                    -- This is really messy now
                    roadSegment =
                        if cz <= lz1
                        then drawRoadSegment col
                            (x1 + x,  y1) (w1 * roadSegmentWidth)
                            (x2 + x', y2) (w2 * roadSegmentWidth)
                        else blank

                    getRoadObjectZ obj = oz
                        where (_, _, oz) = roadObjectPosition obj

                    (objToDraw, restObj) = span ((<lz2) . getRoadObjectZ) (dropWhile ((<lz1) . getRoadObjectZ) objects)

                    drawnObjects = pictures (map (translate x 0 . drawObject cam . shiftRoadObject (0, y, 0)) objToDraw)

                    in drawTrack' (x', dx', y', dy') cam' restObj (nextLine : rest) <> roadSegment <> drawnObjects

project
    :: Camera -- Camera to use for projection
    -> (Float, Float, Float) -- Position of the projected object
    -> (Point, Float) -- On screen coordinates and project scale factor
project cam (lx, ly, lz) = (screenPos, scale * halfWidth)
    where
        (cx, cy, cz) = cameraPosition cam
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

drawGame :: GameState -> Picture
drawGame (GameState cam _ track roadObjs player) =
    drawTrack cam roadObjs track <> drawnPlayer
    where
        ((x, y), scale) = project cam (playerPosition player)
        w = 500 * scale
        drawnPlayer = drawRoadSegment black (x, y) w (x, y + w * 2) w

----------------------------------------------------------------

---- | Building Track Section |---------------------------------
---- | This section contains functions necessary to build tracks
---- | and connect them with each other
----
---- | It is also possible to generate tracks manually
----
---- | Note: It's a good idea to begin and end the track with
---- | short straight segments to avoid noticable connections
---- | between tracks

sampleTrack :: RacingTrack
sampleTrack = connectTracksMany
    [
        addCurve Gently TurningLeft (makeTrack ShortTrack [green, dark green]),
        addCurve Steeply TurningRight (
            addPitch Steeply GoingUp (makeTrack ShortTrack [red, dark red])
        ),
        addCurve Steeply TurningLeft(
            addPitch Steeply GoingDown (makeTrack ShortTrack [green, dark green])
        ),
        addPitch Steeply GoingDown (makeTrack ShortTrack [green, dark green])
    ]

-- | Example of the track generated by hand
sampleTrack2 :: RacingTrack
sampleTrack2 = trackBuilder 360
    where
        trackBuilder 0 = []
        trackBuilder n =
            trackBuilder (n - 1) ++ [RoadLine (0, lz) curve pitch col]
            where
                n' = fromIntegral n

                pitch = if n < 180
                    then pi^2*cos(pi*n'/180)/2
                    else 0
                curve = 0

                ly = sin (n' / 30) * 1500
                lz = n' * 200

                col = if even n
                    then green
                    else dark green

-- | Makes a track given its length, curve rate and a list of alternating colors
makeTrack :: TrackLength -> [Color] -> RacingTrack
makeTrack trackLength = makeTrackCustom (Common trackLength)

-- | Makes a track with customizable length, curve rate and a list of alternating colors
makeTrackCustom
    :: WithCustom TrackLength Int
    -> [Color] -> RacingTrack
makeTrackCustom trackLength colors = take length (infRacingTrack track)
    where
        length = fromCustom trackLengthValue trackLength
        track = foldr (connectTracks . (:[]) . RoadLine (0, 0) 0 0) [] colors

-- | Adds a curve to a track
addCurve :: TrackChangeRate -> TrackCurveDirection -> RacingTrack -> RacingTrack
addCurve = addCurveCustom . Common

-- | Adds a custom curve to a track
addCurveCustom
    :: WithCustom TrackChangeRate Float
    -> TrackCurveDirection
    -> RacingTrack -> RacingTrack
addCurveCustom curve dir =
    map (changeRoadLineCurveRate (trackCurveDir dir (fromCustom trackCurveValue curve)))

-- | Adds a pitch to a track
addPitch :: TrackChangeRate -> TrackPitchDirection -> RacingTrack -> RacingTrack
addPitch = addPitchCustom . Common

-- | Adds a custom pitch to a track
addPitchCustom
    :: WithCustom TrackChangeRate Float
    -> TrackPitchDirection
    -> RacingTrack -> RacingTrack
addPitchCustom pitch dir =
    map (changeRoadLinePitchRate (trackPitchDir dir (fromCustom trackPitchValue pitch)))

-- | Adds a hill to a track
addHill :: TrackHill -> TrackPitchDirection -> RacingTrack -> RacingTrack
addHill = addHillCustom easeInOutSine . Common

-- | Adds a custom hill to a track
addHillCustom
    :: InterpolationFunc
    -> WithCustom TrackHill Float
    -> TrackPitchDirection
    -> RacingTrack -> RacingTrack
addHillCustom interpolate hill dir track =
    let -- Get the hill length
        func x = trackHillDir dir (fromCustom trackHillHeight hill * interpolate x)

        -- Pack road lines with normilized coordinates
        len = fromIntegral (length track)
        packed = zip track (map (/len) [1..])

    in map (\(rl, x) -> shiftRoadLine (func x, 0) rl) packed

-- | Shifts the track by the given amount
shiftTrack :: (Float, Float) -> RacingTrack-> RacingTrack
shiftTrack delta = map (shiftRoadLine delta)

-- | Connects two tracks together
connectTracks :: RacingTrack -> RacingTrack -> RacingTrack
connectTracks track = (track ++) . shiftTrack (dy, dz)
    where
        -- Getting the y coordinate of the last road line of the first track
        -- and lifting second track by this amount
        dy = case last track of
            (RoadLine (ly, _) _ _ _) -> ly

        dz = fromIntegral (length track) * roadSegmentLength

-- | Concatenates a list of tracks together
connectTracksMany :: [RacingTrack] -> RacingTrack
connectTracksMany = foldr connectTracks []

-- | Generates infinite racing track from the given one
infRacingTrack :: RacingTrack -> RacingTrack
infRacingTrack track = connectTracks track (infRacingTrack track)

----------------------------------------------------------------

playerMovement :: Event -> (Float, Float) -> (Float, Float)
playerMovement ev (dy, dz) = case ev of
    (EventKey (Char 'w') Down _ _) -> (dy, 1)
    (EventKey (Char 'w') Up _ _)   -> (dy, 0)

    (EventKey (Char 's') Down _ _) -> (dy, -1)
    (EventKey (Char 's') Up _ _)   -> (dy, 0)

    (EventKey (Char 'a') Down _ _) -> (-1, dz)
    (EventKey (Char 'a') Up _ _)   -> (0,  dz)

    (EventKey (Char 'd') Down _ _) -> (1, dz)
    (EventKey (Char 'd') Up _ _)   -> (0, dz)

    _ -> (dy, dz)

handleGame :: Event -> GameState -> GameState
handleGame ev (GameState cam keys track roadObjs playerPos) =
    GameState cam (playerMovement ev keys) track roadObjs playerPos

updateGame :: Float -> GameState -> GameState
updateGame dt (GameState cam keys track roadObjs player) = newState
    where
        (_, cy, cz) = cameraPosition cam
        (px, py, pz) = playerPosition player

        -- Drop road lines behind the camera
        updatedTrack = dropWhile ((< cz) . snd . roadLinePosition) track

        -- Get coordinates of the nth road line ahead
        nthPosition offset = Data.Maybe.fromMaybe (0, 0)
            (roadLineNthMaybe roadLinePosition offset updatedTrack)
        (ly1, _) = nthPosition 4 -- For the camera height
        (ly2, _) = nthPosition  4 -- For the player position

        smoothness = cameraSmoothness cam

        -- TODO: refactor logic cuuse it's really ugly

        -- Camera height difference
        camDY = (cameraHeight + ly1 - cy) * smoothness
        -- Camera position difference (relative to the player)
        camDZ = (pz - cz - 1000) * smoothness
        shiftedCam = shiftCamera (0, camDY, camDZ) cam

        -- Player height difference
        plrDY = ly2 - py

        nthCurve offset = Data.Maybe.fromMaybe 0
            (roadLineNthMaybe roadLineCurveRate offset updatedTrack)

        curveEffect = 5 * pSpdZ * nthCurve 4

        (vx, vz) = keys
        (pSpdX, pSpdZ) = playerVelocity player
        newVelocity = (approach pSpdX vx 0.3, approach pSpdZ vz 0.05)

        move = shiftPlayer ((pSpdX - curveEffect) * 40, plrDY, pSpdZ * 350) . changePlayerVelocity newVelocity

        newState = GameState shiftedCam keys updatedTrack roadObjs (move player)

gameWindow :: Display
gameWindow = InWindow "Outrun - clone" (1024, 768) (0, 0)

outrun :: IO ()
outrun = play gameWindow white 60 initState drawGame handleGame updateGame
    where
        initState = GameState initCamera (0, 0) (infRacingTrack sampleTrack)
            [RoadObject (0, 0, 1000) yellow, RoadObject (0, 0, 2000) yellow, RoadObject (0, 0, 3000) yellow]
            initPlayer

        initCamera = Camera (0, cameraHeight, 0) (1024, 768) 0.3 100 0.75
        initPlayer = Player (0, 0, 0) (0, 0) black

main :: IO ()
main = outrun
