module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

cameraHeight      = 1500
roadSegmentLength = 200
roadSegmentWidth  = 2000

type Position = (Float, Float, Float)

data Camera = Camera
    { cameraPosition       :: Position   -- Position in world space
    , cameraResolution     :: (Int, Int) -- Screen resolution
    , cameraDepth          :: Float      -- Camera depth
    , cameraRenderDistance :: Int        -- Render distance
    }

changeRenderDistance :: Int -> Camera -> Camera
changeRenderDistance renderDist cam = cam { cameraRenderDistance = renderDist }

shiftCamera :: Position -> Camera -> Camera
shiftCamera (x2, y2, z2) cam = cam { cameraPosition = newPos }
    where
        (x1, y1, z1) = cameraPosition cam
        newPos = (x1 + x2, y1 + y2, z1 + z2)

data RoadLine = RoadLine
    { roadLinePosition  :: (Float, Float) -- (y, z) coordinates of the line
    , roadLineCurveRate :: Float          -- Curvature
    , roadLineColor     :: Color          -- Color of the road segment
    }

changeRoadLineCurvature :: Float -> RoadLine -> RoadLine
changeRoadLineCurvature curve roadLine = roadLine { roadLineCurveRate = curve }

shiftRoadLine :: (Float, Float) -> RoadLine -> RoadLine
shiftRoadLine (dy, dz) rl = rl { roadLinePosition = newPos }
    where
        (y, z) = roadLinePosition rl
        newPos = (y + dy, z + dz)

data TrackLength = SHORT | NORMAL | LONG
trackLengthValue :: TrackLength -> Int
trackLengthValue length = case length of
    SHORT  -> 50
    NORMAL -> 150
    LONG   -> 300

data TrackDirection = LEFT | RIGHT
trackDirToSignum :: Num a => TrackDirection -> (a -> a)
trackDirToSignum LEFT  = negate
trackDirToSignum RIGHT = id

data TrackCurve = NONE | GENTLE TrackDirection | MEDIUM TrackDirection | STEEP TrackDirection
trackCurveValue :: TrackCurve -> Float
trackCurveValue curve = case curve of
    GENTLE dir -> trackDirToSignum dir 0.1
    MEDIUM dir -> trackDirToSignum dir 0.5
    STEEP  dir -> trackDirToSignum dir 0.8
    NONE       -> 0

type RacingTrack = [RoadLine]

data GameState = GameState Camera (Float, Float) RacingTrack


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

drawTrack :: Camera -> RacingTrack -> Picture
drawTrack = drawTrack' (0, 0)
    where
        drawTrack' _ _ [] = blank     -- Cannot draw a segment from an empty list
        drawTrack' _ _ [line] = blank -- Cannot draw a segment from a single line ¯\_(ツ)_/¯
        drawTrack' _ (Camera _ _ _ 0) _ = blank -- Zero render distance

        drawTrack' (x, dx) cam (line1 : lines) =
            drawTrack' (x', dx') cam' lines <> segment

            where
                RoadLine (ly, lz) curve col = line1

                ((x1, y1), w1) = project cam line1
                (line2 : _ ) = lines
                ((x2, y2), w2) = project cam line2

                rendDist = cameraRenderDistance cam
                cam' = changeRenderDistance (rendDist - 1) cam 

                x'  = x + dx
                dx' = dx + curve

                (_, _, cz) = cameraPosition cam
                segment = if cz <= lz
                    then drawRoadSegment col (x1 + x, y1) w1 (x2 + x', y2) w2
                    else blank

project :: Camera -> RoadLine
    -> (Point, Float) -- On screen coordinates and
                      -- projected width of the road line
project (Camera (cx, cy, cz) (width, height) depth _) roadLine =
    (screenPos, w)
    where
        RoadLine (ly, lz) _ _ = roadLine

        halfWidth = fromIntegral width / 2
        halfHeight = fromIntegral height / 2

        -- Project the road line onto the screen
        scale = depth / (lz - cz)
        sx = (1 + scale * negate cx) * halfWidth
        sy = (1 + scale * (ly - cy)) * halfHeight
        w = scale * roadSegmentWidth * halfWidth

        -- Translate coordinates to the bottom center of the screen
        screenPos = (sx - halfWidth, sy - halfHeight)

drawGame :: GameState -> Picture
drawGame (GameState cam _ track) = drawTrack cam track

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
        makeTrack SHORT (MEDIUM LEFT)  [green, dark green],
        makeTrack SHORT (MEDIUM RIGHT) [red, dark red]
    ]

-- | Example of the track generated by hand
sampleTrack2 :: RacingTrack
sampleTrack2 = trackBuilder 360
    where
        trackBuilder 0 = []
        trackBuilder n =
            trackBuilder (n - 1) ++ [RoadLine (ly, lz) curve col]
            where
                n' = fromIntegral n

                curve = 0.1

                ly = sin (n' / 30) * 1500
                lz = n' * 200

                col = if even n
                    then green
                    else dark green

-- | Makes a track given its length, curve rate and a list of alternating colors
makeTrack :: TrackLength -> TrackCurve -> [Color] -> RacingTrack
makeTrack trackLength trackCurve colors = take length (infRacingTrack track)
    where
        length = trackLengthValue trackLength

        track = addCurve trackCurve
            (foldr (connectTracks . (:[]) . RoadLine (0, 0) 0) [] colors)

-- | Adds a curve rate to a track
addCurve :: TrackCurve -> RacingTrack -> RacingTrack
addCurve curve = map (changeRoadLineCurvature (trackCurveValue curve))

-- | Shifts the track by the given amount
shiftTrack :: Float -> RacingTrack-> RacingTrack
shiftTrack dz = map (shiftRoadLine (0, dz))

-- | Connects two tracks together
connectTracks :: RacingTrack -> RacingTrack -> RacingTrack
connectTracks track = (track ++) . shiftTrack delta
    where
        delta = fromIntegral (length track) * roadSegmentLength

-- | Concatenates a list of tracks together
connectTracksMany :: [RacingTrack] -> RacingTrack
connectTracksMany = foldr connectTracks []

-- | Generates infinite racing track from the given one
infRacingTrack :: RacingTrack -> RacingTrack
infRacingTrack track = connectTracks track (infRacingTrack track)

----------------------------------------------------------------

handleGame :: Event -> GameState -> GameState
handleGame ev (GameState cam (dy, dz) track) = case ev of
    (EventKey (Char 'w') Down _ _) -> GameState cam (dy,  1) track
    (EventKey (Char 'w') Up _ _)   -> GameState cam (dy,  0) track
    (EventKey (Char 's') Down _ _) -> GameState cam (dy, -1) track
    (EventKey (Char 's') Up _ _)   -> GameState cam (dy,  0) track
    _ -> GameState cam (dy, dz) track

updateGame :: Float -> GameState -> GameState
updateGame dt (GameState cam delta track) = newState
    where
        (_, ddz) = delta
        (_, cy, cz) = cameraPosition cam

        updatedTrack = dropWhile ((< cz) . snd . roadLinePosition) track

        -- Get y offset of the road line which is under the camera
        ly = case take 1 (reverse (take offset updatedTrack)) of
            (r : _) -> fst (roadLinePosition r)
            _ -> 0
            where offset = 5

        ddy = (cameraHeight + ly) - cy

        newState = GameState (shiftCamera (0, ddy * 0.5, ddz * 100) cam) delta updatedTrack

gameWindow :: Display
gameWindow = InWindow "Outrun - clone" (1024, 768) (0, 0)

main :: IO ()
main = play gameWindow white 60 initState drawGame handleGame updateGame
    where
        initState = GameState initCamera (0, 0) (infRacingTrack sampleTrack)
        initCamera = Camera (0, cameraHeight, 0) (1024, 768) 0.84 100
