module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

roadSegmentLength = 200
roadSegmentWidth  = 2000

type Position = (Float, Float, Float)

data Camera = Camera
    { cameraPosition       :: Position   -- Position in world space
    , cameraResolution     :: (Int, Int) -- Screen resolution
    , cameraDepth          :: Float      -- Camera depth
    , cameraRenderDistance :: Int        -- Render distance
    }

changeRenderDistance :: Camera -> Int -> Camera
changeRenderDistance (Camera pos res depth _) = Camera pos res depth

shiftCamera :: Camera -> Position -> Camera
shiftCamera (Camera pos res depth rendDist) (x2, y2, z2) =
    Camera newPos res depth rendDist
    where
        (x1, y1, z1) = pos
        newPos = (x1 + x2, y1 + y2, z1 + z2)

data RoadLine = RoadLine
    { roadLinePosition  :: (Float, Float) -- (y, z) coordinates of the line
    , roadLineCurvature :: Float          -- Curvature
    , roadLineColor     :: Color          -- Color of the road segment
    }

type RacingTrack = [RoadLine]

data GameState = GameState Camera (Float, Float) RacingTrack

-- | Generate infinite racing track from the given one
gameRacingTrack :: RacingTrack -> RacingTrack
gameRacingTrack track =
    track ++ gameRacingTrack (shiftTrack track)
    where
        shiftTrack :: RacingTrack -> RacingTrack
        shiftTrack track' =
            map (\(RoadLine (ly, lz) curv col) ->
                RoadLine (ly, lz + dz) curv col) track'
            where
                dz = fromIntegral (length track') * roadSegmentLength

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
drawTrack _ [] = blank     -- Cannot draw a segment from an empty list
drawTrack _ [line] = blank -- Cannot draw a segment from a single line ¯\_(ツ)_/¯
drawTrack (Camera _ _ _ 0) _ = blank -- Zero render distance

drawTrack camera (line1 : (line2 : lines)) =
    drawTrack camera' (line2 : lines) <> segment
    where
        RoadLine (ly, lz) curve col = line1

        (p1, w1) = project camera line1
        (p2, w2) = project camera line2

        rendDist = cameraRenderDistance camera
        camera' = changeRenderDistance camera (rendDist - 1)

        (_, _, cz) = cameraPosition camera
        segment = if cz <= lz
            then drawRoadSegment col p1 w1 p2 w2
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

sampleTrack :: RacingTrack
sampleTrack = trackBuilder 12
    where
        trackBuilder 0 = []
        trackBuilder n =
            trackBuilder (n - 1) ++ [RoadLine (ly, lz) 0 col]
            where
                n' = fromIntegral n

                ly = 0 --sin (n' / 30) * 1500
                lz = n' * 200

                col = if even n
                    then green
                    else dark green

gameWindow :: Display
gameWindow = InWindow "Outrun - clone" (1024, 768) (0, 0)


drawGame :: GameState -> Picture
drawGame (GameState cam _ track) = drawTrack cam track

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
        (_, _, cz) = cameraPosition cam

        newState = GameState (shiftCamera cam (0, 0, ddz * 1000)) delta updatedTrack

        updatedTrack = dropWhile ((< cz) . snd . roadLinePosition) track

main :: IO ()
main = play gameWindow white 60 initState drawGame handleGame updateGame
    where
        initState = GameState initCamera (0, 0) (gameRacingTrack sampleTrack)
        initCamera = Camera (0, 1500, 0) (1024, 768) 0.84 1600
