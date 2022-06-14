module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

type Position = (Float, Float, Float)

data RoadLine = RoadLine
    Int            -- Road line index
    (Float, Float) -- (y, z) coordinates of the line
    Float          -- Curvature
    Color          -- Color of the road segment

data Camera = Camera
    Position   -- Position in world space
    (Int, Int) -- Screen resolution
    Float      -- Camera depth
    Int        -- Render distance

data GameState = GameState Camera (Float, Float) [RoadLine]

shiftCamera :: Camera -> Position -> Camera
shiftCamera (Camera (x1, y1, z1) res depth rendDist) (x2, y2, z2) =
    Camera (x1 + x2, y1 + y2, z1 + z2) res depth rendDist

changeRenderDistance :: Camera -> Int -> Camera
changeRenderDistance (Camera pos res depth rendDist) = Camera pos res depth

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

drawTrack :: Camera -> [RoadLine] -> Picture
drawTrack _ [] = blank     -- Cannot draw a segment from an empty list
drawTrack _ [line] = blank -- Cannot draw a segment from a single line ¯\_(ツ)_/¯
drawTrack (Camera _ _ _ 0) _ = blank
drawTrack camera (line1 : (line2 : lines)) =
    drawTrack camera' (line2 : lines) <> segment
    where
        RoadLine _ (ly, lz) curve col = line1
        (Camera (cx, cy, cz) res depth rendDist) = camera

        (p1, w1) = project camera line1
        (p2, w2) = project camera line2

        camera' = changeRenderDistance camera (rendDist - 1)

        segment = if cz <= lz
            then drawRoadSegment col p1 w1 p2 w2
            else blank

roadWidth = 2000

project :: Camera -> RoadLine -> (Point, Float)
project (Camera (cx, cy, cz) (width, height) depth _) roadLine =
    (screenPos, w)
    where
        RoadLine _ (ly, lz) _ _ = roadLine

        halfWidth = fromIntegral width / 2
        halfHeight = fromIntegral width / 2

        -- Project the road line onto the screen
        scale = depth / (lz - cz)
        sx = (1 + scale * negate cx) * halfWidth
        sy = (1 + scale * (ly - cy)) * halfHeight
        w = scale * roadWidth * halfWidth

        -- Translate coordinates to the bottom center of the screen
        screenPos = (sx - halfWidth, sy - halfHeight)

track :: [RoadLine]
track = trackBuilder 1600
    where
        trackBuilder 0 = []
        trackBuilder n =
            trackBuilder (n - 1) ++ [RoadLine n (ly, lx) 0 col]
            where
                n' = fromIntegral n

                ly = sin (n' / 30) * 1500
                lx = n' * 200

                col = case n `mod` 2 of
                    0 -> green
                    _ -> dark green

gameWindow :: Display
gameWindow = InWindow "Outrun - clone" (1024, 768) (0, 0)

-- drawing :: Float -> Picture
-- drawing t = drawTrack cam track <> color black (rectangleWire 1024 768)
--     where
--         z = t * 2000
--         cam = Camera (0, 1500 + h, z) (1024, 768) 0.84 10

--         h = case take 1 (dropWhile (\(RoadLine _ (ly, lz) _ _) -> lz < z) track) of
--             (RoadLine _ (ly, _) _ _ : _) -> ly
--             _ -> 0

drawGame :: GameState -> Picture
drawGame (GameState cam _ track) = drawTrack cam track

handleGame :: Event -> GameState -> GameState
handleGame ev (GameState cam (dy, dz) track) = case ev of
    (EventKey (Char 'w') Down _ _) -> GameState cam (dy,  1) track
    (EventKey (Char 'w') Up _ _)   -> GameState cam (dy,  0) track
    (EventKey (Char 's') Down _ _) -> GameState cam (dy, -1) track
    (EventKey (Char 's') Up _ _)   -> GameState cam (dy,  0) track
    _ -> GameState cam (dy, dz) track

main :: IO ()
main = play gameWindow white 60 initState drawGame handleGame updateGame
    where
        initState = GameState cam (0, 0) track
        cam = Camera (0, 1500, 0) (1024, 768) 0.84 1000

        updateGame _dt (GameState cam (dy, dz) track) =
            GameState (shiftCamera cam (0, 0, dz * 100)) (dy, dz) track
