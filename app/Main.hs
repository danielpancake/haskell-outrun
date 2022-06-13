module Main where

import Graphics.Gloss

type Position = (Float, Float, Float)

data RoadLine = RoadLine
    Int -- Road line index
    (Float, Float) -- (y, z) coordinates of the line
    Color -- Color of the road segment

data Camera = Camera
    Position -- Position in world space
    (Int, Int) -- Screen resolution
    Float -- Camera depth

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
drawTrack _ [] = blank -- Cannot draw a segment from an empty list
drawTrack _ [line] = blank -- Cannot draw a segment from a single line ¯\_(ツ)_/¯
drawTrack cam (line1 : (line2 : lines)) = drawTrack cam (line2 : lines) <> segment
    where
        Camera (_, _, cz) _ _ = cam
        RoadLine _ (_, z) col = line1

        (p1, w1) = project cam line1
        (p2, w2) = project cam line2

        segment = if cz <= z
            then drawRoadSegment col p1 w1 p2 w2
            else blank

roadWidth = 2000

project :: Camera -> RoadLine -> (Point, Float)
project (Camera (cx, cy, cz) (width, height) depth) roadLine =
    (screenPos, w)
    where
        RoadLine _ (ly, lz) _ = roadLine

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
            trackBuilder (n - 1) ++ [RoadLine n (ly, lx) col]
            where
                n' = fromIntegral n

                ly = 0
                lx = n' * 200

                col = case n `mod` 2 of
                    0 -> green
                    _ -> dark green

window :: Display
window = InWindow "Kinda working road" (1024, 768) (0, 0)

drawing :: Float -> Picture
drawing t = drawTrack cam track <> color black (rectangleWire 1024 768)
    where
        cam = Camera (0, 1500, t * 2000) (1024, 768) 0.84

main :: IO ()
main = animate window white drawing
