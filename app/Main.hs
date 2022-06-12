module Main where

import Graphics.Gloss

-- data RoadLength = Short | Medium | Long

data RoadLine = RoadLine
    Float -- Z coordinate of the start of the road line
    Float -- Z coordinate of the end of the road line
    Color -- Color of the road segment

type CameraPosition = (Float, Float, Float) -- (x, y, z)

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


drawRoad :: CameraPosition -> [RoadLine] -> Picture
drawRoad _ [] = blank
drawRoad c (RoadLine z1 z2 col : lines) = segment <> drawRoad c lines
    where
        (cx, cy, cz) = c

        (x1, y1, w1) = projectRoadLine c z1
        (x2, y2, w2) = projectRoadLine c z2

        segment = if z1 - cz >= 0
            then drawRoadSegment col (x1, y1) w1 (x2, y2) w2
            else blank

projectRoadLine
    :: CameraPosition -- Camera position (x, y, z)
    -> Float -- Road line Z coordinate
    -> (Float, Float, Float) -- (screen x, screen y, w)
projectRoadLine (cx, cy, cz) z = (sx, sy, ww)
    where
        roadWidth  = 300
        cameraDepth = 100

        scale = 270 / (z - cz)

        sx = 0
        sy = -scale * 400 -- inverted y coordinate
        ww =  scale * roadWidth


roadTest :: Int -> [RoadLine]
roadTest 0 = []
roadTest n = roadSegment : roadTest (n - 1)
    where
        hh = 400

        h1 = hh * n
        h2 = h1 + hh

        roadSegment = RoadLine (fromIntegral h1) (fromIntegral h2) col
        col = case n `mod` 2 of
            0 -> green
            _ -> dark green

window :: Display
window = InWindow "Kinda working road" (960, 540) (0, 0)

drawing :: Float -> Picture
drawing t = rectangleSolid 960 540 <>
            translate 0 0 (color green (drawRoad (0, 1, t * 100) (roadTest 200))) <>
            color red (line [(-480, 0), (480, 0)])

main :: IO ()
main = animate window white drawing
