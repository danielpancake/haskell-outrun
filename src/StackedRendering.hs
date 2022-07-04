module StackedRendering (module StackedRendering) where
import           Graphics.Gloss.Data.Picture

drawStacked :: Float -> Float -> [Picture] -> Picture
drawStacked dx dy pics = mconcat picsWithOffsets
  where
    len = length pics
    offsets =
      scanl (\(a,b) (c,d) -> (a+c,b+d)) (0, 0)
      (replicate len (dx, dy))

    picsWithOffsets =
      zipWith (curry (\(p, (x, y)) -> translate x y p)) pics offsets
