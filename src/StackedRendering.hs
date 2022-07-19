module StackedRendering (module StackedRendering) where
import Graphics.Gloss.Data.Picture

drawStacked :: [Picture] -> Picture
drawStacked = drawStackedExt 0 0 1 0

drawStackedExt
  :: Float
  -> Float
  -> Float
  -> Float
  -> [Picture] -> Picture
drawStackedExt dx dy dscale scaleFactor pics = mconcat picsWithOffsets
  where
    len = length pics
    offsets = reverse (
        scanl (\(a,b,c) (d,e,f) -> (a+d,b+e,c+f))
        (0, 0, scaleFactor)
        (replicate len (dx, dy, -dscale)))

    picsWithOffsets =
      zipWith (curry (\(pic, (x, y, s)) ->
          scale s s
          (translate x y pic)
        )) pics offsets
