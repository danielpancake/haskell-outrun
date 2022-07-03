module Outrun.Data (module Outrun.Data) where
import           Data.Tuple.HT

-- | Point in the 2D space
type PointR2 = (Float, Float)

getXR2
 ,  getYR2
 :: PointR2 -> Float
getXR2 = fst
getYR2 = snd

shiftPos2D :: PointR2 -> PointR2 -> PointR2
shiftPos2D (x, y) (x', y') = (x + x', y + y')

-- | Point in the 3D space
type PointR3 = (Float, Float, Float)

getXR3
 ,  getYR3
 ,  getZR3
 :: PointR3 -> Float
getXR3 = fst3
getYR3 = snd3
getZR3 = thd3

shiftPos3D :: PointR3 -> PointR3 -> PointR3
shiftPos3D (x, y, z) (x', y', z') = (x + x', y + y', z + z')
