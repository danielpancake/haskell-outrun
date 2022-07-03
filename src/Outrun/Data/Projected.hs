-- | Data type for the projected objects
module Outrun.Data.Projected (module Outrun.Data.Projected ) where
import           Outrun.Data

data Projected a = Projected
  { fromProjected     :: a
  , projectedPosition :: PointR2
  , projectedScale    :: Float
  }

shiftProjected :: PointR2 -> Projected a -> Projected a
shiftProjected delta (Projected a p s) =
  Projected a (shiftPos2D p delta) s
