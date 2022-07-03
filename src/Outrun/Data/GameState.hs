module Outrun.Data.GameState (module Outrun.Data.GameState) where
import           Graphics.Gloss.Interface.IO.Game
import           Outrun.Data.Camera
import           Outrun.Data.RacingTrack
import           Outrun.Data.RoadObject

data OutrunGameState = GameState
  { gameInput       :: [Key]
  , gameCamera      :: Camera
  , gameRacingTrack :: RacingTrack
  , gamePlayer      :: DynamicRoadObject
  , gameMetrics     :: OutrunGameMetrics
  , gameBackground  :: Picture
  }

data OutrunGameMetrics = Metrics
  { gameInitialTime :: Float
  , gameTrackLength :: Float
  , gameTime        :: Float
  , gameLaps        :: Int
  }
