module Outrun.Data.GameState (module Outrun.Data.GameState) where
import           Graphics.Gloss.Interface.IO.Game
import           Outrun.Data.AssetLibrary
import           Outrun.Data.Camera
import           Outrun.Data.RacingTrack
import           Outrun.Data.RoadObject

data OutrunGameState = GameState
  { gameAssetLibrary :: AssetLibrary
  , gameInput        :: [Key]
  , gameCamera       :: Camera
  , gameRacingTrack  :: RacingTrack
  , gamePlayer       :: DynamicRoadObject
  , gameMetrics      :: OutrunGameMetrics
  }

data OutrunGameMetrics = Metrics
  { gameInitialTime :: Float
  , gameTrackLength :: Float
  , gameTime        :: Float
  , gameLaps        :: Int
  }
