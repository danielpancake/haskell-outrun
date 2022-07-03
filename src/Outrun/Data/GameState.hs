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
  , gameTime        :: Float
  , gameBackground  :: Picture
  }
