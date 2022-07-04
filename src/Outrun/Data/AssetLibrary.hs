module Outrun.Data.AssetLibrary (module Outrun.Data.AssetLibrary) where
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Juicy

data Asset
  = Sprite    { getSprite    :: Picture   }
  | Animation { getAnimation :: [Picture] }

type AssetLibrary = [(String, Asset)]

addToLibrary :: (String, Asset) -> AssetLibrary -> AssetLibrary
addToLibrary = (:)

fetchSpriteFromLibrary :: String -> AssetLibrary -> Asset
fetchSpriteFromLibrary name lib =
  fromMaybe (Sprite blank) (lookup name lib)

fetchAnimationFromLibrary :: String -> AssetLibrary -> Asset
fetchAnimationFromLibrary name lib =
  fromMaybe (Animation []) (lookup name lib)

loadSprite
  :: FilePath -- Path to the sprite directory
  -> String   -- Sprite name
  -> IO (String, Asset)
loadSprite dir name = do
  pic <- loadJuicyPNG (dir ++ name ++ ".png")
  return (name, Sprite (fromMaybe blank pic))

loadAnimation
  :: FilePath -- Path to the animation directory
  -> String   -- Animation name
  -> Int      -- Number of frames
  -> IO (String, Asset)
loadAnimation dir name number = do
  pics <- sequence loadFrames
  return (name, Animation (map (fromMaybe blank) pics))
  where
    loadFrames =
      map (\i -> loadJuicyPNG (dir ++ name ++ show i ++ ".png"))
      [1..number]
