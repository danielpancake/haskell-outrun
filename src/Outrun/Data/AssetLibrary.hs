module Outrun.Data.AssetLibrary (module Outrun.Data.AssetLibrary) where
import           Codec.Picture
import           Graphics.Gloss
import           Graphics.Gloss.Juicy

data Asset
  = Sprite    ((Int, Int),  Picture )
  | Animation ((Int, Int), [Picture])

type AssetLibrary = [(String, Asset)]

addToLibrary :: (String, Asset) -> AssetLibrary -> AssetLibrary
addToLibrary = (:)

fetchSpriteFromLibrary :: String -> AssetLibrary -> ((Int, Int), Picture)
fetchSpriteFromLibrary name lib = case lookup name lib of
  Just (Sprite asset) -> asset
  _                   -> error ("Could not find sprite " ++ name ++ " in library")

fetchSpriteFromLibraryF :: String -> AssetLibrary -> ((Float, Float), Picture)
fetchSpriteFromLibraryF name lib = case lookup name lib of
  Just (Sprite ((w, h), pic)) -> ((fromIntegral w, fromIntegral h), pic)
  _                           -> error ("Could not find sprite " ++ name ++ " in library")

fetchAnimationFromLibrary :: String -> AssetLibrary -> ((Int, Int), [Picture])
fetchAnimationFromLibrary name lib = case lookup name lib of
  Just (Animation asset) -> asset
  _                      -> error ("Could not find animation " ++ name ++ " in library")

fetchAnimationFromLibraryF :: String -> AssetLibrary -> ((Float, Float), [Picture])
fetchAnimationFromLibraryF name lib = case lookup name lib of
  Just (Animation ((w, h), pic)) -> ((fromIntegral w, fromIntegral h), pic)
  _                              -> error ("Could not find animation " ++ name ++ " in library")

loadImage :: FilePath -> IO (Maybe DynamicImage)
loadImage filepath = do
  img <- readImage filepath
  return $ case img of
    Left  _   -> Nothing
    Right img -> Just img

getResolution :: DynamicImage -> (Int, Int)
getResolution img = (ww, hh)
  where
    imgRGBA8 = convertRGBA8 img
    ww = imageWidth imgRGBA8
    hh = imageHeight imgRGBA8

getPicture :: DynamicImage -> Picture
getPicture = fromImageRGBA8 . convertRGBA8

loadSprite
  :: FilePath -- Path to the sprite directory
  -> String   -- Sprite name
  -> IO (String, Asset)
loadSprite dir name = do
  img <- loadImage (dir ++ name ++ ".png")
  return (name, Sprite (proccess img))
  where
    proccess i = case i of
      Just pic -> (getResolution pic, align (getResolution pic) (getPicture pic))
      Nothing  -> ((0, 0), blank)

    align (w, h) =
      translate (fromIntegral w / 2) (fromIntegral h / 2)

loadAnimation
  :: FilePath -- Path to the animation directory
  -> String   -- Animation name
  -> Int      -- Number of frames
  -> IO (String, Asset)
loadAnimation dir name number = do
  pics <- sequence loadFrames
  return (name, Animation (resolution pics, map proccess pics))
  where
    loadFrames =
      map (\i -> loadImage (dir ++ name ++ show i ++ ".png"))
      [1..number]

    proccess = maybe blank (\x -> align (getResolution x) (getPicture x))
    resolution i = maximum (map (maybe (0, 0) getResolution) i)

    align (w, h) =
      translate (fromIntegral w / 2) (fromIntegral h / 2)

data HorizontalAlign = LeftAlign | CenterAlign | RightAlign
data VerticalAlign   = TopAlign  | MiddleAlign | BottomAlign

pictureAlign :: HorizontalAlign -> VerticalAlign -> Float -> Float -> Picture -> Picture
pictureAlign halign valign w h = translate x y
  where
    x = case halign of
      LeftAlign   -> 0
      CenterAlign -> -w / 2
      RightAlign  -> -w

    y = case valign of
      TopAlign    -> -h
      MiddleAlign -> -h / 2
      BottomAlign -> 0
