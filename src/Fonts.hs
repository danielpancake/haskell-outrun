module Fonts (module Fonts) where

import           Codec.Picture
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Juicy

data Glyth = Glyth
  { glythWidth  :: Int
  , glythHeight :: Int
  , glythPic    :: Picture
  }

emptyGlyth :: Glyth
emptyGlyth = Glyth 0 0 blank

getGlyth :: Font -> Char -> Glyth
getGlyth font = fromMaybe emptyGlyth . (`lookup` font)

type Font  = [(Char, Glyth)]

bottomLeftOriginPic :: Int -> Int -> Picture -> Picture
bottomLeftOriginPic w h =
  translate (fromIntegral w / 2) (fromIntegral h / 2)

loadImage :: FilePath -> IO (Maybe Glyth)
loadImage filepath = do
  img <- readImage filepath
  return $ case img of
    Left  _   -> Nothing
    Right img -> Just (Glyth width height pic)
      where
        imgRGBA = convertRGBA8 img
        width   = imageWidth imgRGBA
        height  = imageHeight imgRGBA
        pic     = bottomLeftOriginPic width height $
                  fromImageRGBA8 imgRGBA

loadFont
  -- Font should be splitted into collection of files (*.png per character)
  -- Naming of the files should be following: "{fontname}{index}.png"
  -- Each index corresponds to one character of the second argument
  :: FilePath -- Path to the font directory
  -> String -- Font name
  -> String -- String from which character order is taken
  -> IO Font
loadFont dir font order = do
  glyths <- sequence fontPics
  return (zip order (map (fromMaybe emptyGlyth) glyths))
  where
    fontPics = zipWith
      (\i _ -> loadImage (dir ++ font ++ show i ++ ".png"))
      [1..] order

textWithFont :: Font -> String -> Picture
textWithFont font = textWithFontExt font 0.5

textWithFontExt :: Font -> Float -> String -> Picture
textWithFontExt font sep =
  foldr (
    (\(Glyth hoff _ pic1) pic2 ->
      pic1
      <>
      translate (fromIntegral hoff + sep) 0 pic2
    ) . getGlyth font) blank

debugShowTextWithFont :: Font -> String -> IO ()
debugShowTextWithFont font text =
  display window (dark white) (textWithFont font text)
  where
     window = InWindow "Debug -- text with font" (1024, 768) (0, 0)
