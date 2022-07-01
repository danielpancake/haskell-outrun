module Fonts (module Fonts) where

import           Codec.Picture
import           Data.Bifunctor
import           Data.List
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Juicy

-- | Aligns a picture to its botton left corner
bottomLeftOriginPic :: Int -> Int -> Picture -> Picture
bottomLeftOriginPic w h =
  translate (fromIntegral w / 2) (fromIntegral h / 2)

-- | Converts Gloss.Color to PixelRGBA8
colorToRGBA8 :: Color -> PixelRGBA8
colorToRGBA8 color =
  PixelRGBA8 (round r) (round g) (round b) (round a)
  where
    (r, g, b, a) = rgbaOfColor color

-- | Multiplies two pixels by their values
multiplyPixelRGBA8 :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
multiplyPixelRGBA8 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  PixelRGBA8 (r1 * r2) (g1 * g2) (b1 * b2) (a1 * a2)

-- | Multiplies an Image by solid color
multiplyImageRGBA8 :: Color -> Image PixelRGBA8 -> Image PixelRGBA8
multiplyImageRGBA8 color =
  pixelMap (multiplyPixelRGBA8 (colorToRGBA8 color))

data Glyth = Glyth
  { glythWidth  :: Int
  , glythHeight :: Int
  , glythPic    :: [ (Color, Picture) ]
  }

-- Before using a font (rawfont) it should be proccessed
-- Proccessing makes it possible to use font of different colors
type RawFont = [ (Char, Maybe DynamicImage) ]
type Font    = [ (Char, Glyth) ]

emptyGlyth :: Glyth
emptyGlyth = Glyth 0 0 []

getGlyth :: Font -> Char -> Glyth
getGlyth font = fromMaybe emptyGlyth . (`lookup` font)

getGlythImg :: Color -> Glyth -> Picture
getGlythImg color glyth = case lookup color colPics of
  Just pic -> pic
  Nothing  -> fromMaybe blank (lookup white colPics)
  where
    colPics = glythPic glyth

loadImage :: FilePath -> IO (Maybe DynamicImage)
loadImage filepath = do
  img <- readImage filepath
  return $ case img of
    Left  _   -> Nothing
    Right img -> Just img

loadFont
  -- Font should be splitted into collection of files (*.png per character)
  -- Naming of the files should be following: "{fontname}{index}.png"
  -- Each index corresponds to one character of the second argument
  :: FilePath -- Path to the font directory
  -> String -- Font name
  -> String -- String from which character order is taken
  -> IO RawFont
loadFont dir font order = do
  rawGlyths <- sequence fontImgs
  return (zip order rawGlyths)
  where
    fontImgs = zipWith
      (\i _ -> loadImage (dir ++ font ++ show i ++ ".png"))
      [1..] order

proccessGlythColors :: [Color] -> DynamicImage -> Glyth
proccessGlythColors colors img =
  Glyth ww hh (zip colors pics)
  where
    imgRGBA8 = convertRGBA8 img

    ww = imageWidth imgRGBA8
    hh = imageHeight imgRGBA8

    pics = map (
      (bottomLeftOriginPic ww hh . fromImageRGBA8)
      . (`multiplyImageRGBA8` imgRGBA8)
      ) colors

proccessFont :: RawFont -> Font
proccessFont = proccessFontColors []

proccessFontColors :: [Color] -> RawFont -> Font
proccessFontColors colors =
  map (second (maybe emptyGlyth (proccessGlythColors uniqueColors)))
  where
    -- White is default color for the font
    -- which will be imported in any case
    uniqueColors = nub (white : colors)

-- | Renders text with the given font
textWithFont :: Font -> String -> Picture
textWithFont font = textWithFontExt font white 1 1 0.5

-- | Renders text with extra parameters
textWithFontExt
  :: Font
  -> Color  -- Color of the text
  -> Float  -- Font horizontal scale
  -> Float  -- Font vertical scale
  -> Float  -- Separation between characters
  -> String -- Text to render
  -> Picture
textWithFontExt font color xscale yscale sep =
  scale xscale yscale .
  foldr (
    (\glyth otherGlyths ->
      getGlythImg color glyth
      <>
      translate (fromIntegral (glythWidth glyth) + sep) 0 otherGlyths
    ) . getGlyth font) blank

debugShowTextWithFont :: Font -> String -> IO ()
debugShowTextWithFont font text =
  display window (dark white) (textWithFont font text)
  where
     window = InWindow "Debug -- text with font" (1024, 768) (0, 0)
