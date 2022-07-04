module Fonts (module Fonts) where
import           Codec.Picture
import           Data.Bifunctor
import           Data.List
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Juicy
import Outrun.Data.AssetLibrary

-- | Multiplies two pixels by their values
multiplyPixelRGBA8 :: Color -> PixelRGBA8 -> PixelRGBA8
multiplyPixelRGBA8 color (PixelRGBA8 r2 g2 b2 a2) =
  PixelRGBA8 (f r1 r2) (f g1 g2) (f b1 b2) (f a1 a2)
  where
    (r1, g1, b1, a1) = rgbaOfColor color
    f a b = round (a * fromIntegral b)

-- | Multiplies an Image by solid color
multiplyImageRGBA8 :: Color -> Image PixelRGBA8 -> Image PixelRGBA8
multiplyImageRGBA8 = pixelMap . multiplyPixelRGBA8

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
      (align ww hh . fromImageRGBA8)
      . (`multiplyImageRGBA8` imgRGBA8)
      ) colors
    
    align w h =
      translate (fromIntegral w / 2) (fromIntegral h / 2)

proccessFont :: RawFont -> Font
proccessFont = proccessFontColors []

proccessFontColors :: [Color] -> RawFont -> Font
proccessFontColors colors =
  map (second (maybe emptyGlyth (proccessGlythColors uniqueColors)))
  where
    -- White is default color for the font
    -- which will be imported in any case
    uniqueColors = nub (white : colors)

data Label = Label
  { getLabel       :: Picture
  , getLabelWidth  :: Float
  , getLabelHeight :: Float
  }

-- | Renders label with the given font
labelWithFont :: Font -> String -> Label
labelWithFont font = labelWithFontExt font white 1 1 0.5

-- | Renders label with extra parameters
labelWithFontExt
  :: Font
  -> Color  -- Color of the label
  -> Float  -- Font horizontal scale
  -> Float  -- Font vertical scale
  -> Float  -- Separation between characters
  -> String -- Text to render
  -> Label  -- Width and picture of the label
labelWithFontExt font color xscale yscale sep string =
  Label (scale xscale yscale rendered) width height
  where
    glyths = map (getGlyth font) string

    widths  = map ((+sep) . fromIntegral . glythWidth) glyths
    heights = map (fromIntegral . glythHeight) glyths

    width  = sum widths * xscale
    height = maximum heights * yscale

    offsets = scanl (+) 0 widths

    pics     = map (getGlythImg color) glyths
    rendered = mconcat (zipWith (`translate` 0) offsets pics)

labelAlignment :: HorizontalAlign -> VerticalAlign -> Label -> Label
labelAlignment LeftAlign BottomAlign label = label
labelAlignment halign valign (Label pic w h) =
  Label (pictureAlign halign valign w h pic) w h

debugShowTextWithFont :: Font -> String -> IO ()
debugShowTextWithFont font label =
  display window (dark white) ((getLabel . labelWithFont font) label)
  where
     window = InWindow "Debug -- label with font" (1024, 768) (0, 0)
