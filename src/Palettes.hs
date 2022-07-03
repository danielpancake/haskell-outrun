module Palettes (module Palettes) where
import           Graphics.Gloss

-- | Converts four integer RGBA values to a Gloss.Color
makeColor8 :: Int -> Int -> Int -> Int -> Color
makeColor8 r g b a =
  makeColor
    (fromIntegral r / 255)
    (fromIntegral g / 255)
    (fromIntegral b / 255)
    (fromIntegral a / 255)

-- AFR-32 PALETTE
-- Created by Alpha6
-- An earthy palette with plenty of natural colors
afr32_white,
  afr32_dorado,
  afr32_darkdorado,
  afr32_sungrass,
  afr32_olive,
  afr32_red,
  afr32_hippieblue,
  afr32_cyan
  :: Color

afr32_white      = makeColor8 236 235 231 255
afr32_dorado     = makeColor8 105 91  89  255
afr32_darkdorado = makeColor8 79  66  64  255
afr32_sungrass   = makeColor8 147 148 70  255
afr32_olive      = makeColor8 118 119 29  255
afr32_red        = makeColor8 176 34  34  255
afr32_hippieblue = makeColor8 73  163 180 255
afr32_cyan       = makeColor8 29  233 222 255
