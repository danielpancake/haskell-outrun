module Main where

import           Data.Maybe
import           Fonts
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Environment
import           Graphics.Gloss.Juicy
import           Outrun
import           Outrun.Data.AssetLibrary
import           Palettes

main :: IO ()
main = do
  resolution <- getScreenSize

  backgound <- loadSprite "./images/" "background"
  veh_suv   <- loadAnimation "./images/suv/" "veh_suv" 32

  fontWaffle <- loadFont
    "./images/fontWaffle/"
    "fontWaffle"
    "!#\"?&'(),-./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ[]abcdefghijklmnopqrstuvwxyz "

  outrunPlay
    resolution
    [backgound, veh_suv]
    (proccessFontColors [afr32_olive, afr32_cyan, afr32_turmeric] fontWaffle)
    sampleTrack
