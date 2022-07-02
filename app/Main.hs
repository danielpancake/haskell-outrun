module Main where

import           Data.Maybe
import           Fonts
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Environment
import           Graphics.Gloss.Juicy
import           Outrun
import           Palettes

main :: IO ()
main = do
  screenRes <- getScreenSize

  backgound <- loadJuicyPNG "./images/background.png"

  fontWaffle <- loadFont
    "./images/fontWaffle/"
    "fontWaffle"
    "!#\"?&'(),-./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ[]abcdefghijklmnopqrstuvwxyz "

  outrunPlay
    (fromMaybe blank backgound)
    screenRes
    (proccessFontColors [afr32_cyan] fontWaffle)
    sampleTrack
  --debugShowTextWithFont fontWaffle "hellow world"
