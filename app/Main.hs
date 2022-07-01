module Main where

import           Fonts
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Environment
import           Outrun
import           Palettes

main :: IO ()
main = do
  screenRes <- getScreenSize

  fontWaffle <- loadFont
    "./images/fontWaffle/"
    "fontWaffle"
    "!#\"?&'(),-./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ[]abcdefghijklmnopqrstuvwxyz "

  outrunPlay screenRes (proccessFontColors [afr32_cyan] fontWaffle) sampleTrack
  --debugShowTextWithFont fontWaffle "hellow world"
