module Main where

import           Fonts
import           Outrun

main :: IO ()
main = do
  fontWaffle <- loadFont
    "./images/fontWaffle/"
    "fontWaffle"
    "!#\"?&'(),-./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ[]abcdefghijklmnopqrstuvwxyz "

  outrunPlayTest (proccessFont fontWaffle) sampleTrack
  --debugShowTextWithFont fontWaffle "hellow world"
