module Main where
import           Fonts
import           Graphics.Gloss.Interface.Environment
import           Outrun
import           Outrun.Data.AssetLibrary
import           Palettes

main :: IO ()
main = do
  resolution <- getScreenSize

  backgound <- loadSprite "./images/" "background"
  cactus    <- loadSprite "./images/" "cactus"
  clouds    <- loadSprite "./images/" "clouds"
  finish    <- loadSprite "./images/" "finish"

  veh_suv   <- loadAnimation "./images/suv/" "veh_suv" 32

  fontWaffle <- loadFont
    "./images/fontWaffle/"
    "fontWaffle"
    "!#\"?&'(),-./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ[]abcdefghijklmnopqrstuvwxyz "

  outrunPlay
    resolution
    [backgound, clouds, veh_suv, finish, cactus] 
    (proccessFontColors [afr32_olive, afr32_cyan, afr32_turmeric] fontWaffle)
    (desertTrack [cactus, finish])