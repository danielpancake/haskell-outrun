module Main where
import           Fonts
import           Graphics.Gloss.Interface.Environment
import           Outrun
import           Outrun.Data.AssetLibrary
import           Palettes

main :: IO ()
main = do
  resolution <- getScreenSize

  assets <- sequence (loadSprite imageDir <$> sprites)

  veh_suv <- loadAnimation "./images/suv/" "veh_suv" 32

  fontWaffle <- loadFont
    "./images/fontWaffle/"
    "fontWaffle"
    "!#\"?&'(),-./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ[]abcdefghijklmnopqrstuvwxyz "

  outrunPlay
    resolution
    (veh_suv : assets)
    (proccessFontColors [afr32_olive, afr32_cyan, afr32_turmeric] fontWaffle)
    (desertTrack assets)
  where
    imageDir = "./images/"
    sprites  = ["background", "clouds", "cactus", "finish", "right_arrow"]
