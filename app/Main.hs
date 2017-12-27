module Main where

import           Graphics.UI.WX     hiding (Event)
import           Reactive.Banana
import           Reactive.Banana.WX
import           Widget.Mult

main :: IO ()
main = start $ do
  f <- frame [text := "Mental Math"]
  multW <- newMultWidget f
  setupMultWidget multW
  compile (multNetwork multW) >>= actuate
