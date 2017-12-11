module Main where

import           Control.Monad.State
import           Core
import           Data.Maybe
import           Graphics.UI.WX      hiding (Event)
import           Reactive.Banana
import           Reactive.Banana.WX
import           System.Random
import           Text.Read           (readMaybe)

main :: IO ()
main = start $ do
  f <- frame [text := "Mental Math"]
  prompt1 <- staticText f []
  prompt2 <- staticText f []
  input <- entry f []
  output <- staticText f []
  bnext <- button f [text := "Next"]
  g0 <- getStdGen

  set f [layout := margin 10 $ row 10
         [ widget prompt1
         , label "x"
         , widget prompt2
         , label "="
         , widget input
         , widget output
         , widget bnext
         ]]
  let networkDescription = do
        bInput <- behaviorText input ""
        eBnext <- event0 bnext command
        bG <- accumB g0 (execState (randMult (0, 99)) <$ eBnext)

        let mult = evalState (randMult (0, 99)) <$> bG
            bPrompt1 = show . multA <$> mult
            bPrompt2 = show . multB <$> mult
            result = fmap <$> (flip ($) . multAns <$> mult) <*> (fmap (==) . readMaybe <$> bInput)
            showNumber = maybe "--" show
        sink prompt1 [ text :== bPrompt1 ]
        sink prompt2 [ text :== bPrompt2 ]
        sink output [ text :== showNumber <$> result]
  compile networkDescription >>= actuate
