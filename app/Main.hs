module Main where

import           Control.Monad.State
import           Core
import           Graphics.UI.WX      hiding (Event)
import           Reactive.Banana
import           Reactive.Banana.WX
import           System.Random
import           Text.Read           (readMaybe)

main :: IO ()
main = start $ do
  f <- frame [text := "Mental Math"]
  prompt1 <- staticText f [fontSize := 32]
  prompt2 <- staticText f [fontSize := 32]
  multSymb <- staticText f [fontSize := 32, text := "x"]
  input <- entry f [processEnter := True]
  output <- staticText f [fontSize := 32]
  score <- staticText f []
  g0 <- getStdGen

  set f [layout := margin 10 $ row 30
         [ widget prompt1
         , widget multSymb
         , widget prompt2
         , label "="
         , minsize (sz 400 100) $ widget input
         , widget output
         , widget score
         ]]

  let networkDescription = do
        bInput <- behaviorText input ""
        eNext <- event0 input command
        eClear <- event0 input command
        bClear <- stepper "" ("" <$ eClear)
        upperLim <- accumB (20 :: Int) ((round . (* 1.5) . fromIntegral) <$ eNext)
        -- _ <- fmap ((fmap (flip ($)) upperLim) <*>) (ra)
        let bNewMult = curry (randMult :: (Int, Int) -> State StdGen Mult) 1 <$> upperLim
            eNewMult = bNewMult <@ eNext

        -- bFG <- accumB g0 (execState (randMult (1, 150)) <$ eNext)
        bG <- accumB g0 (execState <$> eNewMult)

        let bMult = evalState <$> bNewMult <*> bG
            bPrompt1 = show . multA <$> bMult
            bPrompt2 = show . multB <$> bMult
            bResult = fmap <$> (flip ($) . multAns <$> bMult) <*> (fmap (==) . readMaybe <$> bInput)
            showNumber = maybe "--" show
            eResultText = bResult <@ eNext

        eSteppedResult <- stepper "--" (showNumber <$> eResultText)
        sink prompt1 [ text :== bPrompt1 ]
        sink prompt2 [ text :== bPrompt2 ]
        sink output [ text :== eSteppedResult]
        sink input [text :== bClear]
  compile networkDescription >>= actuate
