module Widget.Mult where

import           Control.Monad.State
import           Core
import           Graphics.UI.WX      hiding (Event)
import           Reactive.Banana
import           Reactive.Banana.WX
import           System.Random
import           Text.Read           (readMaybe)

data MultWidget = MultWidget
  { multPane   :: Panel ()
  , multPrompt :: StaticText ()
  , multInput  :: TextCtrl ()
  , multOutput :: StaticText ()
  , multG0     :: StdGen
  }

widgetProps :: MultWidget -> IO ()
widgetProps w = set (multPane w) [layout := margin 10 $ row 10
                                      [ widget (multPrompt w)
                                      , label "="
                                      , widget (multInput w)
                                      , widget (multOutput w)]]
  >> set (multInput w) [processEnter := True]

multNetwork :: MultWidget -> MomentIO ()
multNetwork w = do
  bInput <- behaviorText (multInput w) ""
  eNext <- event0 (multInput w) command
  bClear <- stepper  "" ("" <$ eNext)
  let multProb = randMult (0, 99)
  bG <- accumB (multG0 w) (execState multProb <$ eNext)
  let bMult = evalState (randMult (0, 99)) <$> bG
      bPrompt = (\a b -> show a ++ " x " ++ show b)
        <$> (multA <$> bMult) <*> (multB <$> bMult)
      bResult = fmap <$> (flip (==) . multAns <$> bMult)
        <*> (readMaybe <$> bInput)
  bSteppedResult <- stepper "--" (maybe "--" show <$> (bResult <@ eNext))
  sink (multPrompt w) [text :== bPrompt]
  sink (multOutput w) [text :==  bSteppedResult]
  sink (multInput w) [text :== bClear]
