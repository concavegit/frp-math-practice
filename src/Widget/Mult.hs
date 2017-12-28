{-# LANGUAGE LambdaCase #-}

module Widget.Mult
  ( MultWidget(..)
  , multNetwork
  , newMultWidget
  , setupMultWidget
  ) where

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
  , multPoints :: StaticText ()
  , multTimer  :: StaticText ()
  , multGauge  :: Gauge ()
  , multTicker :: Timer
  , multG0     :: StdGen
  }

-- |Creates a new multiplication widget
newMultWidget :: Frame () -> IO MultWidget
newMultWidget = (>>= (\pane prompt input points wTimer gauge ticker -> MultWidget pane
                       <$> prompt
                       <*> input
                       <*> points
                       <*> wTimer
                       <*> gauge
                       <*> ticker
                       <*> getStdGen)
                 <$> id
                 <*> flip staticText []
                 <*> flip entry []
                 <*> flip staticText []
                 <*> flip staticText []
                 <*> flip (`hgauge` 90) []
                 <*> flip timer [])
  . flip panel []

-- |Sets the layout of the multiplication widget as well as properties
-- and focus.
setupMultWidget :: MultWidget -> IO ()
setupMultWidget w = do
  set (multPane w) [layout := margin 10 $ grid 10 10
                    [ [ widget (multPrompt w)
                      , label "="
                      , widget (multInput w)
                      , widget (multPoints w)
                      ]
                    , [ widget (multTimer w)
                      ]
                    , [ widget (multGauge w)]
                    ]]

  set (multInput w) [processEnter := True]
  set (multTicker w) [interval := 1000]
  set (multGauge w) [selection := 90]
  focusOn (multInput w)

-- |Sets up the reactive behaviors for the widget
multNetwork :: MultWidget -> MomentIO ()
multNetwork w = do
  bInput <- behaviorText (multInput w) ""
  eNext <- event0 (multInput w) command
  bClear <- stepper "" ("" <$ eNext)
  eTicker <- event0 (multTicker w) command
  bTimeLeft <- accumB 90 ((\n -> if n > 0 then n - 1 else 0) <$ eTicker)

  let bInProgress = (> 0) <$> bTimeLeft
      multProb = randMult (0, 99)

  bG <- accumB (multG0 w) (execState multProb <$ eNext)

  let bMult = evalState multProb <$> bG
      bPrompt = (\a b -> show a ++ " x " ++ show b)
        <$> (multA <$> bMult) <*> (multB <$> bMult)
      bResult = fmap <$> (flip (==) . multAns <$> bMult)
        <*> (readMaybe <$> bInput)
      eScore = (\case Just True -> (+1)
                      _         -> subtract 1)
        <$> bResult <@ eNext
  bScore <- accumB (0 :: Int) eScore

  sink (multInput w) [text :== bClear]
  sink (multInput w) [enabled :== bInProgress]
  sink (multPoints w) [text :== show <$> bScore]
  sink (multPrompt w) [text :== bPrompt]
  sink (multTimer w) [text :== show <$> bTimeLeft]
  sink (multGauge w) [selection :== bTimeLeft]
