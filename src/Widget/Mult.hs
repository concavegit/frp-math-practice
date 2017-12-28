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
  , multOutput :: StaticText ()
  , multPoints :: StaticText ()
  , multTimer  :: StaticText ()
  , multTicker :: Timer
  , multG0     :: StdGen
  }

-- |Creates a new multiplication widget
newMultWidget :: Frame () -> IO MultWidget
newMultWidget = (>>= (\pane prompt input output points wTimer ticker -> MultWidget pane
                       <$> prompt
                       <*> input
                       <*> output
                       <*> points
                       <*> wTimer
                       <*> ticker
                       <*> getStdGen)
                 <$> id
                 <*> flip staticText []
                 <*> flip entry []
                 <*> flip staticText []
                 <*> flip staticText []
                 <*> flip staticText []
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
                    , [ widget (multOutput w)
                      , widget (multTimer w)
                      ]
                    ]]

  set (multInput w) [processEnter := True]
  set (multTicker w) [interval := 1000]
  focusOn (multInput w)

-- |Sets up the reactive behaviors for the widget
multNetwork :: MultWidget -> MomentIO ()
multNetwork w = do
  bInput <- behaviorText (multInput w) ""
  eNext <- event0 (multInput w) command
  bClear <- stepper "" ("" <$ eNext)
  eTicker <- event0 (multTicker w) command
  bTimeLeft <- accumB (90 :: Int) (subtract 1 <$ eTicker)

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
  bSteppedResult <- stepper "--" (maybe "--" show <$> (bResult <@ eNext))

  sink (multInput w) [text :== bClear]
  sink (multInput w) [enabled :== bInProgress]
  sink (multOutput w) [text :==  bSteppedResult]
  sink (multPoints w) [text :== show <$> bScore]
  sink (multPrompt w) [text :== bPrompt]
  sink (multTimer w) [text :== show <$> bTimeLeft]
