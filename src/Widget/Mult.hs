{-# LANGUAGE LambdaCase #-}

module Widget.Mult
  ( MultWidget(..)
  , multNetwork
  , newMultWidget
  , setupMultWidget
  ) where

import           Control.Monad.State
import           Core
import           Data.Time.Clock
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
  , multG0     :: StdGen
  }

-- |Creates a new multiplication widget
newMultWidget :: Frame () -> IO MultWidget
newMultWidget = (>>= (\pane prompt input output points -> MultWidget pane
                       <$> prompt
                       <*> input
                       <*> output
                       <*> points
                       <*> getStdGen)
                 <$> id
                 <*> flip staticText []
                 <*> flip entry []
                 <*> flip staticText []
                 <*> flip staticText [])
  . flip panel []

-- |Sets the layout of the multiplication widget as well as properties
-- and focus.
setupMultWidget :: MultWidget -> IO ()
setupMultWidget w = do
  set (multPane w) [layout := margin 10 $ row 10
                                      [ widget (multPrompt w)
                                      , label "="
                                      , widget (multInput w)
                                      , widget (multPoints w)
                                      , widget (multOutput w)]]
  set (multInput w) [processEnter := True]
  focusOn (multInput w)

-- |Sets up the reactive behaviors for the widget
multNetwork :: MultWidget -> MomentIO ()
multNetwork w = do
  bInput <- behaviorText (multInput w) ""
  eNext <- event0 (multInput w) command
  bClear <- stepper  "" ("" <$ eNext)

  let multProb = randMult (0, 99)

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

  sink (multPrompt w) [text :== bPrompt]
  sink (multOutput w) [text :==  bSteppedResult]
  sink (multPoints w) [text :== show <$> bScore]
  sink (multInput w) [text :== bClear]

timer :: Int -> MomentIO (Behavior Int)
timer t = do
  initialTime <- liftIO getCurrentTime
  currentTime <- fromPoll getCurrentTime
  pure $ subtract t . round . flip diffUTCTime initialTime <$> currentTime
