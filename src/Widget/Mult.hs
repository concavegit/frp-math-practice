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
  , multGauge  :: Gauge ()
  , multTicker :: Timer
  , multReset  :: Button ()
  , multG0     :: StdGen
  }

-- |Creates a new multiplication widget
newMultWidget :: Frame () -> IO MultWidget
newMultWidget = (>>= (\pane prompt input points gauge ticker reset
                      -> MultWidget pane
                       <$> prompt
                       <*> input
                       <*> points
                       <*> gauge
                       <*> ticker
                       <*> reset
                       <*> getStdGen)

                 <$> id
                 <*> flip staticText []
                 <*> flip entry []
                 <*> flip staticText []
                 <*> flip (`vgauge` 90) []
                 <*> flip timer []
                 <*> flip button [])
  . flip panel []

-- |Sets the layout of the multiplication widget as well as properties
-- and focus.
setupMultWidget :: MultWidget -> IO ()
setupMultWidget w = do
  set (multPane w) [layout := (lay <$> multPrompt
                               <*> multInput
                               <*> multPoints
                               <*> multGauge
                               <*> multReset) w]

  set (multInput w) [processEnter := True]
  set (multGauge w) [selection := 90]
  set (multReset w) [text := "Restart"]
  focusOn (multInput w)

lay :: (Widget w1, Widget w2, Widget w3, Widget w4, Widget w5)
  => w1 -> w2 -> w3 -> w4 -> w5 -> Layout
lay prompt input points gauge reset = margin 10 $ row 10
  [ widget gauge
  , column 10
    [ widget prompt
    , widget input
    , widget points
    , widget reset
    ]
  ]

-- |Sets up the reactive behaviors for the widget
multNetwork :: MultWidget -> MomentIO ()
multNetwork w = do
  bInput <- behaviorText (multInput w) ""
  eNext <- event0 (multInput w) command
  eTicker <- event0 (multTicker w) command
  eResetClicked <- event0 (multReset w) command

  let eResetTimer = const 90 <$ eResetClicked
      eCountTimer = (\n -> if n > 0 then n - 1 else 0) <$ eTicker
      eResetPoints = const 0 <$ eResetClicked
      eClear = "" <$ eNext

  bClear <- stepper "" eClear

  bTimeLeft <- accumB 90 $ unionWith const eCountTimer eResetTimer

  let bInProgress = (> 0) <$> bTimeLeft
      multProb = randMult (0, 99)
      ct = red
      c0 = blue
      bColor = colorGrad ct c0 <$> bTimeLeft
      eResetState = execState multProb <$ eResetClicked
      eNextState = execState multProb <$ eNext

  bG <- accumB (multG0 w) $ unionWith const eResetState eNextState

  let bMult = evalState multProb <$> bG
      bPrompt = (\a b -> show a ++ " x " ++ show b)
        <$> (multA <$> bMult) <*> (multB <$> bMult)
      bResult = fmap <$> (flip (==) . multAns <$> bMult)
        <*> (readMaybe <$> bInput)
      eScore = (\case Just True -> (+1)
                      _         -> subtract 1)
        <$> bResult <@ eNext

  bScore <- accumB (0 :: Int) (unionWith const eResetPoints eScore)

  sink (multInput w) [ text :== bClear
                     , enabled :== bInProgress
                     ]
  sink (multPoints w) [text :== ("Score: " ++) . show <$> bScore]
  sink (multPrompt w) [text :== bPrompt]
  sink (multGauge w) [ color :== bColor
                     , selection :== bTimeLeft
                     ]
  sink (multReset w) [enabled :== not <$> bInProgress]

colorGrad :: Color -> Color -> Int -> Color
colorGrad c0 ct i = rgb (pBetween (colorRed c0) (colorRed ct))
  (pBetween (colorGreen c0) (colorGreen ct))
  (pBetween (colorBlue c0) (colorBlue ct))
  where pBetween x y = x + round (fromIntegral ((y - x) * i) / 90 :: Double)
