module Core
  ( randMult
  , randTrig
  , Mult (..)
  , Trig (..)
  ) where

import           Control.Arrow
import           Control.Monad.State
import           System.Random

data Mult = Mult { multA   :: Int
                 , multB   :: Int
                 , multAns :: Int
                 }

data Trig = Trig { trigAngle :: Int
                 , trigOp    :: TrigOp
                 , trigAns   :: Int
                 }

data TrigOp = Cos | Sin | Tan deriving (Enum, Show)

instance Random TrigOp where
  randomR = (. randomR (0, 5)) . first . (!!) . cycle . uncurry enumFromTo
  random = randomR (Cos, Tan)

rand :: (RandomGen g, Random a) => (a, a) -> State g a
rand = state . randomR

randMult :: RandomGen g => (Int, Int) -> State g Mult
randMult ns = (\a b -> Mult a b (a * b)) <$> rand ns <*> rand ns

randTrig :: RandomGen g => State g Trig
randTrig = (\a -> Trig a <$> toEnum <*> (round :: Integral a => Double -> a)
             . (* 1000) . ($ fromIntegral a) . ([cos, sin, tan] !!))
  <$> rand (0, 90) <*> rand (0, 2)
