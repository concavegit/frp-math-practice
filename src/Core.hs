module Core
  ( randMult
  , Mult (..)
  ) where

import           Control.Monad.State
import           System.Random

data Mult = Mult { multA   :: Int
                 , multB   :: Int
                 , multAns :: Int
                 } deriving Show

rand :: (RandomGen g, Random a) => (a, a) -> State g a
rand = state . randomR

randMult :: (RandomGen g) => (Int, Int) -> State g Mult
randMult ns = (\a b -> Mult a b (a * b)) <$> rand ns <*> rand ns
