module Module3.ListFunctions (
  addTwoElements
  , nTimes
  ) where

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y n = x : y : n

nTimes :: a -> Int -> [a]
nTimes x n = take n $ repeat x