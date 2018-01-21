module Chapter16 (
  seqA
  , sum'n'count
  , integration
) where

import Data.Char

seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | otherwise = let
         recc a1 a2 a3 0 = a1
         recc a1 a2 a3 n = recc a2 a3 (a2 + a3 - 2 * a1) (n - 1)
         in recc 1 2 3 n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (toInteger $ sum digits, toInteger $ length $ digits) where
  digits = numbers (show $ abs x) []
  numbers x nums | null x    = nums
                 | otherwise = numbers (tail x) ((digitToInt $ head x) : nums)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b | a < b = step * ((f a + f b) / 2 + sum values)
                  | otherwise = (-1) * step * ((f a + f b) / 2 + sum values)
  where
    values = [f (n * step) | n <- [1..999]]
    step = (b - a) / 1000