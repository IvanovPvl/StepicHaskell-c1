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
integration f a b = let
  chunks = 1000
  step = (b - a) / chunks
  sum acc _ 0 = acc
  sum acc x n = sum (acc + f x) (x + step) (n - 1) in
  ((f a + f b) / 2 + sum 0 (a + step) (chunks - 1)) * step