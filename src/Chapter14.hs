module Chapter14 (
  twoDigits2Int
  , dist
  ) where

import Data.Char

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y
  | isDigit x && isDigit y = (digitToInt x) * 10 + digitToInt y
  | otherwise = 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2