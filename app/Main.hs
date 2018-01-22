module Main where

import Module2

main :: IO ()
main = print res where
  sum3Squares = (\x y z -> x + y + z) `on3` (^2)
  res = sum3Squares 1 2 3