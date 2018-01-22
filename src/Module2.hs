module Module2 (
  multSecond
  , on3
) where

import Data.Function

multSecond = g `on` h

g = (*)
h = snd

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 f op x y z = f (op x) (op y) (op z))