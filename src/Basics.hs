module Basics where

import Data.Function (on)

-- 1

getSecondFrom :: a -> b -> c -> b
getSecondFrom x y z = y

multSecond = g `on` h
g = (*)
h = snd

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 f op x y z = f (op x) (op y) (op z)

-- 2

doItYourself = f' . g' . h'
f' = logBase 2
g' = (^3)
h' = max 42

-- 3

class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (x, y) = "(" ++ toString x ++ "," ++ toString y ++ ")"

-- 4

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x | doesEnrageGork x && doesEnrageMork x = stomp $ stab x
                  | doesEnrageMork x = stomp x
                  | doesEnrageGork x = stab x
                  | otherwise = x

class (Bounded a, Enum a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc n | maxBound == n = minBound
          | otherwise = succ n

  spred :: a -> a
  spred n | minBound == n = maxBound
          | otherwise = pred n

avg :: Int -> Int -> Int -> Double
avg x y z = fromInteger (toInteger x + toInteger y + toInteger z) / 3.0
