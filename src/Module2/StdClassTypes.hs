module Module2.StdClassTypes (
  avg
) where

class Printable a where
  toString :: a -> [Char]
  
instance Printable Bool where
  toString True  = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (x, y) = "(" ++ (toString x) ++ "," ++ (toString y) ++ ")"

class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab x | doesEnrageMork x && doesEnrageGork x = stomp $ stab x
                | doesEnrageMork x                     = stomp x
                | doesEnrageGork x                     = stab x
                | otherwise                            = x

a = 12
b = 7.22
c = 4.12
d = 0.12

class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x | maxBound == x = minBound
          | otherwise     = succ x

  spred :: a -> a
  spred x | minBound == x = maxBound
          | otherwise     = pred x

avg :: Int -> Int -> Int -> Double
avg x y z = fromInteger (toInteger x + toInteger y + toInteger z) / 3.0