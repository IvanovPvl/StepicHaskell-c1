module DataTypes where

-- 4.1

data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

stringToColor :: String -> Color
stringToColor c = case c of
                    "Red"   -> Red
                    "Green" -> Green
                    "Blue"  -> Blue


data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error _ = GT
cmp Info _ = LT
cmp _ Info = GT
cmp _ Error = LT

-- data Result = Fail | Success

-- processData :: SomeData -> String
-- processData d = case doSomeWork d of
--     (Success, _)      -> "Success"
--     (Fail, errorCode) -> "Fail: " ++ show errorCode

-- 4.2

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle a b) = a * b

-- data Result' = Fail' Int | Success'

-- instance Show Result' where
--     show Success'          = "Success"
--     show (Fail' errorCode) = "Fail: " ++ show errorCode

-- doSomeWork' :: SomeData -> Result'
-- doSomeWork' d = case doSomeWork d of
--     (Success, _)      -> Success'
--     (Fail, errorCode) -> Fail' errorCode

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) | a == b    = True
                         | otherwise = False
isSquare _ = False

data Bit = Zero | One deriving (Show)
data Sign = Minus | Plus deriving (Show)
data Z = Z Sign [Bit] deriving (Show)

zToInt :: Z -> Int
zToInt (Z Plus bits) = helper bits 0 0
zToInt (Z Minus bits) = -(helper bits 0 0)

helper []        _ acc = acc
helper (One:xs)  i acc = helper xs (i + 1) (acc + 2 ^ i)
helper (Zero:xs) i acc = helper xs (i + 1) acc

intToZ :: Int -> Z
intToZ n = Z s $ reverse $ helper' (abs n) [] where
    s = case signum n of
        1 -> Plus
        _ -> Minus

helper' 0 _   = [Zero]
helper' 1 acc = One : acc
helper' n acc = helper' (n `div` 2) (bit : acc) where
    bit = case n `mod` 2 of
        1 -> One
        _ -> Zero

add :: Z -> Z -> Z
add x y = intToZ $ zToInt x + zToInt y

mul :: Z -> Z -> Z
mul = intToZ $ zToInt x * zToInt y
