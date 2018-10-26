module DataTypes where

-- import Data.Time.Clock
-- import Data.Time.Format
-- import System.Locale
import Data.List (findIndex)
import Data.Char (isDigit, isSpace)

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

-- distance :: Point -> Point -> Double
-- distance (Point x1 y1) (Point x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

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
mul x y = intToZ $ zToInt x * zToInt y

-- 4.3

-- timeToString :: UTCTime -> String
-- timeToString = formatTime defaultTimeLocale "%a %d %T"

-- data LogLevel = Error | Warning | Info deriving (Show)

-- data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

-- logLevelToString :: LogLevel -> String
-- logLevelToString = show

-- logEntryToString :: LogEntry -> String
-- logEntryToString log = let
--     timeStr = timeToString $ timestamp log
--     levelStr = logLevelToString $ logLevel log
--     in timeStr ++ ": " ++ levelStr ++ ": " ++ message log

-- data Person = Person { firstName :: String, lastName :: String, age :: Int }

-- updateLastName :: Person -> Person -> Person
-- updateLastName p1 p2 = p2 { lastName = lastName p1 }

-- abbrFirstName :: Person -> Person
-- abbrFirstName :: Person -> Person
-- abbrFirstName p@(Person { firstName = fn }) = p { firstName = newFirstName fn } where
--     newFirstName fn | length fn <= 2 = fn
--                     | otherwise = head fn : "."

-- data Coord a = Coord a a

-- distance :: Coord Double -> Coord Double -> Double
-- distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

-- manhDistance :: Coord Int -> Coord Int -> Int
-- manhDistance (Coord x1 y1) (Coord x2 y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

-- 4.4

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) | isDigit x = Just x
                 | otherwise = findDigit xs

findDigitOrX :: [Char] -> Char
findDigitOrX s = case findDigit s of
    Just c   -> c
    Nothing  -> 'X'

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

--

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Show)

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show)

parsePerson :: String -> Either Error Person
parsePerson s = case parsePairs s of
    Left ParsingError -> Left ParsingError
    Right (("firstName", fn):("lastName", ln):("age", a):_) ->
        case parseAge a of
            Nothing  -> Left $ IncorrectDataError a
            Just age -> Right $ Person { firstName = fn, lastName = ln, age = age }
    Right _ -> Left IncompleteDataError

parseAge :: String -> Maybe Int
parseAge s = case all isDigit s of
    True -> Just $ read s
    _    -> Nothing

parsePairs :: String -> Either Error [(String, String)]
parsePairs s = foldr f (Right []) $ lines s where
  f _ (Left e) = Left e
  f line (Right xs) = case getPair line of
    Just p  -> Right $ p : xs
    Nothing -> Left ParsingError

getPair :: String -> Maybe (String, String)
getPair s = case findIndex (== '=') s of
    Just i -> r where
        (k, v) = splitAt i s
        r = case (trim k, trim $ tail v) of
            ("", _) -> Nothing
            (_, "") -> Nothing
            (a, b)  -> Just (a, b)
    _ -> Nothing

trim :: String -> String
trim = f . f where
    f = reverse . dropWhile isSpace
