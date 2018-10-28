module DataTypes where

-- import Data.Time.Clock
-- import Data.Time.Format
-- import System.Locale
import Data.List (findIndex)
import Data.Char (isDigit, isSpace)
import Prelude hiding (lookup)
import qualified Data.List as L

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

eitherToMaybe :: Either a a -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

-- 4.5

data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x $ toList xs

data Nat = Zero | Suc Nat deriving (Show)

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero Zero = Zero
add x Zero = x
add Zero x = x
add (Suc n) x = add n (Suc x)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul _ Zero = Zero
mul (Suc Zero) n = n
mul n (Suc Zero) = n
mul x y = helper x Zero y where
    helper t acc Zero = acc
    helper t acc (Suc s) = helper t (add acc t) s

fac :: Nat -> Nat
fac Zero = Suc Zero
fac (Suc Zero) = Suc Zero
fac n = helper n (Suc Zero) where
    helper (Suc Zero) acc = acc
    helper t@(Suc n) acc = helper n $ mul acc t

--

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf x) = 0
height (Node x y) = 1 + max (height x) (height y)

size :: Tree a -> Int
size (Leaf x) = 1
size (Node x y) = 1 + size x + size y

avg :: Tree Int -> Int
avg t =
    let (c, s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int, Int)
    go (Leaf x) = (1, x)
    go (Node x y) = (c1 + c2, s1 + s2) where
        (c1, s1) = go x
        (c2, s2) = go y

--

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq, Show)

instance Semigroup Xor where
    mempty = Xor False
    Xor x `mappend` Xor y = Xor $ x /= y

-- newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
--     deriving (Eq, Show)

-- instance Monoid a => Monoid (Maybe' a) where
--     mempty = Maybe' $ Just mempty
--     Maybe' Nothing `mappend` _ = Maybe' Nothing
--     _ `mappend` Maybe' Nothing = Maybe' Nothing
--     Maybe' m1 `mappend` Maybe' m2 = Maybe' $ m1 `mappend` m2

-- class MapLike m where
--     empty :: m k v
--     lookup :: Ord k => k -> m k v -> Maybe v
--     insert :: Ord k => k -> v -> m k v -> m k v
--     delete :: Ord k => k -> m k v -> m k v
--     fromList :: Ord k => [(k,v)] -> m k v
--     fromList [] = empty
--     fromList ((k, v):xs) = insert k v (fromList xs)

-- newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
--     deriving (Eq, Show)

-- instance MapLike ListMap where
--     empty = ListMap []
--     lookup k (ListMap m) = L.lookup k m
--     insert k v mo@(ListMap m) = case lookup k mo of
--         Nothing -> ListMap $ m ++ [(k, v)]
--         Just _  -> ListMap $ insert' k v m  where
--             insert' k v [] = []
--             insert' k v ((k1, v1):xs) | k == k1 = (k, v) : xs
--                                       | otherwise = (k1, v1) : insert' k v xs

--     delete k (ListMap []) = ListMap []
--     delete k (ListMap m) = ListMap $ delete' k m where
--         delete' k [] = []
--         delete' k ((k1, v1):xs) | k == k1 = xs
--                               | otherwise = (k1, v1) : delete' k xs

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap $ \_ -> Nothing
    lookup k (ArrowMap f) = f k
    insert k v (ArrowMap f) = ArrowMap $ \k1 -> if k1 == k then Just v else (f k1)
    delete k (ArrowMap f) = ArrowMap $ \k1 -> if k1 == k then Nothing else (f k1)
        