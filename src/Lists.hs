module Lists where

-- 1

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y xs = x : y : xs

nTimes:: a -> Int -> [a]
nTimes a n = take n $ repeat a

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) | odd x = x : oddsOnly xs
                | otherwise = oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = reverse xs == xs

sum2 :: Num a => [a] -> [a] -> [a]
sum2 [] [] = []
sum2 (x:xs) [] = x : sum2 xs []
sum2 [] (y:ys) = y : sum2 [] ys
sum2 (x:xs) (y:ys) = x + y : sum2 xs ys

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = sum2 xs $ sum2 ys zs

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems xs@(x:_) = let
    spanned = span (== x) xs
    in fst spanned : (groupElems $ snd spanned)

-- 2

readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs) | p1 x || p2 x = x : filterDisj p1 p2 xs
                        | otherwise = filterDisj p1 p2 xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort xs = let
    pivot = head xs
    t = tail xs
    less = filter (<= pivot) t
    greater = filter (> pivot) t
    in qsort less ++ [pivot] ++ qsort greater

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

perms :: [a] -> [[a]]
perms = undefined

delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> maximum [x, y, z])

-- 3

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

repeatHelper = id

coins = [2, 3, 7]

-- 4

concatList :: [[a]] -> [a]
concatList = foldr (++) []

lengthList :: [a] -> Int
lengthList = foldr (\_ l -> l + 1) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr f 0 where
    f x s | odd x = x + s
          | otherwise = s

-- 5

meanList :: [Double] -> Double
meanList = uncurry (/) . foldr (\x (s, c) -> (s + x, c + 1)) (0, 0)

-- 6

lastElem :: [a] -> a
lastElem = foldl1 (flip const)
