module Intro where

import Data.Char (digitToInt, isDigit)

-- 2

lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

sign x = if x > 0 then 1 else if x < 0 then -1 else 0

-- 3

infixl 6 |-|
x |-| y = abs (x - y)

-- 4

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y
    | isDigit x && isDigit y = a * 10 + b
    | otherwise              = 100
    where a = digitToInt x
          b = digitToInt y

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2

-- 5

doubleFact :: Integer -> Integer
doubleFact n
    | n <= 2    = n
    | otherwise = n * doubleFact(n - 2)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci (-2) = -1
fibonacci n | abs(n) == 1 = 1
            | n > 1       = fibonacci (n - 1) + fibonacci (n - 2)
            | otherwise   = fibonacci (n + 2) - fibonacci (n + 1)

fib :: Integer -> Integer
fib n | n > 1 = fib' 0 1 n
            | n < 0 = fib'' 0 1 n
            | otherwise = n

fib' a b n | n <= 1 = b
           | otherwise = fib' b (a + b) (n - 1)

fib'' a b n | n >= -1 = b
            | otherwise = fib'' b (a - b) (n + 1)

-- 6

seqA :: Integer -> Integer
seqA n
    | n == 0 = 1
    | n == 1 = 2
    | n == 2 = 3
    | otherwise = let
        recc a1 a2 a3 0 = a1
        recc a1 a2 a3 n = recc a2 a3 (a3 + a2 - 2 * a1) (n - 1)
        in recc 1 2 3 n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = let
    helper sum count n
            | n < 10    = (sum + n, count + 1)
            | otherwise = helper (sum + n `mod` 10) (count + 1) (n `div` 10)
    in helper 0 0 (abs x)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let
    chunks = 1000
    step = (b - a) / chunks
    sum acc x step f 0 = acc
    sum acc x step f n = sum (acc + f x) (x + step) step f (n - 1) in
    ((f a + f b) / 2 + sum 0 (a + step) step f (chunks - 1)) * step
