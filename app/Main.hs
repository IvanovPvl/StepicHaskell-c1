module Main where

import Data.Functor

main :: IO ()
main = putStrLn "main"

data Log a = Log [String] a deriving (Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f s = \x -> (Log [s] (f x))

-- execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
-- execLoggers x f1 f2
