module Chapter15 (
  doubleFact
  , fibonacci
  , fibonacciTail
) where

doubleFact :: Integer -> Integer
doubleFact n | n <= 1    = 1
             | otherwise = n * doubleFact(n - 2)

fibonacci :: Integer -> Integer
fibonacci n | n > 1     = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0     = fibonacci (n + 2) - fibonacci (n + 1)
            | otherwise = n

fibonacciTail :: Integer -> Integer
fibonacciTail n | n > 1 = fib' 0 1 n
                | n < 0 = fib'' 0 1 n
                | otherwise = n

fib' a b n | n <= 1 = b
           | otherwise = fib' b (a + b) (n - 1)

fib'' a b n | n >= -1 = b
            | otherwise = fib'' b (a - b) (n + 1)