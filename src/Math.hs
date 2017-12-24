module Math where

import String

divideByOr :: [Int] -> Int -> Bool
divideByOr []     y = False
divideByOr (x:xs) y
  | (mod y x == 0) = True
  | otherwise      = divideByOr xs y


divideByOrList :: [Int] -> [Int] -> [Int]
divideByOrList divisors list = filter (divideByOr divisors) list

fib :: [Int]
fib = fib' 1 2 where
  fib' x y = x : (fib' y (x+y))

primes :: Int -> [Int]
primes x = primes' x 2 where
  primes' :: Int -> Int -> [Int]
  primes' x y
    | x == y       = y:[]
    | mod x y == 0 = y:(primes' (div x y) y)
    | otherwise    = primes' x (y + 1)

isPalindromic :: Int -> Bool
isPalindromic x = String.isPalindromic (show x)