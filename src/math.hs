module Math where

divideByOr :: [Int] -> Int -> Bool
divideByOr []     y = False
divideByOr (x:xs) y =
  | (mod y x == 0) = True
  | otherwise      = divideByOr xs y


divideByOrList :: [Int] -> [Int] -> [Int]
divideByOrList divisors list = filter (divideByOr divisors) list

fib :: [Int]
fib = fib' 1 2 where
  fib' x y = x : (fib' y (x+y))