module Main (main) where

import Math

main = print $ sum $ filter (\x -> mod x 2 == 0) $ takeWhile (< 4000000) fib