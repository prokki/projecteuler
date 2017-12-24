{-
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}
module Main (main) where

import Data.List
import Math

palindromicTripel :: [(Int, Int, Int)]
palindromicTripel = filter (\(_, _, a) -> if Math.isPalindromic a then True else False) $ [ (x, y, x*y) | x <- [1..999], y <- [1..999] ]

main = print $ head $ sortBy (\(_,_,c1) -> \(_,_,c2) -> if c1 > c2 then LT else GT) palindromicTripel