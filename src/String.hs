module String where

splitHalf :: [a] -> ([a], [a])
splitHalf []   = ([], [])
splitHalf list = splitAt (div (length list) 2) list

splitHalf' :: [a] -> ([a], [a])
splitHalf' []   = ([], [])
splitHalf' list
  | mod (length list) 2 == 0 =  (first, x:xs)
  | otherwise                = (first, xs) where
    (first, x:xs) = splitHalf list

isPalindromic :: Eq a => [a] -> Bool
isPalindromic list = first == reverse second where
  (first, second) = splitHalf' list
--  | mod (length list) 2 == 0 = 