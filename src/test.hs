-- head [1,2,3,4,5,6] = 1
-- tail [1,2,3,4,5,6] = [2,3,4,5,6]
-- 1:2:3:4:5:6:[]     = [1,2,3,4,5,6]
-- [1,2,3] ++ [4,5,6] = [1,2,3,4,5,6]

-- reverseList [1,2,3,4,5,6] = [6,5,4,3,2,1]
reverseList :: [Int] -> [Int]
reverseList []     = []
reverseList (x:xs) = (reverseList xs) ++ (x:[])


-- mul3 [1,2,3,4,5,6] = [3,6,9,12,15,18]
mul3 []     = []
mul3 (x:xs) = (x*3:[]) ++ mul3 xs

-- filterGT3 [1,2,3,4,5,6] = [4,5,6]
filterGT3 []     = []
filterGT3 (x:xs)
 | (x > 3)   = (x:(filterGT3 xs)) 
 | otherwise = filterGT3 xs

-- empty [1,2,3] = False
empty [] = True
empty _  = False

-- mix3 [1,2,3,4,5,6] = [1,3,2,3,3,3,4,3,5,3,6,3]
mix3 []     = []
mix3 (x:xs) = (x:3: (mix3 xs))

--quad [1,2,3,4,5,6] = [1,4,9,16,25,36]
quad     [] = []
quad (x:xs) = (x*x: (quad xs))

-- mulList 6 [1,2,3,4,5,6] = [6,12,18,24,30,36]
mulist y [] = []
mulist y (x:xs) = (y*x): (mulist y xs)

-- filterGT 6 [100,5,8, 2, 70] = [100,8,70]
filterGT y [] = []
filterGT y (x:xs)
  |(x > y) = (x: filterGT y xs)
  | otherwise = filterGT y xs

--repeati 8 3 = [8,8,8]
repeati x 0 = []
repeati x y = x : repeati x (y-1)   