subList :: [a] -> [[a]]
subList (x)
  | length x == 1 = [x] 
  | otherwise = [x] ++ (subList (init x))

allSubLists :: [a] -> [[a]]
allSubLists (x) 
  | length x == 1 = [x] 
  | otherwise = subList (x) ++ allSubLists (tail x)

sumList :: (Ord a, Num a) => [a] -> a
sumList [] = 0
sumList (x:xs) = (+) x (sumList xs)

quickSort :: (Num a, Ord a) => [[a]] -> [[a]]
quickSort [] = []
quickSort (x:xs) =
  quickSort [s | s <- xs, let xc = sumList x, (sumList s) <= xc]
  ++ [x] ++
  quickSort [u | u <- xs, let xc = sumList x, (sumList u) > xc]

createWholeKSet :: [Int] -> [[Int]]
createWholeKSet =  quickSort.allSubLists

createSmallestKSet :: [Int] -> Int -> [[Int]]
createSmallestKSet xs k =
  let wholeList = createWholeKSet xs
  in take k wholeList

formatHeading :: String
formatHeading = "size | \t list \n  --------------------- \n"

formatOutput :: [[Int]] -> String
formatOutput [[]] = ""
formatOutput (x:xs) = show( sumList x ) ++ "\t" ++ show (x) ++ "\n" ++ formatOutput xs


list :: [Int]
k :: Int

-- CASE 1
list = [x*(-1)^x | x <- [1..100]]
k = 15

-- CASE 2
-- list = [24,-11,-34,42,-24,7,-19,21]
-- k = 6

-- CASE 3
-- list = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]
-- k = 8




main 
  | length list <= 0 = error "list is empty"
  | otherwise = putStr( formatHeading ++ formatOutput (createSmallestKSet list k))