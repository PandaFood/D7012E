data IndexList = IndexList [Int] Int Int deriving (Show)

subList :: [Int] -> Int -> Int -> [IndexList]
subList x i j
  | length x == 1 = [IndexList x i j]
  | otherwise = [IndexList x i j] ++ (subList (init x) i ((-) j 1))

allSubLists :: [Int] -> Int -> Int -> [IndexList]
allSubLists x i j
  | length x == 1 = [IndexList x i j]
  | otherwise = subList x i j ++ allSubLists (tail x) ((+) i 1) j

sumList :: IndexList -> Int
sumList (IndexList x _ _ ) = sum x

quickSort :: [IndexList] -> [IndexList]
quickSort [] = []
quickSort (x:xs) =
  quickSort [s | s <- xs, let xc = sumList x, (sumList s) <= xc]
  ++ [x] ++
  quickSort [u | u <- xs, let xc = sumList x, (sumList u) > xc]

createWholeKSet :: [Int] -> [IndexList]
createWholeKSet x = quickSort lst
  where lst = allSubLists x 1 (length x)

createSmallestKSet :: [Int] -> Int -> [IndexList]
createSmallestKSet xs k =
  let wholeList = createWholeKSet xs
  in take k wholeList

getIndexI :: IndexList -> Int
getIndexI (IndexList _ i _ ) = i

getIndexJ :: IndexList -> Int
getIndexJ (IndexList _ _ j ) = j

getIndexList :: IndexList -> [Int]
getIndexList (IndexList l _ _ ) = l

formatHeading :: String
formatHeading = "size | \t i \t j \t sublist \n  ------------------------------------- \n"

formatOutput :: [IndexList] -> String
formatOutput [] = ""
formatOutput (x:xs) = show( sumList x ) ++ "\t" ++  show(getIndexI x) ++ "\t" ++ show(getIndexJ x) ++ "\t" ++ show (getIndexList x) ++ "\n" ++ formatOutput xs


list :: [Int]
k :: Int

-- CASE 1
-- list = [x*(-1)^x | x <- [1..100]]
-- k = 15

-- CASE 2
-- list = [24,-11,-34,42,-24,7,-19,21]
-- k = 6

-- CASE 3
list = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]
k = 8



main 
  | length list <= 0 = error "list is empty"
  | otherwise = putStr( formatHeading ++ formatOutput (createSmallestKSet list k))