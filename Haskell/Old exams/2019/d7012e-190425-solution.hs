-- Suggested solution 
-- Declarative languages D7012E 2019-04-25
-- /Håkan Jonsson

-- 190425: 1

-- helpers used in goldbach - not required in a solution
primes a b = filter (\x -> prime x) [a..b]  
prime x = length (filter (\b -> x `mod` b == 0) [2..x-1] ) == 0

goldbach :: Int -> Bool
goldbach n = [ 1 | a <- p, b <- p, a+b==n ] /= []
  where
  p = primes 2 (n-2)

{- Example: 
*Main> take 40 $ map goldbach [4,6..]
[True,True,True,True,True,True,True,True,True,True,True,True,True,
True,True,True,True,True,True,True,True,True,True,True,True,True,
True,True,True,True,True,True,True,True,True,True,True,True,True,True]
*Main> 
-}



-- 190425: 2

periodise :: [a] -> [a]
periodise xs = xs ++ reverse xs ++ periodise xs

{- Example:
*Main> take 30 $ periodise [0,1,2,3]
[0,1,2,3,3,2,1,0,0,1,2,3,3,2,1,0,0,1,2,3,3,2,1,0,0,1,2,3,3,2]
*Main> 
-}

-- Other versions

periodise2 :: [a] -> [a]
periodise2 xs = period xs ++ periodise2 xs
  where
    period :: [a] -> [a]
    period [x] = x:[x]
    period (x:xs) = (x : period xs) ++ [x]

periodise3 :: [a] -> [a]
periodise3 xs = backandforth xs xs
  where
    backandforth :: [a] -> [a] -> [a]
    backandforth [] ls = backandforth (reverse ls) (reverse ls)
    backandforth (x:xs) ls = x : backandforth xs ls

periodise4 :: [a] -> [a]
periodise4 xs = xs ++ periodise4 (reverse xs)

periodise5 :: [a] -> [a]
periodise5 xs = xs ++ reverse (periodise4 xs)

-- 190425: 3a

data BranchingTree a = Leaf a | Node [BranchingTree a] deriving (Eq,Show)



-- 190425: 3b

mapBT :: (t -> a) -> BranchingTree t -> BranchingTree a
mapBT f (Leaf x) = Leaf (f x)
mapBT f (Node xs) = Node (map (mapBT f) xs)


bt = Node [Leaf 1, Node [Leaf 2, Node [ Node [Leaf 3], Leaf 4],Leaf 5],Node[Leaf 6]]

{-
*Main> bt
Node [Leaf 1,Node [Leaf 2,Node [Node [Leaf 3],Leaf 4],Leaf 5],Node [Leaf 6]]
*Main> mapBT (+3) bt
Node [Leaf 4,Node [Leaf 5,Node [Node [Leaf 6],Leaf 7],Leaf 8],Node [Leaf 9]]
*Main>
-}



-- 190425: 4a

-- (concatMap1 becasue Prelude contains a function concatMap already)

concatMap1 :: ([a] -> [b]) -> [[a]] -> [b]
concatMap1 f = foldr (\x ra -> f x ++ ra  ) [] 

-- Other versions

concatMap2 :: ([a] -> [b]) -> [[a]] -> [b]
concatMap2 f = foldr ((++).f) []


{- Example:
*Main> concatMap (\x -> map (+1) x) [[1,2,3],[4,5]]
[2,3,4,5,6]
*Main> 
-}



-- 190425: 4b

const :: t1 -> t2 -> t1
const x y = x

subst :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
subst f g x = f x (g x)

fix :: ((t1 -> t2) -> t1 -> t2) -> t1 -> t2
fix f x = f (fix f) x