--Important!
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list!"
maximum' [x] = x
--base case
maximum' (x:xs) = max x (maximum' xs)
--recursive

replicate' :: Int -> Int -> [Int]
replicate' 0 v = []
replicate' 1 v = [v]
replicate' n v = v:replicate' (n-1) v
{-
    *Main> replicate' 3 5
    [5,5,5]
-}

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs
{-
    *Main> nums = [1..100]
    *Main> take' 10 nums
    [1,2,3,4,5,6,7,8,9,10]
-}

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
{-
    *Main> reverse [1..10]
    [10,9,8,7,6,5,4,3,2,1]
-}

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
{-
    *Main> zip' [1..10] [0.0,0.1..2.0]
    [(1,0.0),(2,0.1),(3,0.2),(4,0.30000000000000004),(5,0.4000000000000001),(6,0.5000000000000001),(7,0.6000000000000001),(8,0.7000000000000001),(9,0.8),(10,0.9)]
-}

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs
{-
    *Main> elem' 4 [1..100]
    True
    *Main> elem' 100 [1..99]
    False
-}

--Sorting
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let
    left = [a | a <- xs, a <= x]
    right = [a | a <- xs, a > x]
    in quicksort left ++ [x] ++ quicksort right
{-
    *Main>  let randlist1 = [1..10] ++ [30,29..21] ++ [11..15] ++ [19,18..16]
    *Main> randlist1
    [1,2,3,4,5,6,7,8,9,10,30,29,28,27,26,25,24,23,22,21,11,12,13,14,15,19,18,17,16]
    *Main> quicksort randlist1
    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,21,22,23,24,25,26,27,28,29,30]
-}