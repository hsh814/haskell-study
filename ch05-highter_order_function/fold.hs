--Many recursive functions have same pattern:
--base case: []
--function (x:xs) = something and function xs
--To encapsulate these, we can use fold

sum'' :: (Num a) => [a] -> a
sum'' [] = 0
sum'' (x:xs) = x + sum xs

--fold binary function -> accumulator(initial value) -> list
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
--foldl: left fold: start from first

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs
--foldl is tail recursion

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
--foldr: right fold: start from end, can have infinite list
--It is frequently used for making new list

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr f acc xs)

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max
--foldl1, foldr1 does not need explicit accumulator:
--first(or last) element became accumulator

--Other examples
reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x : acc) [] xs

product' :: (Num a) => [a] -> a
product' ns = foldl (*) 1 ns

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x : acc else acc) []

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs
--For infinite list, if there is False, 
--then and' will stop and return false without further calculation


sqrt' :: Int -> Float
sqrt' n = let
    m = fromIntegral n
    in sqrt m

--scanr, scanl: similar to fold...
--sqrtSums :: Int -> Int
--sqrtSums n = length (takeWhile (< n) (scanl1 (+) (map sqrt [1..]))) + 1