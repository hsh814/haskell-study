--You can use another function as parameter
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- -> is right-associative: from left to right
-- You need to use braces to certifiy left is first
{-
    *Main> applyTwice (+3) 10
    16
    *Main> applyTwice (++ " Hello!") "Anna"
    "Anna Hello! Hello!"
-}
myfunction :: Int -> Int -> Int
myfunction x y = x + y

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
--Input: function, list, list, list
{-
    *Main> zipWith' myfunction [1,3..11] [2,4..100]
    [3,7,11,15,19,23]
-}

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g where
    g x y = f y x
{-
    *Main> zip "hello" [1..10]
    [('h',1),('e',2),('l',3),('l',4),('o',5)]
    *Main> flip' zip [1..10] "hello"
    [('h',1),('e',2),('l',3),('l',4),('o',5)]
-}


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
--You can get same result by list comprehension
--[f x | x <- xs]
{-
    *Main> map (+3) [1..10]
    [4,5,6,7,8,9,10,11,12,13]
-}

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x: filter' f xs
    | otherwise = filter' f xs
{-
    *Main> filter (>3) [1..10]
    [4,5,6,7,8,9,10]
-}
{-
    *Main> takeWhile (/=' ') "take while"
    "take"
Sum of square of odd numbers less then 10000
    *Main> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
    166650
-}

--Collatz sequence
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
    | even n = n : collatz (div n 2)
    | otherwise = n : collatz (3 * n + 1)
{-
    *Main> collatz 10
    [10,5,16,8,4,2,1]
-}
--How many Collatz sequence longer then 15 in range of [1..100]?
numLongChains :: Integer -> Int
numLongChains n = length (filter isLong (map collatz [1..n]))
    where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter isLong (map collatz [1..100]))
    where isLong xs = length xs > 15
{-
    *Main> numLongChains 100
    66
-}
--You can map function with two parameters
{-
    Prelude> let listOfFuns = map (*) [0..]
    Prelude> (listOfFuns !! 5) 5
    25
-}