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