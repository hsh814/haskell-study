--Lambda is used for temp function
{-
These two are same expression
    Prelude> map (+3) [1,3,5,6]
    [4,6,8,9]
    Prelude> map (\x->x+3) [1,3,5,6]
    [4,6,8,9]
Lambda also can accept multi parameter
    Prelude> zipWith (\a b -> a * 100 + b * 10) [5,4..1] [1..5]
    [510,420,330,240,150]
-}

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

flip' :: (a -> b ->  c) -> b -> a -> c
flip' f = \x y -> f y x
{-
    *Main> flip' (++) "world" "hello"
    "helloworld"
-}
--These are same
