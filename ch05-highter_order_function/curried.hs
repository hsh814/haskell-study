--All function get only one parameter
--But we have used multiple parameter
--This is curried function
{-
    Prelude> :t max
    max :: Ord a => a -> a -> a
It's equal to 
    max :: Ord a => a -> (a -> a)
left side of -> is parameter type, 
and right side is return type.
-}

multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x * y * z
--call multThree == multTwoWithOne 2 3


--section
