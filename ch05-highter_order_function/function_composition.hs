--Function composition: (f . g)(x) = f(g(x))
(.') :: (b -> c) -> (a -> b) -> a -> c
f .' g = \x -> f (g x)
{-
    Prelude> map (\x -> negate (abs x)) [4, -2, 5, -3, 7, 8, -9, -10]
    [-4,-2,-5,-3,-7,-8,-9,-10]
If you using (.):
    Prelude> map (negate . abs) [4, -2, 5, -3]
    [-4,-2,-5,-3]
-}
--function composition is right-associative
--for multiple parameters:
{-
    Prelude> sum (replicate 5 (max 6.7 8.9))
    44.5
    Prelude> sum . replicate 5 $ max 6.7 8.9
    44.5
Another example
    Prelude> replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
    [180,180]
    Prelude> replicate 2 . product $ map (*3) $ zipWith max [1,2] [4,5]
    [180,180]
-}

--point-free style
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

--due to currying, you can omit xs
fn x = ceiling (negate (tan (cos (max 50 x))))
fn' = ceiling . negate . tan . cos . max 50



