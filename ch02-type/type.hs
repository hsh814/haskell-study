addThree x y z = x + y + z
factorial :: Integer -> Integer
factorial n = product [1..n]
--circumference :: Float -> Float
circumference :: Double -> Double
circumference r = 2 * pi * r

--type name starts with upper case
{-
Use :t to figure out type
    Prelude> :t 'a'
    'a' :: Char
    Prelude> :t 0 == 1
    0 == 1 :: Bool
    Prelude> :t [1, 2, 3]
    [1, 2, 3] :: Num t => [t]
    Prelude> :t 1
    1 :: Num t => t
    Prelude> :t 1.1
    1.1 :: Fractional t => t
Also applied to function
    *Main> addThree 1 2 3
    6
    *Main> :t addThree
    addThree :: Num a => a -> a -> a -> a
type Int is 64bit, Integer is infinite
    *Main> factorial 30
    265252859812191058636308480000000
    *Main> :t factorial
    factorial :: Integer -> Integer
Float is 32 bit
    *Main> circumference 4
    25.132742
use Double: 64bit 
    *Main> circumference 4
    25.132741228718345
head returns first element of list: a stands for generic
    *Main> :t head
    head :: [a] -> a 
class constraint: Eq a => a -> a means a's are equal type
    *Main> :t (==)
    (==) :: Eq a => a -> a -> Bool
class constraint: Ord means it has order
    *Main> :t (>)
    (>) :: Ord a => a -> a -> Bool
class constraint: Show is like ToString() in c#
    *Main> :t show
    show :: Show a => a -> String
Read type reads string to type
    Main> :t read
    read :: Read a => String -> a
Enum: enumerable, Bound: maxBound, minBound, Num: can be Int, Integer, Float, Double
Integral: Int, Integer, Floating: Float, Double
-}