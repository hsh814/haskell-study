import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare x y =
    let
        a = compare (length x) (length y)
        b = compare x y
    in if a == EQ then b else a
    
lengthCompare' :: String -> String -> Ordering
lengthCompare' x y =
    mappend (compare (length x) (length y)) (compare x y)

vowelCompare :: String -> String -> Ordering
vowelCompare x y =
    mappend (compare (length x) (length y))
    (mappend (compare (vowels x) (vowels y)) (compare x y))
    where vowels = length . filter (`elem` "aeiou")