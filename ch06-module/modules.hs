--You can use ouside module by import
import Data.List
--import Data.List (nub, sort) make load only nub and sort function
--import Data.List hiding (nub) == load Data.List exept nub
import qualified Data.Map as DM
--Can access function like Data.Map.filter == DM.filter
--Use module without space => otherwise, it would be treated as composition
import qualified Data.Char as DC


numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
--nub removes abundunt elements.
{-

-}

{-
In ghci:
    Prelude> :m + Data.List
    Prelude Data.List> words "hey these are the words in this sentence"
    ["hey","these","are","the","words","in","this","sentence"]
group groups same elements
    Prelude Data.List> group [1,2,2,2,3,4,4,5,5,2,2,1,1,3]
    [[1],[2,2,2],[3],[4,4],[5,5],[2,2],[1,1],[3]]
To get every same element in group, you should first sort list
    Prelude Data.List> sort ["bip", "boop", "bob", "bip", "bob", "boob", "boop", "boob"]
    ["bip","bip","bob","bob","boob","boob","boop","boop"]
    Prelude Data.List> group . sort ["bip", "boop", "bob", "bip", "bob", "boob", "boop", "boob"]
-}

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words
{-
    *Main Data.List> wordNums "bip bob boop bip bip boop bob bob"
    [("bip",3),("bob",3),("boop",2)]
-}

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = any (isPrefixOf needle) (tails haystack)
{-
    *Main Data.List> tails "party"
    ["party","arty","rty","ty","y",""]
    *Main Data.List> isIn "art" "party"
    True
    *Main Data.List> isIn "lie" "lion king"
    False
-}


{-
To implement Caesar cipher, we will use ord and chr in Data.Char
    Prelude Data.Char> ord 'a'
    97
    Prelude Data.Char> chr 100
    'd'
-}
encodeCaesar :: Int -> String -> String
encodeCaesar offset message = map (\c -> DC.chr $ DC.ord c + offset) message
{-
    *Main Data.Char> encodeCaesar 3 "hey there"
    "kh|#wkhuh"
-}
decodeCaesar :: Int -> String -> String
decodeCaesar = (\offset message -> encodeCaesar (-offset) message)
{-
    *Main Data.Char> decodeCaesar 3 "kh|#wkhuh"
    "hey there"
-}

{-
foldl: may cause stack overflow
    *Main Data.Char> foldl (+) 0 [1..100]
    5050
    *Main Data.Char> foldl (+) 0 [1..1000000]
    500000500000
    *Main Data.Char> foldl (+) 0 [1..100000000]
    *** Exception: stack overflow
    *Main Data.Char> 죽었음
-}
{-
This is because of lazy evaluation.
Then, can we use strict version of foldl?
    Prelude Data.List> foldl' (+) 0 [1..100000000]
    5000000050000000
-}
