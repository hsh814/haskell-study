lucky :: Int -> String
lucky 7 = "Lucky!\n"
lucky x = "Unlucky...\n"
--instead of using huge if-else statement, 
--just define function like this
{-
    *Main> lucky 7
    "Lucky!\n"
    *Main> lucky 1
    "Unlucky...\n"
-}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
--recursion

charName :: Char -> String
charName 'a' = "Alex"
charName 'b' = "Belle"
charName 'c' = "Cecil"
--It may cause error for other inputs
--To avoid this, you should put default statement: _ is wild card
charName _ = "No name"
{-
    *Main> charName 'a'
    "Alex"
    *Main> charName 't'
    "*** Exception: pm.hs:(19,1)-(21,22): Non-exhaustive patterns in function charName
-}

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
--Use tuple as parameter
{-
    *Main> addVectors (0,1) (2,3)
    (2.0,4.0)
-}

head' :: [a] -> a
head' [] = error "Empty List!"
head' (x:_) = x
--Use list as parameter
{-
    *Main> head' [1, 2, 3, 4, 5]
    1
    *Main> head' "hello"
    'h'
-}

tell :: (Show a) => [a] -> String
tell [] = "Empty list!"
tell (x:[]) = "List with 1 element: " ++ show x
tell (x:y:[]) = "List with 2 element: " ++ show x ++ ", " ++ show y
tell (x:y:_) = "List with 3 or more element"
--x:xs -> x is first element, xs is tail list
{-
    *Main> let nums = [1..10]
    *Main> tell nums
    "List with 3 or more element"
    *Main> tell [1]
    "List with 1 element: 1"
    *Main> tell [1,2]
    "List with 2 element: 1, 2"
    *Main> tell []
    "Empty list!"
-}

--as pattern: use @
firstLetter :: String -> String
firstLetter "" = "Empty String!"
firstLetter str@(x:xs) = "The first letter of " ++ str ++ " is " ++ [x]
{-
    *Main> firstLetter "Hello world"
    "The first letter of Hello world is H"
-}
