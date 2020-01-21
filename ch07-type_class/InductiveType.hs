
--recursive
data List1 a = Empty | Cons a (List1 a) deriving (Show, Read, Eq, Ord)
--Cons is another expression of ':'
--Cons append list a single value
{-
    *Main> Empty
    Empty
    *Main> Cons 5 Empty
    Cons 5 Empty
    *Main> Cons 3 $ Cons 4 Empty
    Cons 3 (Cons 4 Empty)
-}
infixr 5 :-:
data List2 a = Empty2 | a :-: (List2 a) deriving (Show, Read, Eq, Ord)
--fixity declaration: 
--infixr -> infix, right-associative
--Always starts with colon
{-
    *Main> 3 :-: 4 :-: Empty2
    3 :-: (4 :-: Empty2)
-}

infixr 5 +++
(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)
{-
    *Main> 1:2:3:[] +++ 2:3:4:[]
    [1,2,3,2,3,4]
-}

--Binary Search Tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving  (Show)


