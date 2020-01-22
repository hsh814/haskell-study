
module InductiveType (
    Tree (EmptyTree, Node),
    singleton,
    treeInsert,
    treeElem,
    numsTree
) where
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

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree
--Tree with only one node

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) 
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

--insert node into tree: at leaf node

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

numsTree :: Tree Int
numsTree = foldr treeInsert EmptyTree [8,6,4,1,7,3,5]
{-
    Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) 
    (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))
    *Main> treeElem 8 numsTree
    True
    *Main> treeElem 12 numsTree
    False
-}





