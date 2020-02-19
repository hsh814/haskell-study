import qualified Data.Foldable as F


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
    
instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) =
        (F.foldMap f l) `mappend` (f x) `mappend` (F.foldMap f r)


--changeNode :: Tree a -> Tree a
--changeNode (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)


data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x


type Breadcrumbs = [Direction]

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)


data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

goL :: (Tree a, [Crumb a]) -> (Tree a, [Crumb a])
goL (Node x l r, bs) = (l, LeftCrumb x r:bs)

goR :: (Tree a, [Crumb a]) -> (Tree a, [Crumb a])
goR (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, [Crumb a]) -> (Tree a, [Crumb a])
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)


type Zipper a = (Tree a, [Crumb a])

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (EmptyTree, bs) = (EmptyTree, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

gotoRoot :: Zipper a -> Zipper a
gotoRoot (t, []) = (t, [])
gotoRoot z = gotoRoot (goUp z)

testTree :: Tree Int
testTree =
    Node 5
    (
        Node 3
            (Node 1 EmptyTree EmptyTree)
            (Node 6 EmptyTree EmptyTree)
    )
    (
        Node 9
            (Node 8 EmptyTree EmptyTree)
            (Node 10 EmptyTree EmptyTree)
    )

freeTree :: Tree Char
freeTree =
    Node 'P'
    (
        Node 'O'
        (
            Node 'L'
                (Node 'N' EmptyTree EmptyTree)
                (Node 'T' EmptyTree EmptyTree)
        )
        (
            Node 'Y'
                (Node 'S' EmptyTree EmptyTree)
                (Node 'A' EmptyTree EmptyTree)
        )
    )
    (
        Node 'L'
        (
            Node 'W'
                (Node 'C' EmptyTree EmptyTree)
                (Node 'R' EmptyTree EmptyTree)
        )
        (
            Node 'A'
                (Node 'A' EmptyTree EmptyTree)
                (Node 'C' EmptyTree EmptyTree)
        )
    )




goLeft' :: Zipper a -> Maybe (Zipper a)
goLeft' (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft' (EmptyTree, _) = Nothing

goRight' :: Zipper a -> Maybe (Zipper a)
goRight' (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight' (EmptyTree, _) = Nothing

goUp' :: Zipper a -> Maybe (Zipper a)
goUp' (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp' (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp' (_, []) = Nothing