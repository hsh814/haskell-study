import qualified Data.Foldable as F
import Data.Monoid

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
    
instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) =
        (F.foldMap f l) `mappend` (f x) `mappend` (F.foldMap f r)


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