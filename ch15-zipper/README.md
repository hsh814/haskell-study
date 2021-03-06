# ch15-zipper

## Contents
- [Tree](#Tree)
- [List](#List)
- [FileSystem](#FileSystem)

## [Tree](./app/Tree.hs)

Due to reference transparency, haskell can't change value.
When we use tree, if we change value, it's just returning new tree with slightly changed values.
Then, how can we use tree?

### search

Make a tree.

```
*Main> freeTree
Node 'P' (Node 'O' (Node 'L' (Node 'N' EmptyTree EmptyTree) (Node 'T' EmptyTree EmptyTree)) (Node 'Y' (Node 'S' EmptyTree EmptyTree) (Node 'A' EmptyTree EmptyTree))) (Node 'L' (Node 'W' (Node 'C' EmptyTree EmptyTree) (Node 'R' EmptyTree EmptyTree)) (Node 'A' (Node 'A' EmptyTree EmptyTree) (Node 'C' EmptyTree EmptyTree)))
```

Let's change node from 'P' to 'T'.

```
changeNode :: Tree a -> Tree a
changeNode (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)
```

It's too complicated... What's the better way?

```
data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (ChangeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (ChangeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r
```

Function accepts list of direction with tree.

```
elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x
```

Let's make function to access certain element by direction.

```
*Main> elemAt [R,L] freeTree
'W'
*Main> let newTree = changeToP [R,L] freeTree
*Main> elemAt [R,L] newTree
'P'
```

### trail

What if we left breadcrums on the trail of searching tree? It would be more easy to access.

```
type Breadcrumbs = [Direction]

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)
```

```
*Main> goLeft $ goRight (freeTree, [])
(Node 'W' (Node 'C' EmptyTree EmptyTree) (Node 'R' EmptyTree EmptyTree),[L,R])
```

### go back

How can we get back to parent node? The only thing we know is path.

Then, simple answer is just add information of nodes in path.

```
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

goL :: (Tree a, [Crumb a]) -> (Tree a, [Crumb a])
goL (Node x l r, bs) = (l, LeftCrumb x r:bs)

goR :: (Tree a, [Crumb a]) -> (Tree a, [Crumb a])
goR (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, [Crumb a]) -> (Tree a, [Crumb a])
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)
```

We can actually go up.

### Zipper type

```
type Zipper a = (Tree a, [Crumb a])

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (EmptyTree, bs) = (EmptyTree, bs)
```

```
*Main> let newFocus = modify (\_ -> 'P') (goR (goL (freeTree, [])))
*Main> let newFocus2 = modify (\_ -> 'X') (goUp newFocus)
```

Zipper type zips data structure and its sub structure.

Let's add a new leaf node.

```
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)
```

```
*Main> let leftest = goL $ goL $ goL $ goL (freeTree, [])
*Main> let newFocus3 = attach (Node 'Z' EmptyTree EmptyTree) leftest
```

We can go to root directly.

```
gotoRoot :: Zipper a -> Zipper a
gotoRoot (t, []) = (t, [])
gotoRoot z = gotoRoot (goUp z)
```

## [List](./app/List.hs)

Zipper can be applied to almost any data structure: include list.

### List zipper

```
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
```

List can be show as mono tree

```
type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)
```

```
*Main> goForward (xs, [])
([2,3,4],[1])
*Main> goForward ([2,3,4], [1])
([3,4],[2,1])
*Main> goBack ([3,4], [2,1])
([2,3,4],[1])
```

You can apply zipper to list like this. You can go forward and backward.


## [FileSystem](./app/FileSystem.hs)

### File

File system is using tree: rb tree, b tree...
File is node and directory is path.

Let's make really simple file system.

```
type Name = String
type Data = String
data FS = File Name Data | Dir Name [FS] deriving (Show)
```

### FS zipper

`cd ..` is easy.

```
fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Dir name (ls ++ [item] ++ rs), bs)
```

You can move into directory.

```
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Dir dirName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb dirName ls rs:bs)

nameIs :: Name -> FS -> Bool
nameIs name (Dir dirName _) = name == dirName
nameIs name (File fileName _) = name == fileName
```

```
*Main> let newFocus = fsTo "anna" $ fsTo "name" (myDisk, [])
*Main> fst newFocus
File "anna" "Annna"

*Main> let newFocus2 = fsTo "elsa" $ fsUp newFocus
*Main> fst newFocus2
File "elsa" "Elssa"
```

### Manipulate file system

```
fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Dir name items, bs) = (Dir newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)
```

Rename file or directory: it does not delete original version.

```
*Main> let newFocus = fsUp $ fsRename "app" $ fsTo "program" (myDisk, [])
*Main> fst newFocus
Dir "root" [File "goat.txt" "baaaaaaa",File "pony.txt" "neigh",Dir "name" [File "anna" "Annna",File "elsa" "Elssa"],Dir "app" [File "bash" "hsab",File "ls" "sl"]]
```

### maybe?

Maybe file or directory does not exist.

Maybe you try to go up from root.

File system can fail. So, it should be Maybe instance. 

```
goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (EmptyTree, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (EmptyTree, _) = Nothing

goUp' :: Zipper a -> Maybe (Zipper a)
goUp' (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp' (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp' (_, []) = Nothing
```

This is improved version.

```
*Main> goLeft' (EmptyTree, [])
Nothing
*Main> goRight' (Node 'A' EmptyTree EmptyTree, [])
Just (EmptyTree,[RightCrumb 'A' EmptyTree])
```

You can use `>>=` since it's monad.

```
*Main> let newFocus = return (freeTree, []) >>= goRight' >>= goLeft'
*Main> newFocus
Just (Node 'W' (Node 'C' EmptyTree EmptyTree) (Node 'R' EmptyTree EmptyTree),[LeftCrumb 'L' (Node 'A' (Node 'A' EmptyTree EmptyTree) (Node 'C' EmptyTree EmptyTree)),RightCrumb 'P' (Node 'O' (Node 'L' (Node 'N' EmptyTree EmptyTree) (Node 'T' EmptyTree EmptyTree)) (Node 'Y' (Node 'S' EmptyTree EmptyTree) (Node 'A' EmptyTree EmptyTree)))])

*Main> newFocus >>= goRight' >>= goRight' >>= goRight'
Nothing
```

