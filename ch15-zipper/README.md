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





## [List](./app/List.hs)







## [FileSystem](./app/FileSystem.hs)


