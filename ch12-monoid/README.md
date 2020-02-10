# ch12-monoid

## Contents
- [NewType](#NewType)
- [Monoid](#Monoid)
- [MonoidInstances](#MonoidInstances)
- [FoldMonoid](#FoldMonoid)

## [NewType](./app/NewType.hs)

`data ZipList a = ZipList {getZipList :: [a]}`

- newtype

`newtype ZipList a = ZipList {getZipList :: [a]}`

What's the difference? 

1. `newtype` is faster then `data`: `data` has overhead when wrapping and unwrapping.
But `newtype` only make different name: no overhead.

2. `newtype` only have one constructor and only one field.

`newtype CharList = CharList {getCharList :: [Char] } deriving (Eq, Show)`

You can also use deriving
```
*Main> CharList "This will be shown"
CharList {getCharList = "This will be shown"}
*Main> CharList "anna" == CharList "anna"
True
*Main> CharList "anna" == CharList "elsa"
False
```

- newtype as instance of type class

```
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
```

Then, how about tuple?

```
newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
    fmap :: (a -> b) -> Pair c a -> Pair c b
    fmap f (Pair (x, y)) = Pair (f x, y)
```
You can use this like
```
*Main> getPair $ fmap (*100) (Pair (2,3))
(200,3)
*Main> getPair $ fmap reverse (Pair ("londong calling", 3))
("gnillac gnodnol",3)
```

- Laziness of newtype

```
data CoolBool = CoolBool {getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
```
Let's use undefined
```
*Main> helloMe (CoolBool True)
"hello"
*Main> helloMe undefined
"*** Exception: Prelude.undefined
```
haskell throws exception

Let's change `data` to `newtype`

`newtype CoolBool = CoolBool {getCoolBool :: Bool }`

```
*Main> helloMe undefined
"hello"
```
It works fine: why?

It's because newtype do not pattern matching 
since it can have only one function

- type, data, newtype

`type` is for type synonym: 

`type IntList = [Int]` -> `IntList` and `[Int]` are equal

But in `newtype`,

`newtype CharList = CharList { getCharList :: [Char] }`

You can't use `++` in `CharList`: the only way is using `getCharList`, 
use `++` for two `[Char]`, and rebuild it into `CharList`

## [Monoid](./app/Monoid.hs)

`*` : get two parameters, multiply them -> `* 1` : same

`++` : get two lists, add them -> `++ []` : same

there are some common thing between `1` and `[]`:
associativity

- `Monoid`

```
module Data.Monoid ( Monoid ) where
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
```

1. `mempty` : It's not a real function: no parameter

2. `mappend` : Binary function

3. `mconcat` : The most important function: it has default

- Rule of monoid

1. `mappend mempty x = x`

2. `mappend x mempty = x`

3. `mappend (mappend x y) z = mappend x (mappend y z)`

1, 2: x is identifier

3: associative


## [MonoidInstances](./app/MonoidInstances.hs)

- list
```
instance Monoid [a] where
    mempty = []
    mappend = (++)
```

```
*Main> mappend [1,2,3] [4,5,6]
 [1,2,3,4,5,6]
```

`mappend a b` need not be same as `mappend b a`

- (+), (*): Sum, Product

Obviously...

```
newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded

instance Num a => Monoid (Product a) where
    mempty = Product 1
    mappend (Product x) (Product y) = Product (x * y)
```
Sum is similar
```
*Main> getProduct $ mappend (Product 3) (Product 9)
27
*Main> getProduct $ mappend (Product 3) mempty
3
*Main> getProduct . mconcat . map Product $ [2,3,4]
24

*Main> getSum $ mappend (Sum 2) (Sum 1)
3
*Main> getSum $ mappend (Sum 5) mempty
5
*Main> getSum . mconcat . map Sum $ [1,2,3]
6
```

- Any, All :: Bool

`Any` is or
```
newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
    mempty = Any False
    mappend (Any  x) (Any y) = Any (x || y)
```

`All` is and

```
newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
    mempty = All True
    mappend (All x) (All y) = All (x && y)
```

- Ordering

```
instance Monoid Ordering where
    mempty = EQ
    mappend LT _ = LT
    mappend EQ y = y
    mappend GT _ = GT
```

Select left one if it's not EQ: 
similar to dictionary ordering

```
*Main> mappend LT GT
LT
*Main> mappend GT LT
GT
*Main> mappend EQ LT
LT
*Main> mappend mempty GT
GT
```

How can we use it?
```
lengthCompare :: String -> String -> Ordering
lengthCompare x y =
    let
        a = compare (length x) (length y)
        b = compare x y
    in if a == EQ then b else a
    
lengthCompare' :: String -> String -> Ordering
lengthCompare' x y =
    mappend (compare (length x) (length y)) (compare x y)
```
`lengthCompare'` version is using fact that Ordering is Monoid

```
*Main> lengthCompare "zen" "ants"
LT
*Main> lengthCompare' "zen" "ant"
GT
```

Let's add other priority
```
vowelCompare :: String -> String -> Ordering
vowelCompare x y =
    mappend (compare (length x) (length y)) 
    (mappend (compare (vowels x) (vowels y)) (compare x y))
    where vowels = length . filter (`elem` "aeiou")
```
First, it compares length
Second, it compares number of vowels
Last, it compares dictionary order
```
*Main> vowelCompare "zen" "anna"
LT
*Main> vowelCompare "zen" "ann"
GT
*Main> vowelCompare "zen" "ana"
LT
```

- `Maybe` Monoid

```
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    mappend Nothing m = m
    mappend m Nothing = m
    mappend (Just m1) (Just m2) = Just (mappend m1 m2) 
```

For Maybe a, a must be Monoid's Instance
```
*Main> mappend ( Just "anna") (Just "elsa")
Just "annaelsa"
*Main> Just (Sum 3) `mappend` Just (Sum 5)
Just (Sum {getSum = 8})
```

It's useful for fail-able monoid calculation.

But what if a is not Monoid?: Easy way is only take first value

```
newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
    mempty = First Nothing
    mappend (First (Just x)) _ = First (Just x)
    mappend (First Nothing) x = x
```
If first is Nothing, it take second value

```
*Main> getFirst $ First (Just 'a') `mappend` First (Just 'b')
Just 'a'
*Main> getFirst $ First Nothing `mappend` First (Just 'b')
Just 'b'
*Main> getFirst $ First (Just 'a') `mappend` First Nothing
Just 'a'
*Main> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10, Nothing, Just 13]
Just 9
```

## [FoldMonoid](./app/FoldMonoid.hs)

