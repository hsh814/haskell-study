# ch12-monoid

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

