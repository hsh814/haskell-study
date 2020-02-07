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


