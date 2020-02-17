# ch14-more-monad

Let's learn about other monads (in mtl package)

```
ghc-pkg list | grep mtl
    mtl-2.2.2
```

## Contents
- [Writer](#Writer)

## [Writer](./app/Writer.hs)

### `applyLog`
```
isBigGang :: Int -> Bool
isBigGang x = x > 9

isBigGang' :: Int -> (Bool, String)
isBigGang' x = (x > 9, "compare to 9")
```

```
*Main> isBigGang' 3
(False,"compare to 9")
*Main> isBigGang' 30
(True,"compare to 9")
```

What if you want to put (3, "Small") into `isBigGang'`?

Let's use `applyLog` like `applyMaybe` we used before.

```
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)
```

```
*Main> applyLog (3, "small ") isBigGang'
(False,"small compare to 9")
*Main> applyLog (30, "big ") isBigGang'
(True,"big compare to 9")
```

```
*Main> let howLong x = (length x, "Applied length.")
*Main> applyLog ("Tobin", "Got outlaw name ") howLong
(5,"Got outlaw name Applied length.")
*Main> applyLog ("Bathcat", "Got outlaw name ") howLong
(7,"Got outlaw name Applied length.")
```

### Monoid

Is log only string? No. It can be any list.

```
applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
```

Will it work for bytestring? We can do it through `mappend`.

```
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, mappend log newLog)
```

Let's make some example

```
type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)
```

```
*Main> applyLog ("beans", Sum 10) addDrink
("milk",Sum {getSum = 35})
*Main> applyLog ("jerky", Sum 35) addDrink
("whiskey",Sum {getSum = 134})
*Main> applyLog ("anything", Sum 134) addDrink
("beer",Sum {getSum = 164})

*Main> ("beans", Sum 10) `applyLog` addDrink `applyLog` addDrink
("beer",Sum {getSum = 65})
```

### Writer Type

```
newtype Writer w a = Writer { runWriter :: (a, w) }
```



1

1

1


1


1

1

1

1