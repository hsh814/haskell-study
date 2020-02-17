# ch14-more-monad

Let's learn about other monads (in mtl package)

```
ghc-pkg list | grep mtl
    mtl-2.2.2
```

## Contents
- [Writer](#Writer)
- [LogInProgram](#LogInProgram)
- [EfficientList](#EfficientList)

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

It's in `Control.Monad.Writer`

```
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
```

`>>=` is like `applyLog`: except tuple is wrapped with `Writer`, so you should unwrap this. 

```
*Main Control.Monad.Writer> runWriter (return 3 :: Writer String Int)
(3,"")
*Main Control.Monad.Writer> runWriter (return 3 :: Writer (Sum Int) Int)
(3,Sum {getSum = 0})
*Main Control.Monad.Writer> runWriter (return 3 :: Writer (Product Int) Int)
(3,Product {getProduct = 1})
```

### do

We can use do notation.

```
import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a * b)
```

```
*Main Control.Monad.Writer> runWriter multWithLog
(15,["Got number: 3","Got number: 5"])
```

You can add another line

```
multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a * b)
```

```
*Main Control.Monad.Writer> runWriter multWithLog
(15,["Got number: 3","Got number: 5","Gonna multiply these two"])
```

## [LogInProgram](./app/LogInProgram.hs)

### gcd

```
gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0 = a
    | otherwise = gcd' b (a `mod` b)
```

```
*Main Control.Monad.Writer> gcd' 48 15
3
```

### context

Let's add some context.

```
gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " % " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd'' b (a `mod` b)
```

```
*Main Control.Monad.Writer> runWriter (gcd'' 48 15)
(3,["48 % 15 = 3","15 % 3 = 0","Finished with 3"])

*Main Control.Monad.Writer> mapM_ putStrLn $ snd $ runWriter (gcd'' 48 15)
48 % 15 = 3
15 % 3 = 0
Finished with 3
```

## [EfficientList](./app/EfficientList.hs)

1

1

1


1


1

1

1

1