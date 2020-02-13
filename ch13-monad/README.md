# ch13-monad

## Contents
- [Maybe](#Maybe)
- [Monad](#Monad)
- [DoNotation](#DoNotation)
- [ListMonad](#ListMonad)

## [Maybe](./app/Maybe)

### What is monad?

Functor -> Applicative Functor -> Monad!

Functor: wrap data type

`fmap :: (Functor f) => (a -> b) -> f a -> f b`

Applicative: unwrap wrapped data and process it and wrap it

`(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b`

Monad: extension of Applicative: with a, returns value with context

`(>>=) :: (Monad m) => m a -> (a -> m b) -> m b`

`>>=` is called bind: with context and normal value, it returns contexted value

### `Maybe`

`Maybe` is monad! 

```
Prelude> fmap (++"!") (Just "wisdom")
Just "wisdom!"
Prelude> fmap (++"!") Nothing
Nothing
Prelude> Just (+3) <*> Just 3
Just 6
Prelude> Nothing <*> Just "fact"
Nothing
```

`Maybe` contains value with maybe have Nothing: 
if you put nothing, then it should be nothing since there's literally nothing to apply.

Then, how can we use `Maybe` as `>>=`?

Let's use `myJust x = Just (x+1)`

```
*Main> myJust 1
Just 2
*Main> myJust 100
Just 101
```

Instead of `>>=`, first use `applyMaybe`
```
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x
```

```
*Main> applyMaybe (Just 3) myJust 
Just 4
*Main> applyMaybe (Just "smile") (\x -> Just (x ++ " :)"))
Just "smile :)"
*Main> applyMaybe Nothing (\x -> Just (x ++ " :)"))
Nothing
```

What if function returns Nothing?

`positiveJust x = if x > 0 then Just x else Nothing`

```
*Main> applyMaybe (Just 3) positiveJust
Just 3
*Main> applyMaybe (Just (-1)) positiveJust
Nothing
```

Not that special...

## [Monad](./app/Monad.hs)

### Monad class
```
class Monad m where
    return :: a -> m a
    
    (>>=) :: m a -> (a -> m b) -> m b
    
    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y
    
    fail :: String -> m a
    fail msg = error msg
```

Why `Monad m` instead `(Applicative m) => Monad m`? 

That's because there were no `Applicative` for haskell... 
But `Monad` is always `Applicative`

1. return: it's equal to `pure` in `Applicative`, 
but returns value with least default context

We met this in IO handling: we used this for fake IO process. 

2. `>>=`: binding

It accept monad value and put it in function that get normal value and returns monad value.

3. `>>`: It has default constructor, but we don't use it very often.

4. `fail`: handling error

### `Maybe`: instance of `Monad`

```
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing
```

`>>=` is equal to `applyMaybe` we made above.

```
Prelude> return "what" :: Maybe String
Just "what"
Prelude> Just 9 >>= \x -> return (x*10)
Just 90
Prelude> Nothing >>= \x -> return (x*10)
Nothing
```

### wirewalking: how to deal with fail-prone context
  
  * introduction
    
Let's think Pierre try to wirewalk. When he uses stick, some birds sit on it. 

If the difference of number of birds are less than 3, he will safe. 
But if number gets over or equal to 4, then he will fall.

```
type Birds = Int
type Stick = (Birds, Birds)

landleft :: Birds -> Stick -> Stick
landleft n (left, right) = (left + n, right)

landright :: Brids -> Stick -> Stick
landright n (left, right) = (left, right + n)
```

Let's add `x -: f = f x` to reverse function and parameter. 
It works like this
```
*Main> landleft 2 (0,0)
(2,0)
*Main> (0,0) -: landleft 1 -: landright 1 -: landleft 2
(3,1)
*Main> landright (-1) (2,1)
(2,0)
```

He will fall in these cases:
```
*Main> landleft 10 (0,3)
(10,3)
*Main> (0,0) -: landleft 1 -: landright 4 -: landleft (-1) -: landright (-2)
(0,2)
```
In second case, the difference became over 3

  * `Maybe`
    
Let's add `Maybe` context
```
landleft :: Birds -> Stick -> Maybe Stick
landleft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landright :: Birds -> Stick -> Maybe Stick
landright n (left, right) 
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise = Nothing
```

```
*Main> landleft 2 (0,0)
Just (2,0)
*Main> landleft 10 (0,3)
Nothing
*Main> (0,0) -: landleft 1 -: landright 4 -: landleft (-1) -: landright (-2)

<interactive>:21:24: error:
```

It works well, but you can't use it consecutively anymore...

  * `Monad`
    
```
*Main> landright 1 (0,0) >>= landleft 2
Just (2,1)
*Main> Nothing >>= landleft 2
Nothing
*Main> return (0,0) >>= landright 2 >>= landleft 2 >>= landright 2
Just (2,4)
```

But you can use `>>=` instead

```
*Main> return (0,0) >>= landleft 1 >>= landright 4 >>= landleft (-1) >>= landright (-2)
Nothing
```

If there are failure in process, it returns Nothing.

  * `banana`
    
```
banana :: Stick -> Maybe Stick
banana _ = Nothing
```
Let's make function that ignores context: 
`banana` on the wire make always falls Pierre fall.

```
*Main> return (0,0) >>= landleft 1 >>= banana >>= landright 1
Nothing
```

Or we can just use `>>` instead: `x >> y = x >>= \_ -> y`

```
*Main> Nothing >> Just 3
Nothing
*Main> Just 3 >> Just 4
Just 4
*Main> Just 3 >> Nothing
Nothing

*Main> return (0,0) >>= landleft 1 >> Nothing >>= landright 1
Nothing
```

## [DoNotation](./app/DoNotation.hs)

### do

do notation is special statement for monad: we already shown this in chapter 8 -IO. 

```
Prelude> Just 3 >>= (\x -> Just (show x ++ "!"))
Just "3!"
```

Then, how about `>>=` is already in lambda?

```
Prelude> Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
Just "3!"

Prelude> Nothing >>= (\x -> Just (show x ++ "!"))
Nothing
Prelude> Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))
Nothing
```

It's like let notation

```
Prelude> let x = 3; y = "!" in show x ++ y
"3!"
```

The difference is monad can fail.
```
foo0 :: Maybe String
foo0 = 
    Just 3 >>= (\x ->
    Just "!" >>= (\y ->
        Just (show x ++ y)))
```
This can be written in do notation

```
foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)
```
It's more simple!

### let

In do notation, everything not in let block is monad.

To see result, we use `<-`.

```
Prelude> Just 9 >>= (\x -> Just (x > 8))
Just True

overEight :: Maybe Bool
overEight = do
    x <- Just 9
    Just (x > 8)
```

### wirewalking

in [app/Monad.hs](./app/Monad.hs)

```
routine :: Maybe Stick
routine = do
    start <- return (0,0)
    first <- landleft 2 start
    second <- landright 2 first
    landleft 1 second
```

```
*Main> routine
Just (3,2)
```
He's doing well.

In do statement, each code line depends on previous one: up to bottom.

Without monad, you should write it this way.
```
routine1 :: Maybe Stick
routine1 = 
    case Just (0,0) of
        Nothing -> Nothing
        Just start -> case landleft 2 start of
            Nothing -> Nothing
            Just first -> case landright 2 first of
                Nothing -> Nothing
                Just second -> landleft 1 second
```

### Pattern matching and failure

```
justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x
```

To get 'h', we used pattern matching. What if it fails?

```
fail :: (Monad m) => String -> m a
fail msg = error msg
```

We use fail function. But `Maybe` has its own implementation.

`fail _ = Nothing`

```
gonnaFail :: Maybe Char
gonnaFail = do
    (x:xs) <- Just ""
    return x
```
This contains fail.

```
*Main> gonnaFail
Nothing
```
It returns Nothing.


## [ListMonad](./app/ListMonad.hs)

### first


1

1

1

- some
