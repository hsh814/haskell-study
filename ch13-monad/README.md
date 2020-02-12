# ch13-monad

## Contents
- [Maybe](#Maybe)
- [Monad](#Monad)

## [Maybe](./app/Maybe)

What is monad?

Functor -> Applicative Functor -> Monad!

Functor: wrap data type

`fmap :: (Functor f) => (a -> b) -> f a -> f b`

Applicative: unwrap wrapped data and process it and wrap it

`(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b`

Monad: extension of Applicative: with a, returns value with context

`(>>=) :: (Monad m) => m a -> (a -> m b) -> m b`

`>>=` is called bind: with context and normal value, it returns contexted value

- `Maybe`

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

- `Maybe`: instance of `Monad`

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

- wirewalking: how to deal with fail-prone context
    - introduction
    
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

    - `Maybe`
    
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

    - `Monad`
    
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

    - `banana`
    
Let's make function that ignores context: 
`banana` on the wire make always falls Pierre fall.

```
banana :: Stick -> Maybe Stick
banana _ = Nothing
```


