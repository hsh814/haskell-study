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

