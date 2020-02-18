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
- [Reader](#Reader)
- [State](#State)
- [Error](#Error)
- [UsefulMonads](#UsefulMonads)
- [SafeRPN](#SafeRPN)


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

### mappend

This is efficient

```
a ++ (b ++ (c ++ d))
```

But this is inefficient

```
((a ++ b) ++ c) ++ d
```

Becuase for every time `mappend` is called, left side is reconstructed.

For example,
```
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " % " ++ show b ++ " = " ++ show (a `mod` b)]
        return result
```

```
Finished with 3
15 % 3 = 0
48 % 15 = 3
```

It's in reverse order: extremely inefficient.

How can we resolve this?

### Difference list

It's list that accept a list and add another list in front of it.

```
f `append` g = \xs -> f (g xs)
```

```
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DDiffList a) where
  (DDiffList f) <> (DDiffList g) = DDiffList (f <> g)
  
instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))
```

```
*Main> fromDDiffList (toDDiffList [1,2,3,4] `mappend` toDDiffList [1,2,3])
[1,2,3,4,1,2,3]
```

Let's apply this to gcd

```
gcd' :: Int -> Int -> Writer (DDiffList String) Int
gcd' a b
    | b == 0 = do
        tell (toDDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcd' b (a `mod` b)
        tell (toDDiffList [show a ++ " % " ++ show b ++ " = " ++ show (a `mod` b)])
        return result
```

```
*Main> mapM_ putStrLn . fromDDiffList . snd . runWriter $ gcd' 110 34
Finished with 2
8 % 2 = 0
34 % 8 = 2
110 % 34 = 8
```

### Compare Performance

```
finalCountDown :: Int -> Writer (DDiffList String) ()
finalCountDown 0 = do
    tell (toDDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDDiffList [show x])
```

It's much faster than

```
slowCountDown :: Int -> Writer [String] ()
slowCountDown 0 = do
    tell ["0"]
slowCountDown x = do
    slowCountDown (x-1)
    tell [show x]
```

## [Reader](./Reader.hs)

### Function as `Monad`

Function is `Functor`.

```
Prelude> let f = (*5)
Prelude> let g = (+3)
Prelude> fmap f g $ 8
55
```

Also, Function is `Applicative`.

```
Prelude> let h = (+) <$> (*2) <*> (+10)
Prelude> h 3
19
```

Then, is function `Monad`? Yes, it is.

```
instance Monad ((->) r) where
    return x = \_ -> x
    h >>= f = \w -> f (h w) w
```

### Reader monad

```
import Control.Monad.Instances

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)
```

`a` and `b` both are applied to 3.

```
*Main> addStuff 3
19
```

We can use let notation instead.

```
addStuff' :: Int -> Int
addStuff' x = let
    a = (*2) x
    b = (+10) x
    in a + b
```

So, function monad is called reader monad. Reader monad can treat function like value with context.

## [State](./app/State.hs)

### Seed Random

Some calculation may depend on state: this is called stateful.

```
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let
        (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in
        (firstCoin, secondCoin, thirdCoin)
```

Haskell is pure: Haskell can't change original value, so it manually returns new generator...

### Stateful

We have `State` monad for this.

```
s -> (a, s)
```

`s` is state. This is like other language's assignment.

### Stack

```
type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)
```

pop and push are stateful.

push result was () since it does not have important infromation.

```
stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), newStack1) = push 3 stack
    (a, newStack2) = pop newStack1
    in pop newStack2
```

```
*Main> stackManip [5,8,2,1]
(5,[8,2,1])
```

result is 5, and new stack is [8,2,1].
But this is too abundunt. How can we make it simple?

### State Monad

In `Control.Monad.State`

```
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = 
        State $ \s ->
            let 
                (a, newState) = h s
                (State g) = f a
            in g newState
```

We can change pop and push.

```
import Control.Monad.State

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop
```

```
*Main> runState stackManip [5,8,2,1]
(5,[8,2,1])
```

We can do more complicated work.

```
stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8
```

```
*Main> runState stackStuff [9,0,2,1,0]
((),[8,3,0,2,1,0])
```

You can use `stackManip` and `stackStuff` in do notation since they are stateful.

```
moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return ()
```

return () does nothing: stays same state.

### MonadState

```
get = state $ \s -> (s, s)

put newState = state $ \s -> ((), newState)
```

get accept state and represent it. put accept a state and make new stateful function that replaces it.

```
stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]
```

You can remake pop and push with get and put.

```
pop1 :: State Stack Int
pop1 = do
    (x:xs) <- get
    put xs
    return x

push1 :: Int -> State Stack ()
push x = do
    xs <- get
    put (x:xs)
```

```
(>>=) :: State s a -> (a -> State s b) -> State s b
```

state `s` stays same, but result `a` may be diffenent.


### Random and State monad

`System.Random` 

```
random :: (RandomGen g, Random a) => g -> (a, g)
```

Now you can see this function is stateful.

```
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a, b, c)
```

```
*Main> runState threeCoins (mkStdGen 444)
((True,False,True),2142124911 2103410263)
```


## [Error](./app/Error.hs)

### `Control.Monad.Error`

```
instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)
```

```
> :t strMsg
strMsg :: Error a => String -> a
*Main> strMsg "Boom!!"
"Boom!!"
```

We use `Either` to denote error.

```
*Main> Left "Boom!!" >>= \x -> return (x+1)
Left "Boom!!"
*Main> Left "Boom!!" >>= \x -> Left "no way!"
Left "Boom!!"
*Main> Right 100 >>= \x -> Left "no way!"
Left "no way!"
*Main> Right 100 >>= \x -> return (x+1)
Right 101
```

## [UsefulMonads](./app/UsefulMonads.hs)

### `liftM`

monad -> applicative functor -> functor

```
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x))

liftM f m = do
    x <- m
    return (f x)

fmap :: (Functor f) => (a -> b) -> f a -> f b
```

Monad don't have to depend on functor becuase of this function.

```
*Main> liftM (*3) (Just 8)
Just 24
*Main> fmap (*3) (Just 8)
Just 24
*Main> runWriter $ liftM not $ writer (True, "chickpeas")
(False,"chickpeas")
*Main> runWriter $ fmap not $ writer (True, "chickpeas")
(False,"chickpeas")
```

`<$>` is applicative version of `fmap`
and `<*>` is 

```
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
```

monad version of `<*>` is `ap`.

```
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
    f <- mf
    x <- m
    return (f x)
```

```
*Main> Just (+3) <*> Just 4
Just 7
*Main> Just (+3) `ap` Just 4
Just 7
```

`liftA2` is convenience function to apply function between two applicatives.

```
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y
```

There are `liftM2`, `liftM3`, `liftM4`...


### `join`

If monad is inside of another monad, then how can we unfold it?

```
join :: (Monad m) => m (m a) -> m a
join mm = do
    m <- mm
    m
```

```
*Main> join (Just (Just 9))
Just 9
*Main> join (Just Nothing)
Nothing
```

You can apply it to list: just concat.

```
*Main> join [[1,2,3],[9,8,7],[4,5,6]]
[1,2,3,9,8,7,4,5,6]
```

Writer, Either

```
*Main> runWriter $ join (writer (writer (1, "aaa"), "bbb"))
(1,"bbbaaa")

*Main> join (Right (Right 9)) :: Either String Int
Right 9
*Main> join (Right (Left "error")) :: Either String Int
Left "error"
*Main> join (Left "error") :: Either String Int
Left "error"
```

join works like this.

```
joinedMaybes :: Maybe Int
joinedMaybes = do
    m <- Just (Just 8)
    m
```

```
*Main> joinedMaybes
Just 8
```

The most interesting thing about join is `m >>= f` is equal to `join (fmap f m)`


### `filterM`


### `foldM`



## [SafeRPN](./app/SafeRPN.hs)





```
1

1

1


1


1

1

1

1
```