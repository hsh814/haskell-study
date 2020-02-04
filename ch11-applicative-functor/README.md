# ch11-applicative-functor

Files are in [app](./app) directory

## [Functors](./app/Functors.hs)

- IO
    ```
    instance Functor IO where
        fmap f action = do
            result <- action 
            return (f result)
    ```

- Functor `(->) r` :: inside of Control.Monad.Instances   
    ```
    instance Functor ((->) r) where
            fmap f g = (\x -> f (g x))
    ```
    fmap can be written as
  
    `fmap :: ( a-> b) -> ((->) r a) -> ((->) r b)`
  
    this is same as infix version below
  
    `fmap :: (a -> b) -> (r -> a) -> (r -> b)`
  
    `(->) r` is instance of Functor
  
    ```
    instance Functor ((->) r) where
        fmap = (.)
    ```
    ```
    Prelude Control.Monad.Instances> :t fmap (*3) (+100)
    fmap (*3) (+100) :: Num b => b -> b
    Prelude Control.Monad.Instances> fmap (*3) (+100) 1
    303
    Prelude Control.Monad.Instances> fmap (show . (*3)) (+100) 1
    "303"
    ```
<br> 
 
- lifting

    fmap is compound function:
  
    `fmap :: (a -> b) -> (f a -> f b)`
  
    it accepts a function and returns new function
  
    ```
    Prelude Control.Monad.Instances> :t fmap (*2)
    fmap (*2) :: (Functor f, Num b) => f b -> f b
    Prelude Control.Monad.Instances> :t fmap (replicate 3)
    fmap (replicate 3) :: Functor f => f a -> f [a]
    ```
    If you use Maybe a, Nothing stays Nothing
    ```
    Prelude Control.Monad.Instances> fmap (replicate 3) [1..4]
    [[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
    Prelude Control.Monad.Instances> fmap (replicate 3) (Just 4)
    Just [4,4,4]
    Prelude Control.Monad.Instances> fmap (replicate 3) (Right "blah")
    Right ["blah","blah","blah"]
    Prelude Control.Monad.Instances> fmap (replicate 3) Nothing
    Nothing
    ```
  
## [FunctorRule](./app/FunctorRule.hs)
- Functor rule
    
    Every instances of Functor follows two rules:
    1. id
        
        If you map id function to Functor value, return value must be equal to original
        ```
        Prelude Control.Monad.Instances> fmap id (Just 3)
        Just 3
        Prelude Control.Monad.Instances> id (Just 3)
        Just 3
        Prelude Control.Monad.Instances> fmap id [1..5]
        [1,2,3,4,5]
        Prelude Control.Monad.Instances> fmap id []
        []
        Prelude Control.Monad.Instances> fmap id Nothing
        Nothing
        ```
       
    2. compund function
        
        `fmap (f . g) = fmap f . fmap g`
        
        or
        
        `fmap (f . g) x = fmap f (fmap g x)`    
        
        If you put Nothing, fmap always return Nothing
        
        ```
        instance Functor Maybe where
            fmap f (Just x) = Just (f x)
            fmap f Nothing = Nothing
        ```
    
    3. breaking the rules
        
        Let's see strange example of non-Functor which is instance of Functor, but do not follow those rules
        
        `data CMaybe a = CNothing | CJust Int a deriving (Show)`
        ```
        *Main> CNothing
        CNothing
        *Main> CJust 0 "haha"
        CJust 0 "haha"
        *Main> :t CNothing
        CNothing :: CMaybe a
        *Main> :t CJust 0 "haha"
        CJust 0 "haha" :: CMaybe [Char]
        *Main> CJust 100 [1..4]
        CJust 100 [1,2,3,4]
        ```
       
        When you use fmap, first field increases 1
        ```
        instance Functor CMaybe where
            fmap f CNothing = CNothing
            fmap f (CJust counter x) = CJust (counter + 1) (f x)
        ```
        
        test
        ```
        *Main> fmap (++"haha") (CJust 0 "ho")
        CJust 1 "hohaha"
        *Main> fmap (++"ha") $ fmap (++"heh") (CJust 0 "ho")
        CJust 2 "hohehha"
        *Main> fmap (++"blah") CNothing
        CNothing
        ```
       
        This is not a Functor since
        ```
        *Main> fmap id (CJust 0 "haha")
        CJust 1 "haha"
        *Main> id (CJust 0 "haha")
        CJust 0 "haha"
        ```
       
## [ApplicativeFunctor](./app/ApplicativeFunctor.hs)       

- wrapped
    
    ```
    Prelude> :t fmap (++) (Just "hey")
    fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
    Prelude> :t fmap compare (Just 'a')
    fmap compare (Just 'a') :: Maybe (Char -> Ordering)
    Prelude> :t fmap compare "A List Of Chars"
    fmap compare "A List Of Chars" :: [Char -> Ordering]
    Prelude> :t fmap (\x y z -> x + y / z) [1..4]
    fmap (\x y z -> x + y / z) [1..4]
      :: (Fractional a, Enum a) => [a -> a -> a]
    ```
    if you call `fmap (*) (Just 3)`, then it returns `Just ((*) 3)` or `Just (3 *)`, and this is function wrapped with `Just`

- Applicative

    `Control.Applicative` has `Applicative` Type Class which has two function `pure` and `<*>`
    
    ```
    class (Functor f) => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
    ```
    `f` must be `Functor`, `pure` makes Type to `Applicative Functor`. 
    `<*>` is advanced version of `fmap`.
    
- Maybe Applicative

    ```
    instance Applicative Maybe where
        pure = Just
        Nothing <*> _ = Nothing
        (Just f) <*> something = fmap f something
    ```
    
    example
    ```
    Prelude> Just (+3) <*> Just 9
    Just 12
    Prelude> pure (+3) <*> Just 10
    Just 13
    Prelude> pure (+3) <*> Just 9
    Just 12
    Prelude> Just (++ "haha") <*> Nothing
    Nothing
    Prelude> Nothing <*> Just "hahahaj"
    Nothing
    ```
    `Just (+3)` and `pure (+3)` works same way: if you use `Maybe`, then use `pure`
    
    
    
- Applicative Style

    You can use multiple `<*>` to deal with multiple Applicatives.
    
    ```
    Prelude> pure (+) <*> Just 3 <*> Just 5
    Just 8
    Prelude> pure (+) <*> Just 3 <*> Nothing
    Nothing
    Prelude> pure (+) <*> Nothing <*> Just 5
    Nothing
    ```
        
    `<*>` is left-associative: 
    
    `pure (+) <*> Just 3 <*> Just 5` is equal to
    
    `(pure (+) <*> Just 3) <*> Just 5`
    
- `<$>`: infix version of `fmap`
    
    ```
    (<$>) :: (Functor f) => (a -> b) -> f a -> f b
    f <$> x = fmap f x
    ```
  
    example
    ```
    Prelude> (++) <$> Just "anna" <*> Just "elsa"
    Just "annaelsa"
    Prelude> (++) "anna" "elsa"
    "annaelsa"
    ```
    
    `f <$> x <*> y <*> z` is similar to non-functor version `f x y z`
    
- Other Applicative Functor 
    * List
        
        List([]) is Also Applicative Functor
        
        ```
        instance Applicative [] where
            pure x = [x]
            (<*>) :: [a -> b] -> [a] -> [b]
            fs <*> xs = [f x | f <- fs, x <- xs]
        ```    
      
        example
        
        ```
        Prelude> pure "Hey" :: [String]
        ["Hey"]
        Prelude> pure "Hey" :: Maybe String
        Just "Hey"
        ```
        
        Let's use them
        ```
        Prelude> [(*0), (+100), (^2)] <*> [1..4]
        [0,0,0,0,101,102,103,104,1,4,9,16]
        ```
      
        Apply two functions to two lists
        ```
        Prelude> [(+), (*)] <*> [1,2] <*> [3,4]
        [4,5,5,6,3,4,6,8]
        ```
        `<$>` example
        ```
        Prelude> (++) <$> ["ha", "heh", "hmm"] <*> ["~", "!"]
        ["ha~","ha!","heh~","heh!","hmm~","hmm!"]
        ```
        
        Nondeterministic computation: unlike 100 or "hello", 
        [1,2,3] can show all the results which cannot be determined 
        <br>
        
    * IO
        ```
        instance Applicative IO where
            pure = return
            (<*>) :: IO (a -> b) -> IO a -> IO b
            a <*> b = do
                f <- a
                x <- b
                return (f x)
        ```
        
        `return` is IO process doing nothing
    
        sequencing: it combine two IO into one.
        ``` 
        myAction :: IO String
        myAction = do
            a <- getLine
            b <- getLine
            return $ a ++ b
        ```
        is equal to
        ```
        myAction :: IO String
        myAction = (++) <$> getLine <*> getLine
        ```
        
- Applicative function: `(->) r`
    
    ``` 
    instance Applicative ((->) r) where
        pure x = (\_ -> x)
        f <*> g = \x -> f x (g x)    
    ```
  
    `pure` ignores parameter
    ```
    Prelude> (pure 3) "blah"
    3
    Prelude> pure 3 "blah"
    3
    ```
  
    `<*>`
    ```
    Prelude> :t (+) <$> (+3) <*> (*100)
    (+) <$> (+3) <*> (*100) :: Num b => b -> b
    Prelude> (+) <$> (+3) <*> (*100) $ 5
    508
    ```
    
    
    