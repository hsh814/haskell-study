# ch11-applicative-functor

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



