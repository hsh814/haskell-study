# ch11-applicative-functor

## [Functors](./app/Functors.hs)
- IO
  ```
  instance Functor IO where
        fmap f action = do
            result <- action 
            return (f result)
  ```

- Functor `(->) r`    
  ```
  instance Functor ((->) r) where
            fmap f g = (\x -> f (g x))
  ```
