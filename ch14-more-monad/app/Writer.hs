import Control.Monad
import Data.Monoid
import Control.Monad.Writer

isBigGang :: Int -> Bool
isBigGang x = x > 9

isBigGang' :: Int -> (Bool, String)
isBigGang' x = (x > 9, "compare to 9")

--applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
--applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, mappend log newLog)



type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

{-
newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
-}

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a * b)