import Control.Monad
import Data.Monoid

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