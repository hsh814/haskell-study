import Control.Monad.Writer
import Data.Monoid
import Control.Monad
import Data.Semigroup

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " % " ++ show b ++ " = " ++ show (a `mod` b)]
        return result


newtype DDiffList a = DDiffList { getDDiffList :: [a] -> [a] }

toDDiffList :: [a] -> DDiffList a
toDDiffList xs = DDiffList (xs++)

fromDDiffList :: DDiffList a -> [a]
fromDDiffList (DDiffList f) = f []

instance Semigroup (DDiffList a) where
  (DDiffList f) <> (DDiffList g) = DDiffList (f <> g)
  
instance Monoid (DDiffList a) where
    mempty = DDiffList (\xs -> [] ++ xs)
    (DDiffList f) `mappend` (DDiffList g) = DDiffList (\xs -> f (g xs))


gcd' :: Int -> Int -> Writer (DDiffList String) Int
gcd' a b
    | b == 0 = do
        tell (toDDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcd' b (a `mod` b)
        tell (toDDiffList [show a ++ " % " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

finalCountDown :: Int -> Writer (DDiffList String) ()
finalCountDown 0 = do
    tell (toDDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDDiffList [show x])

slowCountDown :: Int -> Writer [String] ()
slowCountDown 0 = do
    tell ["0"]
slowCountDown x = do
    slowCountDown (x-1)
    tell [show x]