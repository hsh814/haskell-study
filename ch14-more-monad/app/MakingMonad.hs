import Data.Ratio
import Control.Monad
import Control.Applicative

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving (Show)

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [(Prob [('a', 1%2), ('b', 1%2)], 1%4),
    (Prob [('c', 1%2), ('d', 1%2)], 3%4)]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x,r) -> (x, p*r)) innerxs

instance Applicative Prob where
    pure x = Prob [(x, 1%1)]
    Prob fs <*> Prob xs = Prob $ foldB fs xs


foldB [] _ = []
foldB _ [] = []
foldB ((f, p1):fs) ((x, p2):xs) = (f x, p1 * p2) : (foldB fs xs)


instance Monad Prob where
    return x = Prob [(x, 1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

data RiggedCoin = Heads | Tails deriving (Show, Eq)

riggedCoin :: Prob RiggedCoin
riggedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- riggedCoin
    b <- riggedCoin
    c <- riggedCoin
    return (all (== Tails) [a,b,c])