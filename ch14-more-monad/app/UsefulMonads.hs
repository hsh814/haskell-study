import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

joinedMaybes :: Maybe Int
joinedMaybes = do
    m <- Just (Just 8)
    m


keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throw away."]
        return False


powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs