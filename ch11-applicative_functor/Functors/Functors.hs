--Functors can be maaped like list, tree, Maybe...
--Let's look at IO instance
{-
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
-}

import qualified Data.Char as DC
import qualified Data.List as DL


main0 = do
    line <- getLine
    let line' = reverse line
    putStrLn $ "Input string: " ++ line
    putStrLn $ "Backward: " ++ line'
{-
    this is my input
    Input string: this is my input
    Backward: tupni ym si siht
-}

main = do line <- fmap (DL.intersperse '-' . reverse . map DC.toUpper) getLine
            putStrLn line
