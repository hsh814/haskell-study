import Data.Char
import Data.List

main1 = do
    line <- getLine
    let line' = reverse line
    putStrLn $ "Your input: " ++ line
    putStrLn $ "Output: " ++ line'
{-
    This is my input
    Your input: This is my input
    Output: tupni ym si sihT
-}

main2 = do
    line <- fmap reverse getLine
    putStrLn $ "Output: " ++ line
{-
    This is my second input
    Output: tupni dnoces ym si sihT
-}

main = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line

{-
    hello there
    E-R-E-H-T- -O-L-L-E-H
-}








