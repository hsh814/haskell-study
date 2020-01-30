--prefix calculation

solveRPN :: String -> Double
solveRPN expression = head (foldl foldingFunction [] (words expression))
    where
        foldingFunction (x:y:ys) "*" = (y * x) : ys
        foldingFunction (x:y:ys) "+" = (y + x) : ys
        foldingFunction (x:y:ys) "-" = (y - x) : ys
        foldingFunction (x:y:ys) "/" = (y / x) : ys
        foldingFunction (x:y:ys) "^" = (y ** x) : ys
        foldingFunction (x:xs) "ln" = log x : xs
        foldingFunction xs "sum" = [sum xs]
        foldingFunction xs numberString = read numberString:xs
{-
    *Main> solveRPN "10 4 3 + 2 * -"
    -4.0
    *Main> solveRPN "2.7 ln"
    0.9932517730102834
    *Main> solveRPN "10 10 10 10 sum 4 /"
    10.0
    *Main> solveRPN "10 2 ^"
    100.0
-}

{-
main = do
    putStr "Input expression: "
    expression <- getLine
    putStrLn "Result: " ++ show $ solveRPN expression
-}