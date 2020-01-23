import qualified Control.Monad as CM
import qualified Data.Char as DC

--Some useful IO functions
--putStr
main0 = do
    putStr "Hey,"
    putStr "there"
{-
putStr: just print string without \n
    Hey,there
-}
    putChar 't'
    putChar 'w'
    putChar 's'
{-
putChar: print char..
    tws
-}

putStr1 :: String -> IO ()
putStr1 [] = return ()
putStr1 (x:xs) = do
    putChar x
    putStr xs
--You can define putStr with putChar recursively

main1 = do
    print True
    print 2
    print "hahaha"
    print 3.14
    print [2,4,6]
{-
print: print type as show
print = putStrLn . show
    True
    2
    "hahaha"
    3.14
    [2,4,6]
ghci use print when we input something
-}


main2 = do
    input <- getLine
    CM.when (input == "END") $ do
        putStrLn input
--when
{-
    *Main> main2
    ii
    *Main> main2
    END
    END
-}
--it's equal to
main3 = do
    input <- getLine
    if (input == "END")
        then putStrLn input
        else return ()

--sequence
main4 = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]
--is equal to
main5 = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
{-
    my
    new
    sequence
    ["my","new","sequence"]
map print [1..5] does not produce IO process, 
but it will make list of IO process
    *Main> sequence $ map print [1..5]
    1
    2
    3
    4
    5
    [(),(),(),(),()]
last line is because print returns empty tuple
if you use getLine, then it became [String] since 
getLine :: IO String
-}

--mapM: sequence $ map IOfunction list
--is very commonly used
{-
    *Main> mapM print [1..5]
    1
    2
    3
    4
    5
    [(),(),(),(),()]
-}
--mapM_ : mapM without last tuple list
{-
    *Main> mapM_ print [1..5]
    1   
    2
    3
    4
    5
-}

--forever
main6 = CM.forever $ do
    putStr "input: "
    ln <- getLine
    putStrLn $ map DC.toUpper ln
{-
    input: input
    INPUT
    input: main6
    MAIN6
infinite loop
-}

--forM
main7 = do
    colors <- CM.forM [1..4] (\a -> do
        putStrLn $ "Which color do you associate with the number"
                    ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1,2,3,4 are: "
    CM.mapM putStrLn colors



