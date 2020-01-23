--I/O cause side effect
--But haskell's function is pure: they don't have side effect
--Then, how can we use input or output?
import qualified Data.Char as DC 
--main = putStrLn "Hello, world!"
main0 = putStrLn "Hello, world!"
{-
compile
    ghc --make HelloWorld
output
    ./HelloWorld
    Hello, world!
-}
{-
    *Main> :t putStrLn
    putStrLn :: String -> IO ()
it returns IO () : () is unit == empty tuple, result of IO
it starts when main
-}

--use multiple IO: do
main1 = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hello, " ++ name ++ "!"
{-
    *Main> :t getLine
    getLine :: IO String
compile
    Hello, what's your name?
    shhan
    Hello, shhan!
-}
--Since every IO process produce result, you can do this
main2 = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hello, " ++ name ++ "!"
--if you do this,
--    myLine = getLine
--myLine became function, not result of getLine


main3 = do
    putStrLn "What's your first name?"
    firstname <- getLine
    putStrLn "What's your last name?"
    lastname <- getLine
    let bigFirstName = map DC.toUpper firstname
        bigLastName = map DC.toUpper lastname
    putStrLn $ "Your name is: " ++ bigFirstName ++ " " ++ bigLastName

--You can use let
--But let firstname = getLine is also just function
--Binding must be done like <-
{-
    What's your first name?
    fname 
    What's your last name?
    lname
    Your name is: FNAME LNAME
-}

main4 = do 
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
{-
I used else do since main is IO
    *Main> :t main
    main :: IO ()
test
    hello, world
    ,olleh dlrow
    frankie foster
    eiknarf retsof
    clean up all the mass
    naelc pu lla eht ssam
-}

main = do
    return ()
    a <- return "Haha"
    line <- getLine
    return "Something"
    return 4
    putStrLn line

--return is just value: it does not end IO do block
--return is opposite of <-: you can use let instead
--You can use return at making IO do nothing
{-
    pizza
    pizza
-}

