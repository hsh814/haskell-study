import qualified Control.Monad as CM
import Data.Char as DC

main0 = CM.forever $ do
    ln <- getLine
    putStrLn $ map toUpper ln
{-
ghc --make ToUpperCase
./ToUpperCase < haiku.txt
    I'M A LIL' TEAPOT
    WHAT'S WITH THAT AIRPLANE FOOD, HUH?
    IT'S SO SMALL, TASTELESS
    ToUpperCase: <stdin>: hGetLine: end of file
-}

--getContents :: IO String
--read file to EOF as string
--getContents is lazy: it reads file when needed
main = do
    contents <- getContents
    putStr $ map toUpper contents
--is equal to main0

