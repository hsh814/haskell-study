--Let's read and write file without terminal
import qualified System.IO as SIO
import qualified Control.Exception as CE
import qualified Data.Char as DC
import qualified Data.List as DL
import qualified System.Directory as SD


main0 = do 
    handle <- SIO.openFile "text.txt" SIO.ReadMode
    contents <- SIO.hGetContents handle
    putStr contents
    SIO.hClose handle

--openFile: file path and IOMode -> return file handle
{-
    *Main> :t SIO.openFile
    SIO.openFile :: FilePath -> SIO.IOMode -> IO SIO.Handle
IOMode is enum type
    data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-}

--hGetContents: Handle -> returns IO String
{-
    *Main> :t SIO.hGetContents
    SIO.hGetContents :: SIO.Handle -> IO String
hGetContents is lazy: read file only needed.
Handle is file pointer
-}

{-
ghc --make FileStream.hs 
./FileStream
or just runhaskell FileStream.hs
result:
I think earth is a pretty great place
That's saying something, cause I've been through outer space
I think it suits me, it's just my style
I think I'll gonna stay a little while
I think that strangers are friends you haven't met
I'm blasting monsters and never break a sweat
I'm really thinking I can call this place home
-}

--withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
main1 = do
    SIO.withFile "text.txt" SIO.ReadMode (\handle -> do
        contents <- SIO.hGetContents handle
        putStr contents)

--bracket type: exception -> close the resource
--braket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
--first parameter: get resource like file handler
--second: release resource
--third: use that resource -> do something
withFile1 :: FilePath -> SIO.IOMode -> (SIO.Handle -> IO a) -> IO a
withFile1 name mode f = CE.bracket (SIO.openFile name mode)
    (\handle -> SIO.hClose handle)
    (\handle -> f handle)

--use file as string: readFile, writeFile, appendFile
main2 = do
    contents <- readFile "text.txt"
    putStr contents
    writeFile "text-up.txt" (map DC.toUpper contents)
{-
inside text-up.txt:
I THINK EARTH IS A PRETTY GREAT PLACE
THAT'S SAYING SOMETHING, CAUSE I'VE BEEN THROUGH OUTER SPACE
I THINK IT SUITS ME, IT'S JUST MY STYLE
I THINK I'LL GONNA STAY A LITTLE WHILE
I THINK THAT STRANGERS ARE FRIENDS YOU HAVEN'T MET
I'M BLASTING MONSTERS AND NEVER BREAK A SWEAT
I'M REALLY THINKING I CAN CALL THIS PLACE HOME
-}

--appendFile: just like writeFile, accept it does not delete original text
main3 = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")

{- input
$ runhaskell FileStream.hs 
Beetlejuice
$ runhaskell FileStream.hs 
Beetlejuice
$ runhaskell FileStream.hs 
Bee... cause!
$ runhaskell FileStream.hs 
You're so smart, a stand-up bro 
$ runhaskell FileStream.hs 
Let you know  

$cat todo.txt: output
Beetlejuice
Beetlejuice
Bee... cause!
You're so smart, a stand-up bro 
Let you know
-}

main = do
    contents <- readFile "todo.txt"
    let 
        todoTasks = lines contents
        numberedTasks = zipWith (\n line -> (show n) ++ " - " ++ line) [0..] todoTasks
    putStrLn "To-do list: "
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?: "
    numberString <- getLine
    let 
        num = read numberString
        newTodoList = unlines $ DL.delete (todoTasks !! num) todoTasks
    (tempName, tempHandle) <- SIO.openTempFile "." "temp"
    SIO.hPutStr tempHandle newTodoList
    SIO.hClose tempHandle
    SD.removeFile "todo.txt"
    SD.renameFile tempName "todo.txt"
{-
To-do list: 
0 - Beetlejuice
1 - Beetlejuice
2 - Bee... cause!
3 - You're so smart, a stand-up bro 
4 - Let you know
Which one do you want to delete?: 
4

$ cat todo.txt: output
Beetlejuice
Beetlejuice
Bee... cause!
You're so smart, a stand-up bro 
-}