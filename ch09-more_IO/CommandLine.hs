--We hard coded filename "todo.txt"
--Now, we get filename from commandline as argument
import qualified System.Environment as SE
import qualified Data.List as DL
import qualified System.Directory as SD
import qualified System.IO as SIO
import qualified Control.Exception as CE

main0 = do
    args <- SE.getArgs
    program <- SE.getProgName
    putStrLn "The arguments are: "
    mapM putStrLn args
    putStrLn "The program name is: " 
    putStrLn program
{-
$ runhaskell CommandLine.hs -h -d myfile.txt
The arguments are: 
-h
-d
myfile.txt
The program name is: 
CommandLine.hs
-}

--Let's make program that edit list
--argument: add, view, remove
add :: [String] -> IO ()
add [fileName, todoList] = SIO.appendFile fileName (todoList ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- SIO.readFile fileName
    let 
        todoTasks = lines contents
        numberedTasks = zipWith (\n line -> (show n) ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numStr] = do
    contents <- SIO.readFile fileName
    let 
        todoTasks = lines contents
        num = read numStr
        newTodoList = unlines $ DL.delete (todoTasks !! num) todoTasks
    CE.bracketOnError (SIO.openTempFile "." "temp")
        (\ (tempName, tempHandle) -> do
            SIO.hClose tempHandle
            SD.removeFile tempName)
        (\ (tempName, tempHandle) -> do
            SIO.hPutStr tempHandle newTodoList
            SIO.hClose tempHandle
            SD.removeFile "todo.txt"
            SD.renameFile tempName "todo.txt")

commandNotExist :: String -> [String] -> IO ()
commandNotExist command _ = 
    putStrLn $ command ++ " command doesn't exist"

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch command = commandNotExist command

main = do
    (command:argList) <- SE.getArgs
    dispatch command argList
{-
./CommandLine view todo.txt
0 - Beetlejuice
1 - Beetlejuice
2 - Bee... cause!
3 - You're so smart, a stand-up bro 

./CommandLine add todo.txt "I'll let you know"

./CommandLine view todo.txt
0 - Beetlejuice
1 - Beetlejuice
2 - Bee... cause!
3 - You're so smart, a stand-up bro 
4 - I'll let you know

./CommandLine remove todo.txt 4

./CommandLine view todo.txt
0 - Beetlejuice
1 - Beetlejuice
2 - Bee... cause!
3 - You're so smart, a stand-up bro 

./CommandLine not todo.txt
not command doesn't exist
-}






