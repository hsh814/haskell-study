--Let's read and write file without terminal
import qualified System.IO as SIO
import qualified Control.Exception as CE

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
main = do
    contents <- readFile "text.txt"
    putStr contents


