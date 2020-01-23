--Let's read and write file without terminal
import qualified System.IO as SIO

main = do 
    handle <- SIO.openFile "text.txt" SIO.ReadMode
    contents <- SIO.hGetContents handle
    putStr contents
    SIO.hClose handle

--openFile
{-
    *Main> :t SIO.openFile
    SIO.openFile :: FilePath -> SIO.IOMode -> IO SIO.Handle
IOMode is enum type
    data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
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







