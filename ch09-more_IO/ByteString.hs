--List is useful
--But treating file as [Char] has one weakness: it's slow
--thunk: 2:3:4:[] guarantee the list when first element is processed
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
-- lazy byte string: it access file as chunk: 64kb
{-
Word8 is 8bit Char
BSL.pack :: [Word8] -> ByteString
    *Main> BSL.pack [99,97,100]
    "cad"
    *Main> BSL.pack [50..70]
    "23456789:;<=>?@ABCDEF"
BSL.unpack :: ByteString -> [Word8]
    *Main> let by = BSL.pack [98, 111, 113, 116]
    *Main> by
    "boqt"
    *Main> BSL.unpack by
    [98,111,113,116]

-}

--strict byte string
{-
It's compatiable to lazy version
    BSL.fromChunks [BS.pack [40..45], BS.pack [46..53], BS.pack [55..60]]
    "()*+,-./012345789:;<"
-}

--ByteString's functions are similar to Data.List
--ex) head, tail, init, null, length, map, reverse, foldl, takeWhile, filter...
--System.IO -> also have similar functions: except String is ByteString
--readFile :: FilePath -> IO ByteString

import qualified System.Environment as SE
import qualified System.Directory as SD
import qualified System.IO as SIO
import qualified Control.Exception as CE

main = do
    (fileName1:fileName2:_) <- SE.getArgs
    copy fileName1 fileName2

copy source dest = do
    contents <- BSL.readFile source
    CE.bracketOnError
        (SIO.openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            SIO.hClose tempHandle
            SD.removeFile tempName)
        (\(tempName, tempHandle) -> do
            BSL.hPutStr tempHandle contents
            SIO.hClose tempHandle
            SD.renameFile tempName dest)

{-
$ ls
ByteString     CommandLine.hs  ShortLinesOnly.hs  stack1.yaml
ByteString.hi  FileStream.hs   ToUpperCase.hs     text-up.txt
ByteString.hs  Palindrome.hs   haiku.txt          text.txt
ByteString.o   Random.hs       shortlines.txt     todo.txt

./ByteString todo.txt mytodo.txt

ls
ByteString     CommandLine.hs  ShortLinesOnly.hs  shortlines.txt  todo.txt
ByteString.hi  FileStream.hs   ToUpperCase.hs     stack1.yaml
ByteString.hs  Palindrome.hs   haiku.txt          text-up.txt
ByteString.o   Random.hs       mytodo.txt         text.txt
-}
