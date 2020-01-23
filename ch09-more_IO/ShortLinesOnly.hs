main = do
    contents <- getContents
    putStr $ shortLinesOnly contents

shortLinesOnly :: String -> String
shortLinesOnly str = unlines . filter (\line -> length line < 10) $ lines str
{-
ghc --make ShortLinesOnly
./ShortLinesOnly < shortlines.txt
    txt
    I'm short
    so am I
    sl
-} 