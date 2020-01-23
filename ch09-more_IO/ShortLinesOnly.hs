shortLinesOnly :: String -> String
shortLinesOnly str = unlines . filter (\line -> length line < 10) $ lines str

main0 = do
    contents <- getContents
    putStr $ shortLinesOnly contents

{-
ghc --make ShortLinesOnly
./ShortLinesOnly < shortlines.txt
    I'm short
    so am I
    sl
-} 


--interact: read string as input, put it in function, and output result
main = interact shortLinesOnly
--same result