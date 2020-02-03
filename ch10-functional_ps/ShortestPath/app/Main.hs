import qualified Data.List as DL

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)

type RoadSystem = [Section]

roadToLondon :: RoadSystem
roadToLondon = [
    Section 50 10 30,
    Section 5 90 20,
    Section 40 2 25,
    Section 10 8 0
    ]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let timeA = sum (map snd pathA)
        timeB = sum (map snd pathB)
        forwardTimeToA = timeA + a
        crossTimeToA = timeB + b + c
        forwardTimeToB = timeB + b
        crossTimeToB = timeA + a + c
        newPathToA = if forwardTimeToA <= crossTimeToA
                        then (A, a) : pathA
                        else (C, c) : (B, b) : pathB
        newPathToB = if forwardTimeToB <= crossTimeToB
                        then (B, b) : pathB
                        else (C, c) : (A, a) : pathA
    in (newPathToA, newPathToB)


optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
    in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath

{-
    *Main> optimalPath roadToLondon
    [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]
-}

--now, read roadsystem from file
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
    contents <- getContents
    let 
        threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = DL.concat $ map (show . fst) path
        pathTime = sum $ map snd path
    putStrLn $ "The shortest path is: " ++ pathString
    putStrLn $ "Time take: " ++ show pathTime
{-
    $ ./Main < mypath.txt
    The shortest path is: BCACBBC
    Time take: 75
-}