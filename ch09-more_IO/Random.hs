--haskell is pure: it has referential transparency
--then, how can we get random number?
--You should use IO to do that...
import qualified System.Random as SR
import Control.Monad (when)

--New type class: RandomGen: source of random
--random :: (SR.RandomGen g, SR.Random a) => g -> (a, g)
{-
first: random Int, second: random generator
    Prelude System.Random> random (mkStdGen 100) :: (Int, StdGen)
    (-3633736515773289454,693699796 2103410263)
same result for same input
    Prelude System.Random> random (mkStdGen 100) :: (Int, StdGen)
    (-3633736515773289454,693699796 2103410263)
    Prelude System.Random> random (mkStdGen 101) :: (Int, StdGen)
    (4401549314573480025,2040087561 2103410263)

-}

--let's make program that simulate throwing three coins
threeCoins :: SR.StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let 
        (firstCoin, newGen) = SR.random gen
        (secondCoin, newGen1) = SR.random newGen
        (thirdCoin, newGen2) = SR.random newGen1
    in
        (firstCoin, secondCoin, thirdCoin) 

{-
    *Main> threeCoins (SR.mkStdGen 21)
    (True,True,True)
    *Main> threeCoins (SR.mkStdGen 211)
    (True,True,False)
-}

--randoms: infinite random list
randoms' :: (SR.RandomGen g, SR.Random r) => g -> [r]
randoms' gen = 
    let (value, newGen) = SR.random gen 
    in value: randoms' newGen
{-
    *Main> take 5 $ SR.randoms (SR.mkStdGen 12) :: [Int]
    [-539234551526347469,-8970766343904053269,6222293557510449348,7571659635591020082,-8920558447226095803]
-}


--finite randoms
finiteRandoms0 :: (SR.RandomGen g, SR.Random r, Num n, Eq n) => n -> g -> ([r], g)
finiteRandoms0 0 gen = ([], gen)
finiteRandoms0 n gen = 
    let 
        (value, newGen) = SR.random gen
        (restOfList, finalGen) = finiteRandoms0 (n-1) newGen
    in
        (value: restOfList, finalGen)
 {-
    *Main> finiteRandoms0 10 (SR.mkStdGen 211)
    ([2839264684331777356,-1130764536171600784,7420818779996892622,
    -3200321605962687680,-806962414829970544,3957437243796982158,
    -6189788485647970935,-3315155221524155176,1541899790980098892,
    4892165197559326987],650737682 1701490540)
 -}

--randomR: range
{-
    *Main> SR.randomR (1,6) (SR.mkStdGen 329239)
    (6,289307982 40692)
infinite version: randomRs
    *Main> take 10 $ SR.randomRs ('a','z') (SR.mkStdGen 292) :: [Char]
    "xxwypxyjqf"
-}



--Random and IO
main0 = do
    gen <- SR.getStdGen
    putStrLn $ take 20 (SR.randomRs ('a','z') gen)
{-
    *Main> main
    briwjjqngpqdmobjtaso
    $ stack runhaskell Random.hs
    cbjwwgngqgeqleurnvvk
-}

main1 = do
    gen <- SR.getStdGen
    putStrLn $ take 20 (SR.randomRs ('a','z') gen)
    gen1 <- SR.getStdGen
    putStrLn $ take 20 (SR.randomRs ('a','z') gen1)
{-
exactly same result
    fbbvhauvocmpzvlipnsi
    fbbvhauvocmpzvlipnsi
-}

main2 = do
    gen <- SR.getStdGen
    putStrLn $ take 20 (SR.randomRs ('a','z') gen)
    gen1 <- SR.newStdGen
    putStrLn $ take 20 (SR.randomRs ('a','z') gen1)
{-
new result!
    dimgojtcsxnddaqtbskq
    fauoippjilpobcoytbhc
-}

main = do
    gen <- SR.getStdGen
    askForNumber gen

askForNumber :: SR.StdGen -> IO ()
askForNumber gen = do
    let 
        (randNum, newGen) = SR.randomR (1,10) gen :: (Int, SR.StdGen)
    putStrLn "Guess number between 1 to 10: "
    numStr <- getLine
    when (not $ null numStr) $ do
        let number = read numStr
        if randNum == number 
            then putStrLn "You are correct!"
            else putStrLn $ "Wrong number! Answer: " ++ show randNum
        askForNumber newGen
{-
    5
    Wrong number! Answer: 8
    Guess number between 1 to 10: 
    2
    Wrong number! Answer: 3
    Guess number between 1 to 10: 
    7
    Wrong number! Answer: 5
-}



