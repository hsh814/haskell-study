--haskell is pure: it has referential transparency
--then, how can we get random number?
--You should use IO to do that...
import qualified System.Random as SR

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



