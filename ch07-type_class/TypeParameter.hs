import qualified Data.Map as DM

--Constructor can have multiple parameters
data Maybe a = Nothing | Just a
--a is type parameter
--Mostly, type is not explicitly noted: haskell has type inference

--concrete type: Int, Bool -> no type parameters or use them all
{-
    Prelude> Just "Haha"
    Just "Haha"
    Prelude> Just 81
    Just 81
    Prelude> :t Just "haha"
    Just "haha" :: Maybe [Char]
    Prelude> :t Just 81
    Just 81 :: Num a => Maybe a
    Prelude> :t Nothing
    Nothing :: Maybe a
    Prelude> Just 10 :: Maybe Double
    Just 10.0
-}
{-
data IntMaybe = INothing | IJust Int
data StringMaybe = SNothing | SJust String
data ShapeMaybe = ShNothing | ShJust Shape
-}
--서로 다른 것들을 담을 수 있는 데이터 타입

--generic Maybe: Nothing is polymorphic
--Maybe a can act like Maybe Int
--for example, 5 can be Int or Double
--Empty list [] can be anything's list


data Car = Car {
    company :: String,
    model :: String,
    year :: Int
   }   deriving (Show)

stang = Car {company = "Ford", model = "Mustang", year = 1967}

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) =
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
{-
    *Main> tellCar stang
    "This Ford Mustang was made in 1967"
-}

data Car1 a b c = Car1 {
    company1 :: a,
    model1 :: b,
    year1 :: c
   }   deriving (Show)

tellCar1 :: (Show a) => Car1 String String a -> String
tellCar1 (Car1 {company1 = c, model1 = m, year1 = y}) =
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
{-
    *Main> tellCar1 (Car1 "Ford" "Mustang" 1967)
    "This Ford Mustang was made in 1967"
    *Main> tellCar1 (Car1 "Ford" "Mustang" "ninteen sixty-seven")
    "This Ford Mustang was made in \"ninteen sixty-seven\""
-}

--data (Ord k) => DM.Map k v = ...
--But you don't add type class constraint in type declaration
--if you add it, you should write type class constraint on every function

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector i j k) (Vector x y z) = Vector (i + z) (j + y) (k + z)

dotProd :: (Num a) => Vector a -> Vector a -> a
dotProd (Vector i j k) (Vector x y z) = i * x + j * y + k * z

vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector i j k) m = Vector (m*i) (j*m) (k*m)

--These three function will only work at Vector Num
--and not work at Vector Char or Bool
--also, Vector Double and Vector Int cannot be plussed: works only same type
{-
    *Main> let v1 = Vector 3 4 5
    *Main> let v2 = Vector 1 6 2
    *Main> vplus v1 v2
    Vector 5 10 7
    *Main> dotProd v1 v2
    37
    *Main> vmult v1 10
    Vector 30 40 50
-}



