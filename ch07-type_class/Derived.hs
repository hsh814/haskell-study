--We used deriving before.
data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int
}   deriving (Show)
{-
    *Main> Person "Frankie" "Foster" 23
    Person {firstName = "Frankie", lastName = "Foster", age = 23}
-}
--You can make type Person in Eq class

data EPerson = EPerson {
    efirstName :: String,
    elastName :: String,
    eage :: Int
}   deriving (Eq)
--if you compare two EPerson with == or /=
--haskell will compare every field with ==
--But in one condition: all field must be Eq
frankie = EPerson "Frankie" "Foster" 23
mac = EPerson "Mac" "Kazoo" 8
bloo = EPerson "Bloo" "Kazoo" 4
{-
    *Main> frankie == frankie
    True
    *Main> frankie == mac
    False
    *Main> frankie /= bloo
    True
You can use elem function
    *Main> let people = [frankie, mac, bloo]
    *Main> frankie `elem` people
    True

-}












