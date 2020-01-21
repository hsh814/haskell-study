import qualified Data.Map as DM
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

--You can make Type included in multiple classes
data NewPerson = NewPerson {
    firstname :: String,
    lastname :: String,
    newAge :: Int
}   deriving (Show, Eq, Read)

gwen = NewPerson "Gwen" "Tennyson" 10
ben = NewPerson "Ben" "Tennyson" 10
max = NewPerson "Max" "Tennyson" 65
{-
Eq
    *Main> gwen == ben
    False
    *Main> gwen /= ben
    True
Show
    *Main> gwen
    NewPerson {firstname = "Gwen", lastname = "Tennyson", newAge = 10}
What's Read? String -> NewPerson

-}

francis = "NewPerson {" ++
    "firstname = \"Francis\"," ++
    "lastname = \"Foster\","++
    "newAge = 23}"
{-
    *Main> read francis :: NewPerson
    NewPerson {firstname = "Francis", lastname = "Foster", newAge = 23}
-}

--Ord class
{-
data Bool = False | True deriving (Ord)
    Prelude> True > False
    True
    Prelude> False `compare` True
    LT
Maybe: Nothing comes first
    Prelude> Nothing > Just 100
    False
    Prelude> Nothing > Just (-111)
    False
    Prelude> Just 2 `compare` Just 3
    LT
-}
