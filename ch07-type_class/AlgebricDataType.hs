import qualified Data.Map as DM
--Algebraic data type
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)
--Day does not have any field: Enum type class
{-
    *Main> Monday
    Monday
    *Main> show Wednesday
    "Wednesday"
    *Main> read "Sunday" :: Day
    Sunday
    *Main> Friday == Thursday
    False
    *Main> Monday < Tuesday
    True
Bound
    *Main> minBound :: Day
    Monday
    *Main> maxBound :: Day
    Sunday
Enum: succ, pred
    *Main> succ Monday
    Tuesday
    *Main> pred Friday
    Thursday
-}

--Type synonym: String == [Char]
--type String = [Char]
--type keyword does not generate new type: just synonym

type Name = String
type PhoneNum = String
type PhoneBook = [(Name, PhoneNum)]
--phoneBook :: [(String, String)]
phoneBook :: PhoneBook
phoneBook = 
    [("anna", "800-1223"),
    ("betty", "838-1222"),
    ("cassandra", "328-3234"),
    ("diana", "122-1215"),
    ("elsa", "801-1223"),
    ("frankie", "999-1444")]

inPhoneBook :: Name -> PhoneNum -> PhoneBook -> Bool
inPhoneBook name pnum pbook = (name, pnum) `elem` pbook
{-
    *Main> inPhoneBook "anna" "800-1223" phoneBook
    True
-}

--parameterize type synonym
type AssocList k v = [(k, v)]
type IntMap v = DM.Map Int v

--data Either a b = Left a | Right b deriving (Show, Eq, Ord, Read)
{-
    *Main> Main.Right 20
    Right 20
    *Main> Main.Left "hi"
    Left "hi"
In this case, a remains polymorphic
    *Main> :t Main.Right 'a'
    Main.Right 'a' :: Main.Either a Char
    *Main> :t Main.Left "hi"
    Main.Left "hi" :: Main.Either [Char] b
-}
--You can use Either instead of Maybe

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = DM.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case DM.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = DM.fromList [
    (100, (Taken, "Z1222")),
    (122, (Free, "1222Z")),
    (125, (Free, "X1123")),
    (128, (Taken, "Y1999"))]
{-
    *Main> lockerLookup 101 lockers
    Left "Locker 101 doesn't exist!"
    *Main> lockerLookup 100 lockers
    Left "Locker 100 is already taken!"
    *Main> lockerLookup 125 lockers
    Right "X1123"
-}