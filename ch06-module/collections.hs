import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Char as DC
--associative list: dictionary: key-value pair
phoneBook = 
    [("anna", "800-1223"),
    ("betty", "838-1222"),
    ("cindy", "328-3234"),
    ("diana", "122-1215"),
    ("elsa", "801-1223"),
    ("frankie", "999-1444")]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
{-
    *Main> findKey "anna" phoneBook
    "800-1223"
    *Main> findKey "elsa" phoneBook
    "801-1223"
-}
findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs) 
    | key == k = Just v
    | otherwise = findKey' key xs
--if key is not in dictionary, findKey cause error: use Maybe
{-
    *Main> findKey' "elsa" phoneBook
    Just "801-1223"
    *Main> findKey "elphaba" phoneBook
    "*** Exception: Prelude.head: empty list
    *Main> findKey' "elphaba" phoneBook
    Nothing
-}

findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing xs
--use fold

--Data.Map provide much faster association list: O(log n) map
phoneBook1 = DM.fromList(phoneBook)
{-
    *Main> phoneBook1
    fromList [("anna","800-1223"),("betty","838-1222"),("cindy","328-3234"),
    ("diana","122-1215"),("elsa","801-1223"),("frankie","999-1444")]
-}
--if there were same keys in original list, 
--then only final value remains.
{-
    *Main> DM.fromList([("MS",1), ("MS",2), ("MS",3)])
    fromList [("MS",3)]
-}
--DM.fromList :: (Ord k) => [(k,v)] -> DM.Map k v
--key must be Ord: they can be sorted

phoneBook2 :: DM.Map String String
phoneBook2 = DM.fromList $
    [("alice", "111-2222"),
    ("brandon", "122-3333")]
{-
    *Main> phoneBook2
    fromList [("alice","111-2222"),("brandon","122-3333")]
-}
{-
Data.Map.lookup function
    *Main> :t DM.lookup
    DM.lookup :: Ord k => k -> DM.Map k a -> Maybe a
    *Main> DM.lookup "anna" phoneBook1
    Just "800-1223"
    *Main> DM.lookup "granda" phoneBook1
    Nothing
Data.Map.insert function
    *Main> DM.insert "granda" "001-2234" phoneBook1
    fromList [("anna","800-1223"),("betty","838-1222"),("cindy","328-3234"),
    ("diana","122-1215"),("elsa","801-1223"),("frankie","999-1444"),("granda","001-2234")]
others
    *Main> DM.size phoneBook1
    6
-}

string2digits :: String -> [Int]
string2digits = map DC.digitToInt . filter DC.isDigit
{-
convert to int
    *Main> string2digits "123245-233"
    [1,2,3,2,4,5,2,3,3]
convert map
    *Main> let intBook = DM.map string2digits phoneBook1
    *Main> intBook
    fromList [("anna",[8,0,0,1,2,2,3]),("betty",[8,3,8,1,2,2,2]),("cindy",[3,2,8,3,2,3,4]),
    ("diana",[1,2,2,1,2,1,5]),("elsa",[8,0,1,1,2,2,3]),("frankie",[9,9,9,1,4,4,4])]
-}

phoneBookToMap :: (Ord k) => [(k, String)] -> DM.Map k String
phoneBookToMap xs = DM.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2
--fromListWith: not remove multi value -> add up by provide function
phoneBook3 = phoneBook ++ [("anna", "020-3030"), ("elsa", "021-3030"), ("frankie", "401-2221")]
{-
    *Main> let phoneBook4 = phoneBookToMap phoneBook3
    *Main> DM.lookup "elsa" phoneBook4
    Just "021-3030, 801-1223"
-}
