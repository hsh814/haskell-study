import Data.List (break)

type Name = String
type Data = String
data FS = File Name Data | Dir Name [FS] 
    deriving (Show)

data FSCrumb = FSCrumb Name [FS] [FS] deriving (Show)

type FSZipper = (FS, [FSCrumb]) 

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Dir name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Dir dirName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb dirName ls rs:bs)

nameIs :: Name -> FS -> Bool
nameIs name (Dir dirName _) = name == dirName
nameIs name (File fileName _) = name == fileName


fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Dir name items, bs) = (Dir newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)



myDisk :: FS
myDisk =
    Dir "root"
    [
        File "goat.txt" "baaaaaaa",
        File "pony.txt" "neigh",
        Dir "name"
        [
            File "anna" "Annna",
            File "elsa" "Elssa"
        ],
        Dir "program"
        [
            File "bash" "hsab",
            File "ls" "sl"
        ]
    ]



