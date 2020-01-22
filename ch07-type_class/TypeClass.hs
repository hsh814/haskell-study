import qualified InductiveType as IT
--Type class is kind of interface
--examples: Eq, Ord, Show, Read...
{-
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
-}
--class: define new type class
-- a is type variable
--two functions are defined mutual recursion

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
--instance: define new type class's type instance
--minimal complete definition: == <-> /= is mutual recursion
--So, you should define one of them.
{-
    *Main> Red == Red
    True
    *Main> Red /= Red
    False
    *Main> Red == Yellow
    False
    *Main> Red `elem` [Red, Green, Yellow]
    True
-}
instance Show TrafficLight where
    show Red = "Red light! Stop!"
    show Yellow = "Yellow light! Careful."
    show Green = "Green light! Go!"
{-
    *Main> [Red, Green, Yellow]
    [Red light! Stop!,Green light! Go!,Yellow light! Careful.]
-}

--sub class
{-
Num
class (Eq a) => Num a where
    ...
-}
-- a should be a concrete type: ex) Int, Bool, Maybe Int...
{-
instance Eq Maybe where -> not valid
instance Eq (Maybe m) where -> valid
    but you should add class constraint
instance (Eq m) => Eq (Maybe m) where
-}

--haskell is strict typed.
--in if statement, you can only use bool, not int or string...
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

--What is id?: it returns same thing

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

{-
    *Main> yesno (0 :: Int)
    False
    *Main> yesno (1 :: Int)
    True
    *Main> yesno []
    False
    *Main> yesno [1,2]
    True
    *Main> yesno ""
    False
    *Main> yesno "hi!"
    True
    *Main> yesno Green
    True
    *Main> yesno Yellow
    True
    *Main> yesno Red
    False
    *Main> :t yesno
    yesno :: YesNo a => a -> Bool

-}

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf val yes no = if yesno val
                        then yes
                        else no
{-
    *Main> yesnoIf [] 1 2
    2
    *Main> yesnoIf "hi" True False
    True
-}




--Functor class: mapping ex) list
{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b
-}
--fmap accepts not concrete type, but f is type constructor
--map :: (a -> b) -> [a] -> [b]
--map is actually fmap that works only for list
{-
instance Functor [] where
    fmap = map
run:
    *Main> fmap (*2) [1..4]
    [2,4,6,8]
-}
--I didn't use [a] since it's type constructor

--Maybe is also a Functor
{-
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
-}

--Tree is Functor
instance Functor IT.Tree where
    fmap f IT.EmptyTree = IT.EmptyTree
    fmap f (IT.Node x left right) = IT.Node (f x) (fmap f left) (fmap f right)
{-
    *Main> fmap (*2) IT.EmptyTree
    EmptyTree
    *Main> fmap (*4) (foldr IT.treeInsert IT.EmptyTree [5,7,2])
    Node 8 EmptyTree (Node 28 (Node 20 EmptyTree EmptyTree) EmptyTree)
-}





