--One way to create Type is using keyword data
data TrueorFalse = True | False
--Type name should start with capital

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving (Show)
--Circle Float Float Float is generator
{-
    *Main> :t Circle
    Circle :: Float -> Float -> Float -> Shape
    *Main> :t Rectangle
    Rectangle :: Float -> Float -> Float -> Float -> Shape
-}
area :: Shape -> Float
area (Circle _ _ r) = pi * (r ^ 2)
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
--You cannot write Circle -> Float since Circle is not Type
--Also, you can use pattern matching as generator
{-
    *Main> area $ Circle 10 20 10
    314.15927
-}

--generator is function
{-
    *Main> map (Circle 10 20) [2..5]
    [Circle 10.0 20.0 2.0,Circle 10.0 20.0 3.0,Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0]
-}

data Point = Point Float Float deriving (Show)
--Generator has same name: only one generator
data Shape1 = Circle1 Point Float | Rectangle1 Point Point deriving (Show)

area1 :: Shape1 -> Float
area1 (Circle1 _ r) = pi * r * r
area1 (Rectangle1 (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

{-
    *Main> area1 (Rectangle1 (Point 0 0) (Point 100 100))
    10000.0
-}

move :: Shape1 -> Float -> Float -> Shape1
move (Circle1 (Point x y) r) a b = Circle1 (Point (x + a) (y + b)) r
move (Rectangle1 (Point x1 y1) (Point x2 y2)) a b = Rectangle1 (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
{-
    *Main> move (Circle1 (Point 34 34) 10) 3 4
    Circle1 (Point 37.0 38.0) 10.0
-}


{-
export custom data as module:

module DataType
( Point (..)
, Shape1 (..)
, area1
, move
) where    

(..) makes every generator included.
-}




