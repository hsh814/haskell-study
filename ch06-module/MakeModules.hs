--You can reuse functions and Types of your own by making modules
--module name should be same as file name
module MakeModules
( haskell
, python
, csharp
, cpp
, c
) where 

haskell :: String
haskell = "functional"

python :: Num -> Num -> Num
python n1 n2 = n1 + n2

csharp :: Double -> Double
csharp radius = 4 * pi * (radius ^ 2)

cpp :: Float -> Float -> Float -> Float
cpp f1 f2 f3 = f1 * f2 * f3

c :: Num -> Num
c a = a ^ 2

--You can use this module in other file(in same directory) as
--import MakeModules