--let is similar to where
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea
--in part became value
--it can be used in local
--You can not use this in guard because of scope
{-
    *Main> [let square x = x * x in (square 5, square 3, square 2)]
    [(25,9,4)]
You can use ; to separate
    *Main> (let a = 100; b = 200; c = 300 in a * b * c, let foo="Hi"; bar="There" in foo ++ bar)
    (6000000,"HiThere")
It can be used to unboxing tuple
    *Main> (let (a, b, c) = (1, 2, 3) in a+b+c) * 100
    600
-}

calBmis :: [(Double, Double)] -> [Double]
calBmis xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2]
--list comprehension
calBmis' :: [(Double, Double)] -> [Double]
calBmis' xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]
--Only fat people would be in list
--(w,h) <- xs is generator

--case expression: just like case in c
--syntactic sugar
head' :: [a] -> a
head' [] = error "Empty list!"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of 
    [] -> error "Empty list!"
    (x:_) -> x
--both are same
