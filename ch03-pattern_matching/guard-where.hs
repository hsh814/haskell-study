--Use guard to check true or false: like if statement
tellBMI :: Double -> String
tellBMI bmi
    | bmi <= 18.5 = "Underweight!"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Fat!"
    | otherwise = "Not a human..."
--if statement after pipeline is True: return that
--else: get down
--guard requires indentation
{-
    *Main> tellBMI 18.0
    "Underweight!"
    *Main> tellBMI 20.0
    "Normal"
    *Main> tellBMI 38.9
    "Not a human..."
-}

tellBMI1 :: Double -> Double -> String
tellBMI1 weight height
    | bmi <= 18.5 = "Underweight!"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Fat!"
    | otherwise = "Not a human..."
    where bmi = weight / (height ^ 2)
--where: to avoid computing same value multiple times
{-
    *Main> tellBMI' 55 1.7
    "Normal"
-}

tellBMI2 :: Double -> Double -> String
tellBMI2 weight height
    | bmi <= skinny = "Underweight!"
    | bmi <= normal = "Normal"
    | bmi <= fat = "Fat!"
    | otherwise = "Not a human..."
    where 
        bmi = weight / (height ^ 2)
        skinny = 18.5
        normal = 25.0
        fat = 30.0
--You can define multiple variable with where binding
--all variables should be in same column
--variables can be read anywhere in same scope
{-
    *Main> tellBMI2 100 1.8
    "Not a human..."
-}

badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

niceGreeting :: String
niceGreeting = "Hello! Nice to see you, "

greet :: String -> String
greet "Anna" = niceGreeting ++ "Anna!"
greet "Beatrice" = niceGreeting ++ "Beatrice!"
greet name = badGreeting ++ " " ++ name
--since where binding cannot be shared by other pattern, 
--you should define nice and bad greeting as global.

