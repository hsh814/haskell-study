x -: f = f x

type Birds = Int
type Stick = (Birds, Birds)

landleft :: Birds -> Stick -> Maybe Stick
landleft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landright :: Birds -> Stick -> Maybe Stick
landright n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise = Nothing

banana :: Stick -> Maybe Stick
banana _ = Nothing