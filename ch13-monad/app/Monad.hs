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


routine :: Maybe Stick
routine = do
    start <- return (0,0)
    first <- landleft 2 start
    second <- landright 2 first
    landleft 1 second

routine1 :: Maybe Stick
routine1 = 
    case Just (0,0) of
        Nothing -> Nothing
        Just start -> case landleft 2 start of
            Nothing -> Nothing
            Just first -> case landright 2 first of
                Nothing -> Nothing
                Just second -> landleft 1 second