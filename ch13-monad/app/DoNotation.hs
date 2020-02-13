
foo0 :: Maybe String
foo0 = 
    Just 3 >>= (\x ->
    Just "!" >>= (\y ->
        Just (show x ++ y)))

foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

overEight :: Maybe Bool
overEight = do
    x <- Just 9
    Just (x > 8)

justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

gonnaFail :: Maybe Char
gonnaFail = do
    (x:xs) <- Just ""
    return x