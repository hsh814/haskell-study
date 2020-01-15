--Important!
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list!"
maximum' [x] = x
--base case
maximum' (x:xs) = max x (maximum' xs)
--recursive




