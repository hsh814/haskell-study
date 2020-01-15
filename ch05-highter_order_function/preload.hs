--You can use another function as parameter
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
