import Control.Applicative

sequenceAA :: (Applicative f) => [f a] -> f [a]
sequenceAA [] = pure []
sequenceAA (x:xs) = (:) <$> x <*> sequenceAA xs