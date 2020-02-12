myJust x = Just (x + 1)
positiveJust x = if x > 0 then Just x else Nothing

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

