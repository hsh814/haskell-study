import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

joinedMaybes :: Maybe Int
joinedMaybes = do
    m <- Just (Just 8)
    m