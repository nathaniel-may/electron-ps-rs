
module Scripts.Class where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import System.Console.ANSI (Color)


class Monad m => ShellCommands m where
    cargo :: [Text] -> m ()
    npm :: [Text] -> m ()
    rm :: [Text] -> m ()

class MonadIO m => Std m where
    print :: Text -> m ()
    printWithBoldColor :: Color -> Text -> m ()

class MonadIO m => Concurrent m where
    mapConcurrently :: Traversable t => (a -> m b) -> t a -> m (t b)
