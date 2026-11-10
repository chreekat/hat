-- | Choosing the shell a pane runs.
module Hat.Server.Shell
    ( baseName
    , suitableShell
    ) where

import Control.Exception (IOException, catch)
import System.Environment (getProgName)
import System.Posix.Files (fileAccess)

-- | A path's last component, or the whole path when that is empty (how tmux
-- names a shell).
baseName :: FilePath -> String
baseName path = case Prelude.reverse (takeWhile (/= '/') (Prelude.reverse path)) of
    "" -> path
    base -> base

-- | tmux's @checkshell@: an absolute path to an executable that is not hat
-- itself.
suitableShell :: FilePath -> IO Bool
suitableShell path = case path of
    '/' : _ -> do
        self <- getProgName
        if baseName path == self then pure False
            else fileAccess path False False True
                `catch` \(_ :: IOException) -> pure False
    _ -> pure False
