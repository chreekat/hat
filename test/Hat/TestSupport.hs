-- Helpers shared by more than one spec.
module Hat.TestSupport (bashPath) where

import Data.Maybe (fromMaybe)
import System.Directory (findExecutable)

-- The dev shell's bash, for tests that need a bashism (@exec -a@). Not
-- /bin/sh: that is bash only on NixOS, and dash elsewhere.
bashPath :: IO FilePath
bashPath = fromMaybe (error "bash is not on PATH") <$> findExecutable "bash"
