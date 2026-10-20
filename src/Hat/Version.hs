-- | Hat's own version, as the package declares it.
module Hat.Version
    ( versionText
    ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)

import Paths_hat (version)

-- | The package version in its dotted spelling: what @hat -V@ and
-- @#{version}@ print.
versionText :: Text
versionText = T.pack (showVersion version)
