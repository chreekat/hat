-- | The one place filenames are built. Every socket, store, log, and
-- user-supplied buffer path is assembled here so separator joining and
-- @~@/@HOME@ expansion stay consistent instead of drifting between
-- ad-hoc @<> "/"@ concatenations.
module Hat.Path
    ( HatPath
    , hatPath
    , render
    , (</:>)
    , expandTildeWith
    , expandTilde
    ) where

import System.Environment (lookupEnv)
import System.FilePath (dropTrailingPathSeparator, normalise, (</>))

-- | A filesystem path built through the smart constructor. See 'hatPath'.
newtype HatPath = HatPath FilePath
    deriving (Eq, Ord, Show)

-- | Build a path, collapsing redundant separators and trailing slashes so
-- two spellings of the same location compare equal.
hatPath :: FilePath -> HatPath
hatPath = HatPath . dropTrailingPathSeparator . normalise

-- | The underlying filesystem path.
render :: HatPath -> FilePath
render (HatPath p) = p

-- | Append a component with a single separator, whatever the base's
-- trailing slash.
infixl 5 </:>
(</:>) :: HatPath -> FilePath -> HatPath
HatPath base </:> comp = hatPath (base </> comp)

-- | Expand a leading @~/@ against the given @HOME@ (as looked up from the
-- environment), joining with a single separator. A bare @~@, an unknown
-- home, or any other path is returned unchanged, so applying it twice is a
-- no-op.
expandTildeWith :: Maybe FilePath -> FilePath -> FilePath
expandTildeWith (Just home) ('~' : '/' : rest) = home </> rest
expandTildeWith _ p = p

-- | 'expandTildeWith' against the process @HOME@.
expandTilde :: FilePath -> IO FilePath
expandTilde p = do
    home <- lookupEnv "HOME"
    pure (expandTildeWith home p)
