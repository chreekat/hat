-- | The one place filenames are built. Every socket, store, log, and
-- user-supplied buffer path is assembled here so separator joining and
-- @~@/@~user@/@HOME@ expansion stay consistent instead of drifting between
-- ad-hoc @<> "/"@ concatenations.
module Hat.Path
    ( HatPath
    , hatPath
    , render
    , (</:>)
    , expandTildeWith
    , expandTilde
    ) where

import Control.Exception (try)
import Control.Monad (join)
import System.Environment (lookupEnv)
import System.FilePath (dropTrailingPathSeparator, normalise, (</>))
import System.Posix.User (getUserEntryForName, homeDirectory)

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

-- | Expand a leading tilde, joining with a single separator. @~@ and @~/rest@
-- resolve against the given @HOME@ (as looked up from the environment);
-- @~user@ and @~user/rest@ resolve against the named user's home, as reported
-- by the first argument (the UNIX @~user@ idiom). An unknown home, an unknown
-- user, or any other path is returned unchanged, so applying it twice is a
-- no-op.
expandTildeWith :: (String -> Maybe FilePath) -> Maybe FilePath -> FilePath -> FilePath
expandTildeWith _ (Just home) ('~' : '/' : rest) = home </> rest
expandTildeWith _ (Just home) "~" = home
expandTildeWith namedHome _ p@('~' : rest)
    | not (null user) =
        case namedHome user of
            Just home -> home </> sub
            Nothing -> p
  where
    (user, after) = break (== '/') rest
    sub = drop 1 after
expandTildeWith _ _ p = p

-- | 'expandTildeWith' against the process @HOME@ and the system user database.
-- The named user, if the path calls for one, is resolved through @getpwnam@
-- before the pure expansion runs, keeping the passwd lookup at this IO seam.
expandTilde :: FilePath -> IO FilePath
expandTilde p = do
    home <- lookupEnv "HOME"
    resolved <- maybe (pure []) (fmap (: []) . lookupUserHome) (namedUser p)
    let namedHome name = join (lookup name resolved)
    pure (expandTildeWith namedHome home p)

-- | The username a leading @~user@/@~user\/rest@ names, or @Nothing@ for a
-- bare @~@, a @~\/rest@, or any non-tilde path.
namedUser :: FilePath -> Maybe String
namedUser ('~' : rest@(c : _)) | c /= '/' = Just (takeWhile (/= '/') rest)
namedUser _ = Nothing

-- | The home directory of a named user, or @Nothing@ when the user does not
-- exist (a missing user is not an error, mirroring shells' @~user@). Paired
-- with the username so it can seed a pure resolver.
lookupUserHome :: String -> IO (String, Maybe FilePath)
lookupUserHome name = do
    result <- try (getUserEntryForName name)
    pure . (,) name $ case result of
        Left (_ :: IOError) -> Nothing
        Right entry -> Just (homeDirectory entry)
