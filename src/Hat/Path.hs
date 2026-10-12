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
    , findHome
    , fitsSocketAddr
    ) where

import Control.Exception (try)
import Control.Monad (join)
import System.Environment (lookupEnv)
import System.FilePath (dropTrailingPathSeparator, normalise, (</>))
import System.Posix.User
    (getRealUserID, getUserEntryForID, getUserEntryForName, homeDirectory)

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

-- | 'expandTildeWith' against the given home and the system user database.
-- The named user, if the path calls for one, is resolved through @getpwnam@
-- before the pure expansion runs, keeping the passwd lookup at this IO seam.
expandTilde :: Maybe FilePath -> FilePath -> IO FilePath
expandTilde home p = do
    resolved <- maybe (pure []) (fmap (: []) . lookupUserHome) (namedUser p)
    let namedHome name = join (lookup name resolved)
    pure (expandTildeWith namedHome home p)

-- | tmux's @find_home@: a non-empty @HOME@, else the real user's home from
-- the user database.
findHome :: IO (Maybe FilePath)
findHome = lookupEnv "HOME" >>= \case
    Just h | not (null h) -> pure (Just h)
    _ -> either (const Nothing :: IOError -> Maybe FilePath) (Just . homeDirectory)
        <$> try (getUserEntryForID =<< getRealUserID)

-- | Whether a path fits a @sockaddr_un@: @sun_path@ is 108 bytes, terminator
-- included. Counts the bytes GHC's file-system encoding gives the path (UTF-8,
-- each lone-surrogate escape standing for the one byte it round-trips).
fitsSocketAddr :: FilePath -> Bool
fitsSocketAddr p = sum (map encodedBytes p) < 108
  where
    encodedBytes c
        | c >= '\xdc80' && c <= '\xdcff' = 1
        | c < '\x80' = 1
        | c < '\x800' = 2
        | c < '\x10000' = 3
        | otherwise = 4 :: Int

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
