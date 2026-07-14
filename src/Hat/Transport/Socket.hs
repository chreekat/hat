-- | Unix-socket location and listen/connect plumbing.
module Hat.Transport.Socket
    ( socketDir
    , socketPath
    , defaultSocketPath
    , ensureSocketDir
    , listenOn
    , connectTo
    ) where

import Control.Exception (bracketOnError, try, SomeException)
import Control.Monad (when)
import qualified Network.Socket as N
import System.Directory
    (createDirectoryIfMissing, doesPathExist, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.Posix.Files (setFileMode)
import System.Posix.Types (CUid)
import System.Posix.User (getRealUserID)

-- | Directory holding hat sockets: @$TMUX_TMPDIR/hat-$UID@, or
-- @/tmp/hat-$UID@ when the variable is unset.
socketDir :: Maybe FilePath -> CUid -> FilePath
socketDir tmpdir uid =
    maybe "/tmp" id tmpdir <> "/hat-" <> show (toInteger uid)

-- | Full path of a named socket inside 'socketDir'.
socketPath :: Maybe FilePath -> CUid -> String -> FilePath
socketPath tmpdir uid name = socketDir tmpdir uid <> "/" <> name

-- | The socket the current process should use for the given @-L@ name.
defaultSocketPath :: String -> IO FilePath
defaultSocketPath name = do
    tmpdir <- lookupEnv "TMUX_TMPDIR"
    uid <- getRealUserID
    pure (socketPath tmpdir uid name)

-- | Create the directory that will hold the socket (and the server's
-- lock and log files), mode 0700.
ensureSocketDir :: FilePath -> IO ()
ensureSocketDir path = do
    let dir = takeDirectory path
    createDirectoryIfMissing True dir
    setFileMode dir 0o700

-- | Remove any stale socket file and listen. The caller is responsible
-- for having checked that no live server owns the path.
listenOn :: FilePath -> IO N.Socket
listenOn path = do
    ensureSocketDir path
    stale <- doesPathExist path
    when stale $ removeFile path
    bracketOnError (N.socket N.AF_UNIX N.Stream 0) N.close $ \sock -> do
        N.bind sock (N.SockAddrUnix path)
        setFileMode path 0o600
        N.listen sock 16
        pure sock

-- | Nothing if there is no live server at the path.
connectTo :: FilePath -> IO (Maybe N.Socket)
connectTo path = do
    r <- try $
        bracketOnError (N.socket N.AF_UNIX N.Stream 0) N.close $ \sock -> do
            N.connect sock (N.SockAddrUnix path)
            pure sock
    pure $ case r of
        Left (_ :: SomeException) -> Nothing
        Right sock -> Just sock
