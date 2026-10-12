-- | Unix-socket location and listen/connect plumbing.
module Hat.Transport.Socket
    ( socketDir
    , socketPath
    , defaultSocketPath
    , ensureSocketDir
    , listenOn
    , ConnectFailure (..)
    , connectTo
    ) where

import Control.Exception (bracketOnError, try)
import Control.Monad (when)
import Foreign.C.Error (Errno (..), eCONNREFUSED, eNAMETOOLONG, eNOENT, errnoToIOError)
import GHC.IO.Exception (IOException (..))
import Network.Socket qualified as N
import System.Directory
    (createDirectoryIfMissing, doesPathExist, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.Posix.Files (setFileMode)
import System.Posix.Types (CUid)
import System.Posix.User (getRealUserID)

import Hat.Path (fitsSocketAddr, render, hatPath, (</:>))

-- | Directory holding hat sockets: @$TMUX_TMPDIR/hat-$UID@, or
-- @/tmp/hat-$UID@ when the variable is unset.
socketDir :: Maybe FilePath -> CUid -> FilePath
socketDir tmpdir uid =
    render (hatPath (maybe "/tmp" id tmpdir) </:> ("hat-" <> show (toInteger uid)))

-- | Full path of a named socket inside 'socketDir'.
socketPath :: Maybe FilePath -> CUid -> String -> FilePath
socketPath tmpdir uid name = render (hatPath (socketDir tmpdir uid) </:> name)

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

-- | Why 'connectTo' found no server to talk to.
data ConnectFailure
    = NoServer              -- ^ nothing listens at the path
    | ConnectFailed String  -- ^ the path cannot be reached at all; the @strerror@ text
    deriving (Eq, Show)

-- | Connect to the server at the path. Only a refused or missing socket is
-- 'NoServer' (what a client may autostart past), as in tmux's
-- @client_connect@; a path longer than an address holds fails before trying.
connectTo :: FilePath -> IO (Either ConnectFailure N.Socket)
connectTo path
    | not (fitsSocketAddr path) = pure (Left (ConnectFailed (strerror eNAMETOOLONG)))
    | otherwise = do
        r <- try $
            bracketOnError (N.socket N.AF_UNIX N.Stream 0) N.close $ \sock -> do
                N.connect sock (N.SockAddrUnix path)
                pure sock
        pure $ case r of
            Left (e :: IOException)
                | fmap Errno e.ioe_errno `elem` map Just [eCONNREFUSED, eNOENT] ->
                    Left NoServer
                | otherwise -> Left (ConnectFailed e.ioe_description)
            Right sock -> Right sock
  where
    strerror errno = (errnoToIOError "" errno Nothing Nothing).ioe_description
