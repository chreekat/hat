-- | The ghc-debug socket the server listens on so ghc-debug-brick can attach
-- to it. The stub's own default names the socket after the pid, which an
-- in-place reload keeps; hat pins the name to its server socket instead, and
-- sweeps the sockets an earlier image left open (see 'prepareDebugSocket').
module Hat.Debug
    ( debugSocketDir
    , debugSocketPath
    , fitsSocketAddr
    , prepareDebugSocket
    , boundSocketInodes
    , fdSocketInode
    ) where

import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless)
import Data.Char (isDigit)
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import System.Directory (XdgDirectory (..), getXdgDirectory, listDirectory)
import System.Environment (setEnv)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hPutStrLn, readFile', stderr)
import System.Posix.Files (readSymbolicLink)
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd (..))
import Text.Read (readMaybe)

import Hat.Path (hatPath, render, (</:>))

-- | The directory ghc-debug-stub creates its sockets in and ghc-debug-brick
-- scans for debuggees, under the given XDG data directory. Mirrors the stub's
-- @socketDirectory@: a socket outside it is invisible to brick.
debugSocketDir :: FilePath -> FilePath
debugSocketDir xdgData =
    render (hatPath xdgData </:> "ghc-debug" </:> "debuggee" </:> "sockets")

-- | The debug socket for a server listening on the given socket path, named
-- after it (@hat-default@) so it is stable across reloads and restarts and
-- distinct per @-L@ name.
debugSocketPath :: FilePath -> FilePath -> FilePath
debugSocketPath xdgData sock =
    render (hatPath (debugSocketDir xdgData) </:> ("hat-" <> name))
  where
    name = takeFileName (render (hatPath sock))

-- | Whether a path fits a @sockaddr_un@ (@sun_path@ is 108 bytes including the
-- terminator). ghc-debug-stub aborts the process on a longer one, so the caller
-- must check before handing it over.
fitsSocketAddr :: FilePath -> Bool
fitsSocketAddr p = length p < 108

-- | Point this process's debug socket at 'debugSocketPath' and close any debug
-- socket inherited from an earlier image, returning the pinned path — or
-- 'Nothing', for a home directory deep enough that the path would not fit an
-- address, where the caller must skip the debuggee rather than let the stub
-- abort the server. Call before the stub opens its socket, and in every server
-- whether or not the shim is linked: a build without it still inherits what a
-- reload handed over.
prepareDebugSocket :: FilePath -> IO (Maybe FilePath)
prepareDebugSocket sock = do
    xdgData <- getXdgDirectory XdgData ""
    let path = debugSocketPath xdgData sock
    closeSocketsUnder (debugSocketDir xdgData)
    if fitsSocketAddr path
        then Just path <$ setEnv "GHC_DEBUG_SOCKET" path
        else Nothing <$ hPutStrLn stderr
            ("hat: no ghc-debug socket, path too long: " <> path)

-- Close every fd in this process bound to a unix socket under the directory.
-- An execve carries the fds through but drops the Haskell cleanup that would
-- have closed them, so an image may inherit several generations at once.
closeSocketsUnder :: FilePath -> IO ()
closeSocketsUnder dir = do
    table <- readFile' "/proc/net/unix" `catch` \(_ :: IOException) -> pure ""
    let inodes = boundSocketInodes dir table
    unless (null inodes) $ do
        fds <- listDirectory "/proc/self/fd"
            `catch` \(_ :: IOException) -> pure []
        forM_ fds $ \entry -> do
            link <- readSymbolicLink ("/proc/self/fd" </> entry)
                `catch` \(_ :: IOException) -> pure ""
            case (fdSocketInode link, readMaybe entry) of
                (Just inode, Just fd) | inode `elem` inodes ->
                    closeFd (Fd fd) `catch` \(_ :: IOException) -> pure ()
                _ -> pure ()

-- | The inodes of the unix sockets bound to a path in the directory, read from
-- a @\/proc\/net\/unix@ table. A socket keeps its name there after the file is
-- unlinked, so a path rebound by a later image still yields every generation.
boundSocketInodes :: FilePath -> String -> [Int]
boundSocketInodes dir = mapMaybe entry . lines
  where
    want = render (hatPath dir)
    -- Columns: Num RefCount Protocol Flags Type St Inode [Path]. The path runs
    -- to the end of the line, spaces and all.
    entry ln = case drop 6 (words ln) of
        (inode : rest@(_ : _))
            | render (hatPath (takeDirectory (unwords rest))) == want ->
                readMaybe inode
        _ -> Nothing

-- | The inode a @\/proc\/PID\/fd@ symlink names, when it points at a socket.
fdSocketInode :: FilePath -> Maybe Int
fdSocketInode link = do
    rest <- stripPrefix "socket:[" link
    case span isDigit rest of
        (digits, "]") -> readMaybe digits
        _ -> Nothing
