-- | The server lock: one server per socket path, held for the process's life,
-- in-place reloads included.
--
-- Note [The server lock]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- The lock is a POSIX (fcntl) record lock on @<sock>.lock@. Such a lock
-- belongs to the process, not to an fd: it survives execve, a second lock
-- request from the same process succeeds instead of conflicting, and closing
-- ANY fd the process has on the file drops it. So the server never opens the
-- lock file a second time, and a reload carries every fd it has on the file
-- across the exec ('lockFds'), where the resumed image re-asserts the lock
-- through the inherited one ('reclaimLock') rather than opening a fresh one.
module Hat.Server.Lock
    ( LockResult (..)
    , awaitLock
    , reclaimLock
    , lockFds
    , lockWaitAttempts
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch, try)
import Control.Monad (filterM)
import Data.Maybe (mapMaybe)
import System.Directory (canonicalizePath, doesPathExist, listDirectory)
import System.IO (SeekMode (AbsoluteSeek))
import System.Posix.Files (readSymbolicLink)
import System.Posix.IO qualified as PIO
import System.Posix.Types (Fd (..))
import Text.Read (readMaybe)

-- | The outcome of racing for the server's lock file.
data LockResult
    = LockWon            -- ^ this process now holds the lock
    | LockHeldElsewhere  -- ^ another server already holds it

lockPathFor :: FilePath -> FilePath
lockPathFor sock = sock <> ".lock"

-- | Take the server lock for the socket at @path@. A holder with the path
-- bound is serving (or about to), so it wins outright; one without it is
-- starting up or retired and draining, so poll until it binds the path or
-- exits. Gives up (as held) after @attempts@ polls.
-- See Note [Retiring the socket].
awaitLock :: FilePath -> Int -> IO LockResult
awaitLock path attempts = do
    r <- acquireLock (lockPathFor path)
    bound <- doesPathExist path
    case r of
        LockWon -> pure LockWon
        LockHeldElsewhere
            | bound || attempts <= 1 -> pure LockHeldElsewhere
            | otherwise -> do
                threadDelay lockPollMicros
                awaitLock path (attempts - 1)

lockPollMicros :: Int
lockPollMicros = 10_000

-- | 10s of polls: past any drain, which 'reapPane' bounds.
lockWaitAttempts :: Int
lockWaitAttempts = 1_000

-- O_CREAT, then the lock, held through the fd for the server's life.
-- See Note [The server lock].
acquireLock :: FilePath -> IO LockResult
acquireLock lockPath = do
    fd <- PIO.createFile lockPath 0o600
    r <- lockThrough fd
    case r of
        LockHeldElsewhere -> r <$ PIO.closeFd fd
        LockWon -> pure r

-- | Take the lock for the socket at @path@ in an image a reload resumed,
-- through the fd the outgoing image carried across the exec; a predecessor
-- that carried none is locked afresh. See Note [The server lock].
reclaimLock :: FilePath -> IO LockResult
reclaimLock path = do
    inherited <- lockFds path
    case inherited of
        fd : _ -> lockThrough (Fd (fromIntegral fd))
        [] -> acquireLock (lockPathFor path)

lockThrough :: Fd -> IO LockResult
lockThrough fd = do
    r <- try $ PIO.setLock fd (PIO.WriteLock, AbsoluteSeek, 0, 0)
    pure $ case r of
        Left (_ :: IOException) -> LockHeldElsewhere
        Right () -> LockWon

-- | The fds this process has open on the lock file of the socket at @path@.
lockFds :: FilePath -> IO [Int]
lockFds path = (do
    target <- canonicalizePath (lockPathFor path)
    fds <- mapMaybe readMaybe <$> listDirectory "/proc/self/fd"
    filterM (fmap (== target) . link) fds)
    `catch` \(_ :: IOException) -> pure []
  where
    -- The listing's own directory fd is stale by now, so its link fails.
    link :: Int -> IO FilePath
    link fd = readSymbolicLink ("/proc/self/fd/" <> show fd)
        `catch` \(_ :: IOException) -> pure ""
