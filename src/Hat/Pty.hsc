{-# LANGUAGE CApiFFI #-}

-- | PTY allocation and child process management. One 'PtyHandle' per pane.
--
-- Reads and writes go through a 'Handle' on the master fd so they are
-- green-thread friendly; the raw fd is kept only for ioctl.
module Hat.Pty
    ( Spawn (..)
    , PtyHandle
    , spawn
    , readPty
    , writePty
    , resize
    , waitExit
    , closePty
    , pid
    , ProcessStatus (..)
    ) where

#include <sys/ioctl.h>
#include <termios.h>

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, catch, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (for_)
import Foreign
import Foreign.C.Types
import System.Exit (ExitCode (..))
import System.IO
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.IO
    (closeFd, dupTo, fdToHandle, stdError, stdInput, stdOutput)
import System.Posix.Process
    (ProcessStatus (..), createSession, executeFile, exitImmediately,
     forkProcess, getProcessStatus)
import System.Posix.Signals (signalProcess, sigHUP)
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types (Fd (..), ProcessID)

import Hat.Geometry

data Spawn = Spawn
    { cmd  :: FilePath
    , args :: [String]
    , env  :: [(String, String)]
    , cwd  :: Maybe FilePath
    , size :: Size
    }

data PtyHandle = PtyHandle
    { master :: Fd
    , handle :: Handle
    , child  :: ProcessID
    , exited :: MVar ProcessStatus
    }

foreign import capi "sys/ioctl.h ioctl"
    c_ioctl :: CInt -> CULong -> Ptr () -> IO CInt

setWinsize :: Fd -> Size -> IO ()
setWinsize (Fd fd) sz =
    allocaBytes #{size struct winsize} $ \ws -> do
        #{poke struct winsize, ws_row} ws (fromIntegral sz.rows :: CUShort)
        #{poke struct winsize, ws_col} ws (fromIntegral sz.cols :: CUShort)
        #{poke struct winsize, ws_xpixel} ws (0 :: CUShort)
        #{poke struct winsize, ws_ypixel} ws (0 :: CUShort)
        _ <- c_ioctl fd #{const TIOCSWINSZ} ws
        pure ()

spawn :: Spawn -> IO PtyHandle
spawn s = do
    (masterFd, slaveFd) <- openPseudoTerminal
    setWinsize slaveFd s.size
    childPid <- forkProcess $ do
        closeFd masterFd
        _ <- createSession
        _ <- c_ioctl (case slaveFd of Fd fd -> fd) #{const TIOCSCTTY} nullPtr
        _ <- dupTo slaveFd stdInput
        _ <- dupTo slaveFd stdOutput
        _ <- dupTo slaveFd stdError
        when (slaveFd > Fd 2) $ closeFd slaveFd
        for_ s.cwd $ \dir ->
            changeDirLenient dir
        executeFile s.cmd True s.args (Just s.env)
            `catch` \(_ :: SomeException) -> exitImmediately (ExitFailure 127)
    closeFd slaveFd
    h <- fdToHandle masterFd
    hSetBuffering h NoBuffering
    exitVar <- newEmptyMVar
    _ <- forkIO $ do
        st <- getProcessStatus True False childPid
            `catch` \(_ :: SomeException) -> pure Nothing
        for_ st (putMVar exitVar)
    pure PtyHandle
        { master = masterFd
        , handle = h
        , child = childPid
        , exited = exitVar
        }
  where
    changeDirLenient dir =
        changeWorkingDirectory dir
            `catch` \(_ :: SomeException) -> pure ()

-- | Blocking read of whatever bytes are available. Empty result means the
-- pty is gone (child exited and the slave side closed).
readPty :: PtyHandle -> IO ByteString
readPty pty = do
    r <- try (B.hGetSome pty.handle 65536)
    pure $ case r of
        Left (_ :: SomeException) -> B.empty  -- Linux gives EIO at pty EOF
        Right bs -> bs

writePty :: PtyHandle -> ByteString -> IO ()
writePty pty bs =
    B.hPut pty.handle bs `catch` \(_ :: SomeException) -> pure ()

-- | Change the pty size; the kernel delivers SIGWINCH to the child.
resize :: PtyHandle -> Size -> IO ()
resize pty = setWinsize pty.master

-- | Wait for the child to exit. Idempotent.
waitExit :: PtyHandle -> IO ProcessStatus
waitExit pty = readMVar pty.exited

pid :: PtyHandle -> ProcessID
pid pty = pty.child

-- | Hang up the pane: signal the child and close the master side.
closePty :: PtyHandle -> IO ()
closePty pty = do
    signalProcess sigHUP pty.child `catch` \(_ :: SomeException) -> pure ()
    hClose pty.handle `catch` \(_ :: SomeException) -> pure ()
