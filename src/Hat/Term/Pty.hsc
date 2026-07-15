{-# LANGUAGE CApiFFI #-}

-- | PTY allocation and child process management. One 'PtyHandle' per pane.
--
-- Reads and writes go through a 'Handle' on the master fd so they are
-- green-thread friendly; the raw fd is kept only for ioctl.
module Hat.Term.Pty
    ( Spawn (..)
    , PtyHandle
    , spawn
    , readPty
    , writePty
    , resize
    , waitExit
    , signalHangup
    , signalKill
    , closePty
    , pid
    , foregroundCommand
    , foregroundArgv
    , parseCmdline
    , getWinsize
    , setWinsize
    , sigWinch
    , ProcessStatus (..)
    ) where

#include <signal.h>
#include <sys/ioctl.h>
#include <termios.h>

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (IOException, catch, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import Foreign
import Foreign.C.String (CString, withCString)
import Foreign.C.Types
import System.IO
import System.Posix.IO (closeFd, fdToHandle)
import System.Posix.Process (ProcessStatus (..), getProcessStatus)
import System.Posix.Signals (Signal, signalProcess, sigHUP, sigKILL)
import System.Posix.Terminal (getTerminalProcessGroupID, openPseudoTerminal)
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
    , exited :: MVar (Maybe ProcessStatus)
        -- ^ filled exactly once by the reaper thread; 'Nothing' when the
        -- reap itself failed (e.g. the child was already reaped elsewhere).
    }

foreign import capi "sys/ioctl.h ioctl"
    c_ioctl :: CInt -> CULong -> Ptr () -> IO CInt

-- Forks and execs the child entirely in C (see cbits/hat_shim.c), so no
-- Haskell/RTS code runs in the child between fork and exec — the manual
-- forkProcess path did, and under heavy parallel load its post-fork safe
-- FFI could fail to create an OS thread, killing the child before exec.
foreign import ccall unsafe "hat_spawn_pty"
    c_spawn_pty
        :: CInt -> CInt -> CString -> CString
        -> Ptr CString -> Ptr CString -> IO CInt

setWinsize :: Fd -> Size -> IO ()
setWinsize (Fd fd) sz =
    allocaBytes #{size struct winsize} $ \ws -> do
        #{poke struct winsize, ws_row} ws (fromIntegral sz.rows :: CUShort)
        #{poke struct winsize, ws_col} ws (fromIntegral sz.cols :: CUShort)
        #{poke struct winsize, ws_xpixel} ws (0 :: CUShort)
        #{poke struct winsize, ws_ypixel} ws (0 :: CUShort)
        _ <- c_ioctl fd #{const TIOCSWINSZ} ws
        pure ()

-- | The unix package doesn't export SIGWINCH.
sigWinch :: Signal
sigWinch = #{const SIGWINCH}

-- | Current size of a terminal, e.g. the client's own tty. Falls back
-- to 80x24 when the fd is not a terminal.
getWinsize :: Fd -> IO Size
getWinsize (Fd fd) =
    allocaBytes #{size struct winsize} $ \ws -> do
        rc <- c_ioctl fd #{const TIOCGWINSZ} ws
        if rc /= 0
            then pure Size { rows = 24, cols = 80 }
            else do
                r <- #{peek struct winsize, ws_row} ws :: IO CUShort
                c <- #{peek struct winsize, ws_col} ws :: IO CUShort
                if r == 0 || c == 0
                    then pure Size { rows = 24, cols = 80 }
                    else pure Size
                        { rows = fromIntegral r, cols = fromIntegral c }

spawn :: Spawn -> IO PtyHandle
spawn s = do
    (masterFd, slaveFd) <- openPseudoTerminal
    setWinsize slaveFd s.size
    -- Wrap the master fd FIRST: hSetBuffering NoBuffering on a terminal
    -- handle does a hidden tcsetattr (GHC's setRaw clears ICANON, sets
    -- MIN=1/TIME=0), and a pty pair shares one termios — so this clobbers
    -- the pane's line discipline. Doing it before the spawn makes the
    -- child's own tcsetattr (hat_spawn_pty) strictly last; racing it was
    -- the source of panes coming up non-canonical under load.
    h <- fdToHandle masterFd
    hSetBuffering h NoBuffering
    let Fd m = masterFd
        Fd sl = slaveFd
        cmd = s.cmd
        argv = s.cmd : s.args
        envv = [k <> "=" <> v | (k, v) <- s.env]
    rawPid <- withCString cmd $ \cCmd ->
        withMaybeCString s.cwd $ \cCwd ->
        withCStringArray0 argv $ \pArgv ->
        withCStringArray0 envv $ \pEnv ->
            c_spawn_pty m sl cCwd cCmd pArgv pEnv
    closeFd slaveFd
    when (rawPid < 0) $ ioError (userError "hat: fork failed")
    let childPid = fromIntegral rawPid :: ProcessID
    exitVar <- newEmptyMVar
    _ <- forkIO $ do
        r <- try (getProcessStatus True False childPid)
        -- The put is unconditional: an unfillable slot would park
        -- 'waitExit' forever.
        putMVar exitVar $ case r of
            Left (_ :: IOException) -> Nothing
            Right st                -> st
    pure PtyHandle
        { master = masterFd
        , handle = h
        , child = childPid
        , exited = exitVar
        }

withMaybeCString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeCString Nothing  f = f nullPtr
withMaybeCString (Just s) f = withCString s f

-- A NULL-terminated array of C strings, e.g. an argv or envp.
withCStringArray0 :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray0 xs f =
    withMany withCString xs $ \ptrs -> withArray0 nullPtr ptrs f

-- The canonical/echo/signal line discipline is normalized in the C child
-- (hat_spawn_pty), in its own foreground context after TIOCSCTTY and
-- before exec — deterministic syscalls, no external @stty@ to lose a race.

-- | Blocking read of whatever bytes are available. Empty result means the
-- pty is gone (child exited and the slave side closed).
readPty :: PtyHandle -> IO ByteString
readPty pty = do
    r <- try (B.hGetSome pty.handle 65536)
    pure $ case r of
        Left (_ :: IOException) -> B.empty  -- Linux gives EIO at pty EOF
        Right bs -> bs

writePty :: PtyHandle -> ByteString -> IO ()
writePty pty bs =
    B.hPut pty.handle bs `catch` \(_ :: IOException) -> pure ()

-- | Change the pty size; the kernel delivers SIGWINCH to the child.
resize :: PtyHandle -> Size -> IO ()
resize pty = setWinsize pty.master

-- | Wait for the child to exit. Idempotent. 'Nothing' when the reaper
-- could not obtain a status (e.g. the child was already reaped).
waitExit :: PtyHandle -> IO (Maybe ProcessStatus)
waitExit pty = readMVar pty.exited

pid :: PtyHandle -> ProcessID
pid pty = pty.child

-- | The command of the process group that currently owns the pty's
-- terminal — i.e. the foreground program, which is a child running under
-- the shell rather than the shell itself. Prefers argv[0] (the first
-- field of @/proc/<pgrp>/cmdline@), which is what tmux reports: NixOS
-- wrappers exec the real binary as @.<name>-wrapped@ but pass the public
-- name along in argv[0]. Falls back to @/proc/<pgrp>/comm@ (the
-- executable's name, truncated to 15 bytes) when cmdline is empty.
-- 'Nothing' if the terminal has no foreground group or @/proc@ can't be
-- read (e.g. the process just exited).
foregroundCommand :: PtyHandle -> IO (Maybe Text)
foregroundCommand pty = do
    r <- try (getTerminalProcessGroupID pty.master)
    case r of
        Left (_ :: IOException) -> pure Nothing
        Right pgrp -> do
            argv0 <- procField pgrp "cmdline" (T.takeWhile (/= '\NUL'))
            case argv0 of
                Just _ -> pure argv0
                Nothing -> procField pgrp "comm" T.strip

-- | The full argument vector of the pty's foreground process group, read
-- from @\/proc\/\<pgrp\>\/cmdline@ (the kernel stores argv there, one
-- element per NUL-terminated field). Unlike 'foregroundCommand', which keeps
-- only argv[0] for display, this preserves every argument — including a
-- filename with spaces — so a restore can re-exec @vim@ on the same file.
-- 'Nothing' when there is no foreground group or @\/proc@ can't be read; the
-- shell's own argv when no child is running (callers filter that out).
foregroundArgv :: PtyHandle -> IO (Maybe [Text])
foregroundArgv pty = do
    r <- try (getTerminalProcessGroupID pty.master)
    case r of
        Left (_ :: IOException) -> pure Nothing
        Right pgrp -> do
            c <- try (B.readFile ("/proc/" <> show pgrp <> "/cmdline"))
            pure $ case c of
                Left (_ :: IOException) -> Nothing
                Right bs -> parseCmdline bs

-- | Split a @\/proc\/\<pid\>\/cmdline@ payload into argv. Fields are
-- NUL-separated (with a trailing NUL), and each is decoded leniently: a
-- filename can hold bytes that are not valid UTF-8, and the server may run
-- under a non-UTF-8 locale, so strict decoding would lose the argv entirely.
-- 'Nothing' for an empty cmdline (a kernel thread, or a zeroed argv).
parseCmdline :: ByteString -> Maybe [Text]
parseCmdline bs =
    case filter (not . B.null) (B.split 0 bs) of
        []   -> Nothing
        args -> Just (map (TE.decodeUtf8With TEE.lenientDecode) args)

-- One trimmed field of a @/proc/<pid>@ file; 'Nothing' when unreadable
-- (process gone) or empty (e.g. a cmdline the process has zeroed out).
procField :: (Show p) => p -> String -> (Text -> Text) -> IO (Maybe Text)
procField pgrp name trim = do
    c <- try (TIO.readFile ("/proc/" <> show pgrp <> "/" <> name))
    pure $ case c of
        Left (_ :: IOException) -> Nothing
        Right s -> let t = T.strip (trim s) in
            if T.null t then Nothing else Just t

-- | Send SIGHUP to the pane's child, swallowing "process gone" errors.
-- Unlike 'closePty' this does not touch the master Handle, so it is safe
-- to call while the reader thread is still blocked inside it.
signalHangup :: PtyHandle -> IO ()
signalHangup pty =
    signalProcess sigHUP pty.child `catch` \(_ :: IOException) -> pure ()

-- | Send SIGKILL to the pane's child, swallowing "process gone" errors.
-- The escalation for a child that outlives SIGHUP.
signalKill :: PtyHandle -> IO ()
signalKill pty =
    signalProcess sigKILL pty.child `catch` \(_ :: IOException) -> pure ()

-- | Hang up the pane: signal the child and close the master side.
closePty :: PtyHandle -> IO ()
closePty pty = do
    signalHangup pty
    hClose pty.handle `catch` \(_ :: IOException) -> pure ()
