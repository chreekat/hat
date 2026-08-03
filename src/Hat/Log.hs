{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- The event fields name the JSON keys (via Generic ToJSON) and are only
-- ever set by record construction, never read as selectors — so their
-- partiality across constructors is intentional and safe here.
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Structured JSON logging: one event per line, written by a
-- dedicated thread so callers never block on disk.
--
-- This is the seam the architecture doc calls for; the writer behind
-- it (hand-rolled aeson + TQueue + handle) can be swapped without
-- touching any caller.
module Hat.Log
    ( Logger
    , LogEvent (..)
    , newLogger
    , logEvent
    , flushLogger
    , closeLogger
    , withLogger
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Concurrent.STM
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import GHC.Generics (Generic)
import System.IO

data LogEvent
    = ServerStarted   { socket :: FilePath }
    | ServerStopping  { reason :: Text }
    | ServerReloading { target :: FilePath }  -- ^ the binary an in-place reload re-execs
    | ReloadAdopt     { pane :: Int, phase :: Text }  -- ^ a reload's per-pane adopt progress (start\/replaying\/ready), to pinpoint a resume that stalls
    | ClientConnected { client :: Int, term :: Text }
    | ClientDetached  { client :: Int, reason :: Text }
    | PaneSpawned     { pane :: Int, cmd :: Text }
    | PaneExited      { pane :: Int }
    | PaneResizeFailed { pane :: Int, err :: Text }
    | PaneResizing    { pane :: Int, toRows :: Int, toCols :: Int }  -- ^ written (and flushed) just before the emulator resize call, so a native crash there still leaves the culprit pane and dimensions in the log
    | DaemonFault     { daemon :: Text, err :: Text }  -- ^ a background loop hit an expected fault; logged, non-fatal
    | DaemonStopped   { daemon :: Text, reason :: Text }  -- ^ a background loop gave up for good (e.g. a missing dependency); logged, non-fatal
    | CommandRun      { client :: Int, command :: Text }
    | ConfigError     { file :: FilePath, err :: Text }
    | ProtocolError   { client :: Int, err :: Text }
    | UnknownTermProp { pane :: Int, propKind :: Text, prop :: Int }  -- ^ the emulator reported a terminal property hat does not handle
    | UnhandledPassthrough { pane :: Int, payload :: Text }  -- ^ a tmux passthrough payload hat neither answers nor forwards (OSC 52\/12\/4, …), logged instead of silently dropped
    | ServerCrash     { err :: Text }
    | ServerFatal     { err :: Text }  -- ^ an exception escaped the server's main loop and is taking the process down; carries its 'displayException' (backtrace included)
    deriving (Show, Generic)
    deriving anyclass (ToJSON)

-- A message to the writer thread: a line to append, or a barrier (flush or
-- close) carrying the MVar to signal once the writer has drained everything
-- ahead of it. Barriers ride the same FIFO queue as lines, so they observe
-- every prior line.
data LogMsg
    = LogLine Aeson.Value
    | LogFlush (MVar ())
    | LogClose (MVar ())

newtype Logger = Logger (TQueue LogMsg)

newLogger :: FilePath -> IO Logger
newLogger path = do
    h <- openFile path AppendMode
    hSetBuffering h LineBuffering
    q <- newTQueueIO
    _ <- forkIO (writeLoop h q)
    pure (Logger q)
  where
    writeLoop h q = do
        msg <- atomically (readTQueue q)
        case msg of
            LogLine v   -> BL8.hPutStrLn h (Aeson.encode v) >> writeLoop h q
            LogFlush d  -> hFlush h >> putMVar d () >> writeLoop h q
            LogClose d  -> hClose h >> putMVar d ()  -- last message; loop ends

logEvent :: Logger -> LogEvent -> IO ()
logEvent (Logger q) ev = do
    now <- getCurrentTime
    let v = Aeson.object ["time" .= now, "event" .= ev]
    atomically (writeTQueue q (LogLine v))

-- | Block until every event logged so far has been written to disk. The async
-- writer normally flushes on its own; this is for the fatal path, where the
-- process may exit right after logging its cause and never give it the chance.
flushLogger :: Logger -> IO ()
flushLogger = barrier LogFlush

-- | Flush, then close the log handle and stop the writer. For a clean shutdown
-- (or a test that must read the file back past the writer's exclusive lock).
closeLogger :: Logger -> IO ()
closeLogger = barrier LogClose

-- | Run @body@ with a logger whose file handle and writer thread live only for
-- the scope: 'closeLogger' flushes the queue, closes the handle, and stops the
-- writer on every exit — normal or exceptional — so neither can leak and a body
-- that logged right up to the end loses nothing. This is the structural lifetime
-- the resource-cleanup audit (bug 9) calls for, and the precondition for
-- flushing the log before a @restart-server@ self-exec replaces the image.
withLogger :: FilePath -> (Logger -> IO a) -> IO a
withLogger path = bracket (newLogger path) closeLogger

barrier :: (MVar () -> LogMsg) -> Logger -> IO ()
barrier mk (Logger q) = do
    done <- newEmptyMVar
    atomically (writeTQueue q (mk done))
    takeMVar done
