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
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
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
    | ClientConnected { client :: Int, term :: Text }
    | ClientDetached  { client :: Int, reason :: Text }
    | PaneSpawned     { pane :: Int, cmd :: Text }
    | PaneExited      { pane :: Int }
    | PaneResizeFailed { pane :: Int, err :: Text }
    | DaemonFault     { daemon :: Text, err :: Text }  -- ^ a background loop hit an expected fault; logged, non-fatal
    | DaemonStopped   { daemon :: Text, reason :: Text }  -- ^ a background loop gave up for good (e.g. a missing dependency); logged, non-fatal
    | CommandRun      { client :: Int, command :: Text }
    | ConfigError     { file :: FilePath, err :: Text }
    | ProtocolError   { client :: Int, err :: Text }
    | UnknownTermProp { pane :: Int, propKind :: Text, prop :: Int }  -- ^ libvterm reported a terminal property hat does not handle
    | ServerCrash     { err :: Text }
    deriving (Show, Generic)
    deriving anyclass (ToJSON)

newtype Logger = Logger (TQueue Aeson.Value)

newLogger :: FilePath -> IO Logger
newLogger path = do
    h <- openFile path AppendMode
    hSetBuffering h LineBuffering
    q <- newTQueueIO
    _ <- forkIO $ forever $ do
        v <- atomically (readTQueue q)
        BL8.hPutStrLn h (Aeson.encode v)
    pure (Logger q)

logEvent :: Logger -> LogEvent -> IO ()
logEvent (Logger q) ev = do
    now <- getCurrentTime
    let v = Aeson.object ["time" .= now, "event" .= ev]
    atomically (writeTQueue q v)
