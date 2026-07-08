{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

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
    , nullLogger
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
    | ClientConnected { client :: Int, term :: Text }
    | ClientDetached  { client :: Int, reason :: Text }
    | PaneSpawned     { pane :: Int, cmd :: Text }
    | PaneExited      { pane :: Int }
    | CommandRun      { client :: Int, command :: Text }
    | ConfigError     { file :: FilePath, err :: Text }
    | ProtocolError   { client :: Int, err :: Text }
    | ServerCrash     { err :: Text }
    deriving (Show, Generic)
    deriving anyclass (ToJSON)

data Logger
    = Logger (TQueue Aeson.Value)
    | NullLogger

newLogger :: FilePath -> IO Logger
newLogger path = do
    h <- openFile path AppendMode
    hSetBuffering h LineBuffering
    q <- newTQueueIO
    _ <- forkIO $ forever $ do
        v <- atomically (readTQueue q)
        BL8.hPutStrLn h (Aeson.encode v)
    pure (Logger q)

nullLogger :: Logger
nullLogger = NullLogger

logEvent :: Logger -> LogEvent -> IO ()
logEvent NullLogger _ = pure ()
logEvent (Logger q) ev = do
    now <- getCurrentTime
    let v = Aeson.object ["time" .= now, "event" .= ev]
    atomically (writeTQueue q v)
