{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The handover payload for an in-place server reload. When the server
-- re-execs its own binary, its heap is wiped but the inherited OS handles
-- survive; this is what the outgoing image serializes and the incoming image
-- reads to rebuild the live tree without respawning anything.
--
-- Distinct from 'Hat.Server.Persist.Snapshot', the durable store: that
-- records how to RESPAWN a tree from scratch across a real restart; this
-- records how to RE-ADOPT one whose programs are still running.
module Hat.Server.Reload
    ( ReloadState (..)
    , ReloadSession (..)
    , ReloadWindow (..)
    , ReloadPane (..)
    , encodeReload
    , decodeReload
    ) where

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | The session tree plus the live handles carried across the self-exec: the
-- listening socket fd, and per pane its pty master fd and child pid.
data ReloadState = ReloadState
    { sessions       :: [ReloadSession]
    , currentSession :: Maybe Text  -- ^ name of the focused session at capture
    , listenFd       :: Int         -- ^ inherited listening socket
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

data ReloadSession = ReloadSession
    { name      :: Text
    , startCwd  :: Text
    , currentIx :: Int
    , lastIx    :: Maybe Int
    , windows   :: [ReloadWindow]
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

data ReloadWindow = ReloadWindow
    { ix         :: Int
    , name       :: Text
    , layout     :: Text  -- ^ tmux @window_layout@ string
    , active     :: Int   -- ^ ordinal of the active pane
    , lastActive :: Maybe Int
    , autoRename :: Bool
    , panes      :: [ReloadPane]
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

-- | A pane's working directory and the live OS handles the incoming image
-- adopts (see 'Hat.Term.Pty.adopt') instead of spawning fresh.
data ReloadPane = ReloadPane
    { cwd      :: Text
    , masterFd :: Int
    , childPid :: Int
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

-- | Serialize the handover payload to CBOR for the temp handover file.
encodeReload :: ReloadState -> B.ByteString
encodeReload = BL.toStrict . serialise

-- | Read a handover payload back, 'Left' with a reason on a malformed blob.
decodeReload :: B.ByteString -> Either Text ReloadState
decodeReload bs = case deserialiseOrFail (BL.fromStrict bs) of
    Right rs -> Right rs
    Left err -> Left (T.pack (show err))
