{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The handover payload for an in-place server reload. When the server
-- re-execs its own binary, its heap is wiped but the inherited OS handles
-- survive; this is what the outgoing image serializes and the incoming image
-- reads to rebuild the live tree without respawning anything.
--
-- The format is a frozen envelope wrapping an era-gated payload
-- (see 'encodeHandover'): the incoming image can always recover the
-- version-independent 'ReloadCleanup' core — even from a blob written by an
-- incompatible version — so a mismatch hangs the inherited processes up
-- cleanly instead of orphaning them. This mirrors the compatibility discipline
-- of the wire protocol and the SQLite store (see CLAUDE.md).
module Hat.Server.Reload
    ( ReloadState (..)
    , ReloadSession (..)
    , ReloadWindow (..)
    , ReloadPane (..)
    , ReloadCleanup (..)
    , Handover (..)
    , reloadEra
    , encodeHandover
    , decodeHandover
    ) where

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise (Serialise, decode, deserialiseOrFail, encode, serialise)
import Codec.Serialise.Decoding (Decoder, decodeListLen, decodeWord)
import Codec.Serialise.Encoding (encodeListLen, encodeWord)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | The tree an era-matched reload rebuilds by adopting each pane's inherited
-- pty and child. This is the EVOLVING payload: any change to its shape (or its
-- nested types) requires bumping 'reloadEra', because an image only decodes a
-- payload whose era equals its own.
data ReloadState = ReloadState
    { sessions       :: [ReloadSession]
    , currentSession :: Maybe Text  -- ^ name of the focused session at capture
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

-- | The version-INDEPENDENT core of a handover: the fds the incoming image
-- inherited. Its shape is frozen forever, so a reader of ANY era recovers it —
-- enough to either adopt (era matches) or hang the inherited processes up and
-- release the socket (era does not), never orphaning them.
data ReloadCleanup = ReloadCleanup
    { listenFd :: Int
    , live     :: [(Int, Int)]  -- ^ (pane master fd, child pid), in tree order
    }
    deriving (Eq, Show)

-- | The outcome of reading a handover: the always-recoverable cleanup core,
-- and the tree only when the blob's era matched this build.
data Handover = Handover
    { cleanup :: ReloadCleanup
    , tree    :: Maybe ReloadState
    }
    deriving (Eq, Show)

-- | The payload format version. Bump on ANY change to 'ReloadState' or its
-- nested types. An image decodes a handover's tree only when its era equals
-- this, so a bump makes an older image fall through to safe cleanup rather than
-- misdecode. The golden-byte test pins the encoding, so a shape change that
-- forgets the bump fails the build.
reloadEra :: Int
reloadEra = 1

-- Identifies a hat reload blob, so a stray or foreign file is rejected rather
-- than misread. "HATR".
reloadMagic :: Word
reloadMagic = 0x48415452

-- | Serialize a handover as a frozen 5-element envelope
-- @[magic, era, listenFd, live, payload]@: the first four are stable forever,
-- and @payload@ is the era-gated tree embedded as an opaque byte string, so a
-- reader recovers the cleanup core without ever decoding a foreign payload.
encodeHandover :: ReloadCleanup -> ReloadState -> ByteString
encodeHandover c t = toStrictByteString $
       encodeListLen 5
    <> encodeWord reloadMagic
    <> encode reloadEra
    <> encode c.listenFd
    <> encode c.live
    <> encode (BL.toStrict (serialise t))

-- | Read a handover. The frozen envelope always yields the cleanup core; the
-- tree comes back only when the blob's era matches this build and its payload
-- decodes. 'Left' only when even the envelope is unreadable — a corrupt or
-- foreign file, where there are no fds to reclaim.
decodeHandover :: ByteString -> Either Text Handover
decodeHandover bs =
    case deserialiseFromBytes envelope (BL.fromStrict bs) of
        Left err     -> Left (T.pack (show err))
        Right (_, h) -> Right h
  where
    envelope :: Decoder s Handover
    envelope = do
        len   <- decodeListLen
        magic <- decodeWord
        unless (len == 5 && magic == reloadMagic) $
            fail "not a hat reload handover"
        era     <- decode
        lfd     <- decode
        liveH   <- decode
        payload <- decode
        let cl = ReloadCleanup { listenFd = lfd, live = liveH }
            mt | era == reloadEra =
                     either (const Nothing) Just
                            (deserialiseOrFail (BL.fromStrict payload))
               | otherwise = Nothing
        pure Handover { cleanup = cl, tree = mt }
