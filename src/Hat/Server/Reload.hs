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
    , ReloadModes (..)
    , ReloadScreen (..)
    , emptyReloadScreen
    , ReloadCleanup (..)
    , Handover (..)
    , reloadEra
    , encodeHandover
    , decodeHandover
    ) where

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (decodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise (Serialise, decode, deserialiseOrFail, encode, serialise)
import Codec.Serialise.Decoding (Decoder, decodeListLen, decodeWord)
import Codec.Serialise.Encoding (encodeListLen, encodeWord)
import Control.Monad (replicateM_, unless)
import Data.ByteString (ByteString)
import Data.Maybe (maybeToList)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Hat.Term.Cell (Cell, Style)
import qualified Hat.Term.Cell as Cell

-- | The tree an era-matched reload rebuilds by adopting each pane's inherited
-- pty and child. This is the EVOLVING payload: any change to its shape (or its
-- nested types) requires bumping 'reloadEra', because an image only decodes a
-- payload whose era equals its own.
data ReloadState = ReloadState
    { sessions       :: [ReloadSession]
    , currentSession :: Maybe Text  -- ^ name of the focused session at capture
    , lastSession    :: Maybe Text  -- ^ name of the alternate session
                                    --   (@switch-client -l@ returns to it), if
                                    --   any; a reattaching client adopts it. See
                                    --   'Hat.Server.rebuildReload'.
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

data ReloadSession = ReloadSession
    { name      :: Text
    , startCwd  :: Text
    , currentIx :: Int
    , windowHist :: [Int]  -- ^ MRU window indices, head first; see 'Hat.Server.Mru'
    , windows   :: [ReloadWindow]
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

data ReloadWindow = ReloadWindow
    { ix         :: Int
    , name       :: Text
    , layout     :: Text  -- ^ tmux @window_layout@ string
    , active     :: Int   -- ^ ordinal of the active pane
    , paneHist   :: [Int]  -- ^ MRU pane ordinals, head first
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
    , modes    :: ReloadModes  -- ^ replayed into the adopted pane's fresh
                               --   emulator; see 'Hat.Server.adoptPane'
    , screen   :: ReloadScreen -- ^ the live grid and scrollback, replayed into
                               --   the adopted pane; see 'Hat.Server.adoptPane'
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

-- | A pane's captured screen: the live grid (top row first), its cursor, its
-- alternate-screen flag, and the scrollback (oldest line first). Replayed into
-- the adopted pane's fresh emulator so a full-screen program survives a reload
-- with its display intact. An 'emptyReloadScreen' restores to a blank pane,
-- which is what a pre-screen (era ≤ 2) blob migrates to.
data ReloadScreen = ReloadScreen
    { altScreen     :: Bool
    , cursorRow     :: Int
    , cursorCol     :: Int
    , cursorVisible :: Bool
    -- These should probably be vectors, but I'm leaving it as lists to avoid a
    -- migration. Besides, they get read directly into vectors anyway, and the
    -- serialized shape is basically identical.
    , rows          :: [[Cell]]
    , scrollback    :: [[Cell]]
    , pen           :: Style  -- ^ the live pen (SGR the next glyph takes); see
                              --   'Hat.Server.captureReloadScreen'
    }
    deriving (Eq, Show, Generic)

-- Appended 'pen' tolerantly (era 6): a pre-pen (era ≤ 5) screen is a six-field
-- list, which decodes with the default pen — the same additive-leaf trick the
-- 'Style' and 'Hello' codecs use, so no positional-mirror migration is needed.
-- The list is (constructor-tag word, then fields), matching what a derived
-- Serialise would emit, so a pre-pen (era ≤ 5) six-field screen — 'encodeListLen
-- 7' with no pen — decodes here with the default pen.
instance Serialise ReloadScreen where
    encode s =
           encodeListLen 8
        <> encodeWord 0
        <> encode s.altScreen <> encode s.cursorRow <> encode s.cursorCol
        <> encode s.cursorVisible <> encode s.rows <> encode s.scrollback
        <> encode s.pen
    decode = do
        len <- decodeListLen
        _   <- decodeWord
        s <- ReloadScreen
            <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode
            <*> (if len >= 8 then decode else pure Cell.defaultStyle)
        replicateM_ (max 0 (len - 8)) (() <$ decodeTerm)
        pure s

-- | The blank screen a pane with no captured display restores to: no grid, no
-- scrollback, primary buffer, cursor home and visible. See 'ReloadScreen'.
emptyReloadScreen :: ReloadScreen
emptyReloadScreen = ReloadScreen
    { altScreen = False, cursorRow = 0, cursorCol = 0, cursorVisible = True
    , rows = [], scrollback = [], pen = Cell.defaultStyle }

-- | The app-set mode subscriptions a pane carries across a reload, so a program
-- adopted into a fresh emulator keeps them. A blank set (everything off) is what
-- an era-1 pane, which never recorded them, migrates to.
data ReloadModes = ReloadModes
    { colorReport :: Bool  -- ^ ?2031 color-scheme reporting
    , focusReport :: Bool  -- ^ ?1004 focus reporting
    , mouse       :: Int   -- ^ mouse tracking: 0 off, 1 click, 2 drag, 3 move
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
-- and either the tree to adopt or the reason it cannot be used (a newer era,
-- an unmigratable old era, or a corrupt payload — the caller then falls back
-- to a clean restart driven by 'cleanup').
data Handover = Handover
    { cleanup :: ReloadCleanup
    , tree    :: Either Text ReloadState
    }
    deriving (Eq, Show)

-- | The payload format version. Bump on ANY change to 'ReloadState' or its
-- nested types. An image decodes a handover's tree only when its era equals
-- this, so a bump makes an older image fall through to safe cleanup rather than
-- misdecode. The golden-byte test pins the encoding, so a shape change that
-- forgets the bump fails the build.
reloadEra :: Int
reloadEra = 7

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
        pure Handover { cleanup = cl, tree = decodeReloadTree era payload }

-- | Decode a handover payload written at era @e@ into the CURRENT
-- 'ReloadState', migrating it forward. This is where backward compatibility
-- lives: a build at era X must read every era @1..X@ (enforced by a committed
-- vector per era in the reload corpus test). A payload from a newer era, or one
-- too old to migrate, is a 'Left' — the caller then cleanly restarts rather
-- than adopting a tree it can't trust.
--
-- To introduce era X+1: freeze the current payload types as @…V\<X\>@, keep
-- their decoder, add a @migrate@ from them to the new shape, and add an @e ==
-- X@ arm below that decodes-then-migrates. Add a corpus vector for the new era.
decodeReloadTree :: Int -> ByteString -> Either Text ReloadState
decodeReloadTree e payload
    | e == reloadEra = case deserialiseOrFail (BL.fromStrict payload) of
        Right t  -> Right t
        Left err -> Left ("corrupt reload payload: " <> T.pack (show err))
    -- Eras 4, 5 and 6 share the session/window shape (a single-Int "last", not
    -- the MRU stack). They differ only by additive leaves (era 4 lacks
    -- 'Style.faint', era 5 lacks 'ReloadScreen.pen'), which the hand-written
    -- Style and ReloadScreen decoders default, so one 'ReloadStateV6' mirror
    -- decodes all three; 'migrateV6' lifts the single "last" into a stack.
    | e == 6 || e == 5 || e == 4 = case deserialiseOrFail (BL.fromStrict payload) of
        Right v  -> Right (migrateV6 v)
        Left err -> Left ("corrupt reload payload: " <> T.pack (show err))
    | e == 3 = case deserialiseOrFail (BL.fromStrict payload) of
        Right v  -> Right (migrateV6 (migrateV3 v))
        Left err -> Left ("corrupt reload payload: " <> T.pack (show err))
    | e == 2 = case deserialiseOrFail (BL.fromStrict payload) of
        Right v  -> Right (migrateV6 (migrateV3 (migrateV2 v)))
        Left err -> Left ("corrupt reload payload: " <> T.pack (show err))
    | e == 1 = case deserialiseOrFail (BL.fromStrict payload) of
        Right v  -> Right (migrateV6 (migrateV3 (migrateV2 (migrateV1 v))))
        Left err -> Left ("corrupt reload payload: " <> T.pack (show err))
    | e > reloadEra =
        Left ("reload handover from a newer hat (era " <> T.pack (show e)
              <> "); this build is era " <> T.pack (show reloadEra))
    | otherwise =
        Left ("no migration for reload era " <> T.pack (show e))

-- Era-1 payload shapes, frozen: a pane carried no mode subscriptions. CBOR
-- Generic keys on constructor arity and field order, not names, so these
-- positional mirrors decode an era-1 blob that a modes-bearing 'ReloadPane'
-- no longer can. See 'decodeReloadTree'.
data ReloadStateV1 = ReloadStateV1 [ReloadSessionV1] (Maybe Text)
    deriving (Generic) deriving anyclass (Serialise)
data ReloadSessionV1 = ReloadSessionV1 Text Text Int (Maybe Int) [ReloadWindowV1]
    deriving (Generic) deriving anyclass (Serialise)
data ReloadWindowV1 =
    ReloadWindowV1 Int Text Text Int (Maybe Int) Bool [ReloadPaneV1]
    deriving (Generic) deriving anyclass (Serialise)
data ReloadPaneV1 = ReloadPaneV1 Text Int Int
    deriving (Generic) deriving anyclass (Serialise)

-- Era-2 payload shapes, frozen: a pane carried its modes but no screen. Same
-- positional-mirror trick as the era-1 shapes, so a screen-bearing 'ReloadPane'
-- can still decode an era-2 blob. See 'decodeReloadTree'.
data ReloadStateV2 = ReloadStateV2 [ReloadSessionV2] (Maybe Text)
    deriving (Generic) deriving anyclass (Serialise)
data ReloadSessionV2 = ReloadSessionV2 Text Text Int (Maybe Int) [ReloadWindowV2]
    deriving (Generic) deriving anyclass (Serialise)
data ReloadWindowV2 =
    ReloadWindowV2 Int Text Text Int (Maybe Int) Bool [ReloadPaneV2]
    deriving (Generic) deriving anyclass (Serialise)
data ReloadPaneV2 = ReloadPaneV2 Text Int Int ReloadModes
    deriving (Generic) deriving anyclass (Serialise)

-- | Carry an era-1 tree forward to the era-2 shape: every pane gains a blank
-- mode set, since an era-1 image never recorded one. See 'decodeReloadTree'.
migrateV1 :: ReloadStateV1 -> ReloadStateV2
migrateV1 (ReloadStateV1 sess cur) = ReloadStateV2 (map migSession sess) cur
  where
    migSession (ReloadSessionV1 nm cwd' ci li wins) =
        ReloadSessionV2 nm cwd' ci li (map migWindow wins)
    migWindow (ReloadWindowV1 ix' nm lay act la ar ps) =
        ReloadWindowV2 ix' nm lay act la ar (map migPane ps)
    migPane (ReloadPaneV1 cwd' mfd cpid) =
        ReloadPaneV2 cwd' mfd cpid (ReloadModes False False 0)

-- | Carry an era-2 tree forward to the era-3 shape: every pane gains a blank
-- screen, since an era-2 image never captured one, so it restores to a blank
-- pane (the same behaviour a reload had before live-screen preservation). See
-- 'decodeReloadTree'.
migrateV2 :: ReloadStateV2 -> ReloadStateV3
migrateV2 (ReloadStateV2 sess cur) = ReloadStateV3 (map migSession sess) cur
  where
    migSession (ReloadSessionV2 nm cwd' ci li wins) =
        ReloadSessionV6 nm cwd' ci li (map migWindow wins)
    migWindow (ReloadWindowV2 ix' nm lay act la ar ps) =
        ReloadWindowV6 ix' nm lay act la ar (map migPane ps)
    migPane (ReloadPaneV2 cwd' mfd cpid ms) =
        ReloadPane cwd' mfd cpid ms emptyReloadScreen

-- Era-3 top-level shape, frozen: the tree carried no alternate session, over
-- the era 3–6 session/window shape ('ReloadSessionV6'). See 'decodeReloadTree'.
data ReloadStateV3 = ReloadStateV3 [ReloadSessionV6] (Maybe Text)
    deriving (Generic) deriving anyclass (Serialise)

-- | Carry an era-3 tree forward: it gains an empty alternate session, since an
-- era-3 image never recorded one. See 'decodeReloadTree'.
migrateV3 :: ReloadStateV3 -> ReloadStateV6
migrateV3 (ReloadStateV3 sess cur) = ReloadStateV6 sess cur Nothing

-- Era 3–6 session/window shapes, frozen: a single-Int "last", not the MRU
-- stack. The nested 'ReloadPane' is today's, so its tolerant leaf decoders
-- default era-4's missing faint and era-5's missing pen. See 'decodeReloadTree'.
data ReloadSessionV6 = ReloadSessionV6 Text Text Int (Maybe Int) [ReloadWindowV6]
    deriving (Generic) deriving anyclass (Serialise)
data ReloadWindowV6 =
    ReloadWindowV6 Int Text Text Int (Maybe Int) Bool [ReloadPane]
    deriving (Generic) deriving anyclass (Serialise)

-- Era-6 top-level shape, frozen: it carried the alternate session (gained at
-- era 4) but still a single-Int "last" per session/window. See 'decodeReloadTree'.
data ReloadStateV6 = ReloadStateV6 [ReloadSessionV6] (Maybe Text) (Maybe Text)
    deriving (Generic) deriving anyclass (Serialise)

-- | Carry an era-4/5/6 tree forward: each session's and window's single
-- last-active becomes a one-deep MRU stack. See 'decodeReloadTree'.
migrateV6 :: ReloadStateV6 -> ReloadState
migrateV6 (ReloadStateV6 sess cur lst) = ReloadState (map migSession sess) cur lst
  where
    migSession (ReloadSessionV6 nm cwd' ci li wins) =
        ReloadSession nm cwd' ci (maybeToList li) (map migWindow wins)
    migWindow (ReloadWindowV6 ix' nm lay act la ar ps) =
        ReloadWindow ix' nm lay act (maybeToList la) ar ps
