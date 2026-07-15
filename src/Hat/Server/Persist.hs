-- | The on-disk store for continuous session persistence: a pure
-- 'Snapshot' of the session\/window\/pane tree and its SQLite codec.
--
-- The compatibility surface is the schema. Core columns never change
-- meaning; evolving fields live in a per-row @extra@ JSON column, and DDL
-- is additive only ('bootstrap' uses @CREATE TABLE IF NOT EXISTS@). Reads
-- select explicit core columns and default anything missing, so a new
-- binary can read an old store and vice versa.
module Hat.Server.Persist
    ( Snapshot (..)
    , SessionSnap (..)
    , WindowSnap (..)
    , PaneSnap (..)
    , schemaVersion
    , withStore
    , bootstrap
    , saveSnapshot
    , loadSnapshot
    ) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
    ( FromJSON (..), ToJSON (..), Value (String), decode, encode, object
    , withObject, (.:?), (.=) )
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple

-- | A point-in-time capture of the whole tree, in restore order:
-- sessions in creation order, each window by its index, each pane in its
-- layout-string order.
data Snapshot = Snapshot
    { sessions   :: [SessionSnap]
    , lastActiveSession :: Maybe Text
                           -- ^ name of the session focused at capture time,
                           --   stored in the @meta@ table. See
                           --   'Hat.Server.pickAttachSession'.
    }
    deriving (Eq, Show)

data SessionSnap = SessionSnap
    { name      :: Text    -- ^ session name
    , startCwd  :: Text    -- ^ default working directory for new windows
    , currentIx :: Int     -- ^ index of the current window
    , lastIx    :: Maybe Int
                           -- ^ index of the last-active window (the one
                           --   @last-window@ returns to), if any; carried in
                           --   the session row's @extra@ JSON.
    , windows   :: [WindowSnap]
    }
    deriving (Eq, Show)

data WindowSnap = WindowSnap
    { ix         :: Int    -- ^ window index within the session (sparse)
    , name       :: Text
    , layout     :: Text   -- ^ tmux @window_layout@ string ('Hat.Server.LayoutString.emitLayout')
    , active     :: Int    -- ^ ordinal (in 'panes' order) of the active pane
    , lastActive :: Maybe Int
                           -- ^ ordinal (in 'panes' order) of the last-active
                           --   pane (@last-pane@ returns to it), if any;
                           --   carried in the window row's @extra@ JSON.
    , panes      :: [PaneSnap] -- ^ in layout order
    }
    deriving (Eq, Show)

data PaneSnap = PaneSnap
    { cwd     :: Text          -- ^ the pane's working directory at capture
    , command :: Maybe [Text]  -- ^ argv of the foreground program to re-exec
                               --   on restore (argv[0] is the program), or
                               --   'Nothing' to bring the pane back as a plain
                               --   shell; carried in the row's @extra@ JSON.
                               --   Storing argv as a list, not a flattened
                               --   string, keeps an argument with spaces
                               --   intact — no shell re-splitting on restore.
    }
    deriving (Eq, Show)

-- | The session row's @extra@ JSON payload. Evolving, optional fields live
-- here rather than in core columns, so old and new binaries interoperate.
newtype SessionExtra = SessionExtra (Maybe Int)  -- ^ last-active window index

instance ToJSON SessionExtra where
    toJSON (SessionExtra ml) = object (maybe [] (\l -> ["last_ix" .= l]) ml)

instance FromJSON SessionExtra where
    parseJSON = withObject "session extra" $ \o ->
        SessionExtra <$> o .:? "last_ix"

encodeSessionExtra :: Maybe Int -> Text
encodeSessionExtra ml =
    TE.decodeUtf8 (BL.toStrict (encode (SessionExtra ml)))

decodeSessionExtra :: Text -> Maybe Int
decodeSessionExtra t = case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just (SessionExtra ml) -> ml
    Nothing                -> Nothing

-- | The window row's @extra@ JSON payload.
newtype WindowExtra = WindowExtra (Maybe Int)  -- ^ last-active pane ordinal

instance ToJSON WindowExtra where
    toJSON (WindowExtra ml) = object (maybe [] (\l -> ["last_active" .= l]) ml)

instance FromJSON WindowExtra where
    parseJSON = withObject "window extra" $ \o ->
        WindowExtra <$> o .:? "last_active"

encodeWindowExtra :: Maybe Int -> Text
encodeWindowExtra ml =
    TE.decodeUtf8 (BL.toStrict (encode (WindowExtra ml)))

decodeWindowExtra :: Text -> Maybe Int
decodeWindowExtra t = case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just (WindowExtra ml) -> ml
    Nothing               -> Nothing

-- | The pane row's @extra@ JSON payload. Evolving, optional fields live
-- here rather than in core columns, so old and new binaries interoperate.
newtype PaneExtra = PaneExtra (Maybe [Text])

instance ToJSON PaneExtra where
    toJSON (PaneExtra mc) = object (maybe [] (\argv -> ["command" .= argv]) mc)

instance FromJSON PaneExtra where
    parseJSON = withObject "pane extra" $ \o -> do
        mv <- o .:? "command"
        PaneExtra <$> traverse parseArgv mv
      where
        -- Accept an argv array (current) or a bare string (a store written
        -- by an older binary that persisted only the program name).
        parseArgv (String s) = pure [s]
        parseArgv v          = parseJSON v

encodeExtra :: Maybe [Text] -> Text
encodeExtra mc = TE.decodeUtf8 (BL.toStrict (encode (PaneExtra mc)))

decodeExtra :: Text -> Maybe [Text]
decodeExtra t = case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just (PaneExtra mc) -> mc
    Nothing             -> Nothing

-- | Schema version stamped into the @meta@ table. Bump only when a change
-- cannot be expressed additively.
schemaVersion :: Int
schemaVersion = 1

-- | Open the store at @path@ (a filename, or @":memory:"@), ensure the
-- schema exists, run the action, and close. @path@ is created if absent.
withStore :: FilePath -> (Connection -> IO a) -> IO a
withStore path action =
    bracket (open path) close $ \conn -> do
        bootstrap conn
        action conn

-- | Create the tables if they do not exist. Additive only.
bootstrap :: Connection -> IO ()
bootstrap conn = do
    -- The poll thread and an explicit kill-server save may open write
    -- connections at once; wait for the lock rather than erroring.
    execute_ conn "PRAGMA busy_timeout = 3000"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS meta \
        \(key TEXT PRIMARY KEY, value TEXT NOT NULL)"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS session \
        \(seq INTEGER PRIMARY KEY, name TEXT NOT NULL, \
        \start_cwd TEXT NOT NULL, current_ix INTEGER NOT NULL, \
        \extra TEXT NOT NULL DEFAULT '{}')"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS window \
        \(session_seq INTEGER NOT NULL, ix INTEGER NOT NULL, \
        \name TEXT NOT NULL, layout TEXT NOT NULL, active INTEGER NOT NULL, \
        \extra TEXT NOT NULL DEFAULT '{}', \
        \PRIMARY KEY (session_seq, ix))"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS pane \
        \(session_seq INTEGER NOT NULL, window_ix INTEGER NOT NULL, \
        \ordinal INTEGER NOT NULL, cwd TEXT NOT NULL, \
        \extra TEXT NOT NULL DEFAULT '{}', \
        \PRIMARY KEY (session_seq, window_ix, ordinal))"
    -- Additively upgrade a store written by an older binary that predates
    -- one of these columns. New columns default, so old rows stay valid.
    mapM_ (\t -> ensureColumn conn t "extra" "TEXT NOT NULL DEFAULT '{}'")
        ["session", "window", "pane"]

-- | Add @col@ to @table@ if it is not already present. Table and column
-- names are internal literals, so interpolating them into the DDL is safe.
ensureColumn :: Connection -> Text -> Text -> Text -> IO ()
ensureColumn conn table col decl = do
    info <- query_ conn
        (fromString (T.unpack ("PRAGMA table_info(" <> table <> ")")))
        :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
    unless (col `elem` [ nm | (_, nm, _, _, _, _) <- info ]) $
        execute_ conn (fromString (T.unpack
            ("ALTER TABLE " <> table <> " ADD COLUMN " <> col <> " " <> decl)))

-- | Replace the store's contents with @snap@ in a single transaction. The
-- whole tree is small, so we rewrite it wholesale rather than diffing.
saveSnapshot :: Connection -> Snapshot -> IO ()
saveSnapshot conn snap = withTransaction conn $ do
    execute_ conn "DELETE FROM pane"
    execute_ conn "DELETE FROM window"
    execute_ conn "DELETE FROM session"
    execute conn
        "INSERT INTO meta (key, value) VALUES ('schema_version', ?) \
        \ON CONFLICT(key) DO UPDATE SET value = excluded.value"
        (Only (T.pack (show schemaVersion)))
    execute conn
        "INSERT INTO meta (key, value) VALUES ('last_active_session', ?) \
        \ON CONFLICT(key) DO UPDATE SET value = excluded.value"
        (Only (fromMaybe "" snap.lastActiveSession))
    mapM_ insertSession (zip [0 ..] snap.sessions)
  where
    insertSession :: (Int, SessionSnap) -> IO ()
    insertSession (sseq, s) = do
        execute conn
            "INSERT INTO session (seq, name, start_cwd, current_ix, extra) \
            \VALUES (?, ?, ?, ?, ?)"
            (sseq, s.name, s.startCwd, s.currentIx, encodeSessionExtra s.lastIx)
        mapM_ (insertWindow sseq) s.windows
    insertWindow :: Int -> WindowSnap -> IO ()
    insertWindow sseq w = do
        execute conn
            "INSERT INTO window (session_seq, ix, name, layout, active, extra) \
            \VALUES (?, ?, ?, ?, ?, ?)"
            (sseq, w.ix, w.name, w.layout, w.active, encodeWindowExtra w.lastActive)
        mapM_ (insertPane sseq w.ix) (zip [0 ..] w.panes)
    insertPane :: Int -> Int -> (Int, PaneSnap) -> IO ()
    insertPane sseq wix (ord, p) =
        execute conn
            "INSERT INTO pane (session_seq, window_ix, ordinal, cwd, extra) \
            \VALUES (?, ?, ?, ?, ?)"
            (sseq, wix, ord, p.cwd, encodeExtra p.command)

-- | Read the whole tree back. Returns an empty snapshot for a fresh store.
loadSnapshot :: Connection -> IO Snapshot
loadSnapshot conn = do
    srows <- query_ conn
        "SELECT seq, name, start_cwd, current_ix, extra FROM session ORDER BY seq"
        :: IO [(Int, Text, Text, Int, Text)]
    metaRows <- query conn
        "SELECT value FROM meta WHERE key = ?" (Only ("last_active_session" :: Text))
        :: IO [Only Text]
    let la = case metaRows of
            (Only v : _) | not (T.null v) -> Just v
            _                             -> Nothing
    Snapshot <$> mapM loadSession srows <*> pure la
  where
    loadSession :: (Int, Text, Text, Int, Text) -> IO SessionSnap
    loadSession (sseq, nm, cwd0, curIx, sex) = do
        wrows <- query conn
            "SELECT ix, name, layout, active, extra FROM window \
            \WHERE session_seq = ? ORDER BY ix"
            (Only sseq) :: IO [(Int, Text, Text, Int, Text)]
        ws <- mapM (loadWindow sseq) wrows
        pure SessionSnap
            { name = nm, startCwd = cwd0, currentIx = curIx
            , lastIx = decodeSessionExtra sex, windows = ws }
    loadWindow :: Int -> (Int, Text, Text, Int, Text) -> IO WindowSnap
    loadWindow sseq (wix, nm, lay, act, wex) = do
        prows <- query conn
            "SELECT cwd, extra FROM pane \
            \WHERE session_seq = ? AND window_ix = ? ORDER BY ordinal"
            (sseq, wix) :: IO [(Text, Text)]
        pure WindowSnap
            { ix = wix, name = nm, layout = lay, active = act
            , lastActive = decodeWindowExtra wex
            , panes = [ PaneSnap { cwd = c, command = decodeExtra ex }
                      | (c, ex) <- prows ] }
