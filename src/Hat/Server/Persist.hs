-- | The on-disk store for continuous session persistence: a pure
-- 'Snapshot' of the session\/window\/pane tree and its SQLite codec. The
-- live tables mirror the newest tree; past trees are kept as pruned
-- history generations (see 'archiveSnapshot').
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
    , Archived (..)
    , archiveSnapshot
    , listArchived
    , loadArchived
    , clearLive
    ) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
    ( FromJSON (..), ToJSON (..), Value (String), decode, encode, object
    , withObject, (.:?), (.=) )
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe, maybeToList)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
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
    , windowHist :: [Int]
                           -- ^ MRU window indices, head = last-active (the one
                           --   @last-window@ returns to); carried in the session
                           --   row's @extra@ JSON. See 'Hat.Server.Mru'.
    , windows   :: [WindowSnap]
    }
    deriving (Eq, Show)

data WindowSnap = WindowSnap
    { ix         :: Int    -- ^ window index within the session (sparse)
    , name       :: Text
    , layout     :: Text   -- ^ tmux @window_layout@ string ('Hat.Server.LayoutString.emitLayout')
    , active     :: Int    -- ^ ordinal (in 'panes' order) of the active pane
    , paneHist   :: [Int]
                           -- ^ MRU pane ordinals (in 'panes' order), head =
                           --   last-active (@last-pane@ returns to it); carried
                           --   in the window row's @extra@ JSON.
    , autoRename :: Bool   -- ^ whether the window is in automatic-rename
                           --   state (its name tracks the active pane) rather
                           --   than manually named; carried in the window
                           --   row's @extra@ JSON.
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
    , shellSpawned :: Bool     -- ^ whether 'command' was running as a child of
                               --   the pane's interactive shell (the user typed
                               --   it) rather than as the pane's top-level
                               --   process (a direct @new-window prog@); carried
                               --   in the row's @extra@ JSON. A store written
                               --   before this field omits the key; it defaults
                               --   to 'False'. See 'Hat.Server.restoreRun'.
    }
    deriving (Eq, Show)

-- | The session row's @extra@ JSON payload. Evolving, optional fields live
-- here rather than in core columns, so old and new binaries interoperate.
newtype SessionExtra = SessionExtra [Int]  -- ^ MRU window indices, head first

-- @last_ix@ mirrors the head for readers predating the stack; @last_stack@
-- carries the whole history. A reader prefers the stack, else lifts the head.
instance ToJSON SessionExtra where
    toJSON (SessionExtra hist) = object $
        ["last_ix" .= h | h <- take 1 hist]
        ++ ["last_stack" .= hist | not (null hist)]

instance FromJSON SessionExtra where
    parseJSON = withObject "session extra" $ \o -> do
        stack  <- o .:? "last_stack"
        legacy <- o .:? "last_ix"
        pure (SessionExtra (fromMaybe (maybeToList legacy) stack))

encodeSessionExtra :: [Int] -> Text
encodeSessionExtra hist =
    TE.decodeUtf8 (BL.toStrict (encode (SessionExtra hist)))

decodeSessionExtra :: Text -> [Int]
decodeSessionExtra t = case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just (SessionExtra hist) -> hist
    Nothing                  -> []

-- | The window row's @extra@ JSON payload: the last-active pane ordinal and
-- the automatic-rename flag. A store written before @auto_rename@ existed
-- omits the key; it defaults to 'False', matching the old restore behavior
-- of pinning a restored window's name.
data WindowExtra = WindowExtra [Int] Bool  -- ^ MRU pane ordinals (head first)

-- @last_active@ mirrors the head for readers predating the stack;
-- @last_active_stack@ carries the whole history. See 'SessionExtra'.
instance ToJSON WindowExtra where
    toJSON (WindowExtra hist auto) =
        object (["last_active" .= h | h <- take 1 hist]
                ++ ["last_active_stack" .= hist | not (null hist)]
                ++ ["auto_rename" .= auto | auto])

instance FromJSON WindowExtra where
    parseJSON = withObject "window extra" $ \o -> do
        stack  <- o .:? "last_active_stack"
        legacy <- o .:? "last_active"
        auto   <- fromMaybe False <$> o .:? "auto_rename"
        pure (WindowExtra (fromMaybe (maybeToList legacy) stack) auto)

encodeWindowExtra :: [Int] -> Bool -> Text
encodeWindowExtra hist auto =
    TE.decodeUtf8 (BL.toStrict (encode (WindowExtra hist auto)))

decodeWindowExtra :: Text -> ([Int], Bool)
decodeWindowExtra t = case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just (WindowExtra hist auto) -> (hist, auto)
    Nothing                      -> ([], False)

-- | The pane row's @extra@ JSON payload: the captured command and whether it
-- was spawned from inside the pane's interactive shell. Evolving, optional
-- fields live here rather than in core columns, so old and new binaries
-- interoperate. A store written before @shell_spawned@ existed omits the key;
-- it defaults to 'False'.
data PaneExtra = PaneExtra (Maybe [Text]) Bool

instance ToJSON PaneExtra where
    toJSON (PaneExtra mc shellSp) =
        object (maybe [] (\argv -> ["command" .= argv]) mc
                ++ ["shell_spawned" .= shellSp | shellSp])

instance FromJSON PaneExtra where
    parseJSON = withObject "pane extra" $ \o -> do
        mv <- o .:? "command"
        mc <- traverse parseArgv mv
        shellSp <- fromMaybe False <$> o .:? "shell_spawned"
        pure (PaneExtra mc shellSp)
      where
        -- Accept an argv array (current) or a bare string (a store written
        -- by an older binary that persisted only the program name).
        parseArgv (String s) = pure [s]
        parseArgv v          = parseJSON v

encodeExtra :: Maybe [Text] -> Bool -> Text
encodeExtra mc shellSp =
    TE.decodeUtf8 (BL.toStrict (encode (PaneExtra mc shellSp)))

decodeExtra :: Text -> (Maybe [Text], Bool)
decodeExtra t = case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just (PaneExtra mc shellSp) -> (mc, shellSp)
    Nothing                     -> (Nothing, False)

-- | Schema version stamped into the @meta@ table. A forward-looking breadcrumb,
-- NOT a read-time gate: 'loadSnapshot' reads leniently (core columns, defaulting
-- anything absent, ignoring the unknown), so an old or new store loads without
-- consulting this. Bump it only when a change genuinely cannot be expressed
-- additively — that is the day a reader would finally branch on it to migrate.
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
    -- Each row is one frozen past tree; the live tables above always hold
    -- the newest. AUTOINCREMENT keeps generation ids unique for good, so a
    -- pruned id is never reused. See 'archiveSnapshot'.
    execute_ conn
        "CREATE TABLE IF NOT EXISTS snapshot \
        \(gen INTEGER PRIMARY KEY AUTOINCREMENT, \
        \saved_at TEXT NOT NULL DEFAULT '', \
        \data TEXT NOT NULL, \
        \extra TEXT NOT NULL DEFAULT '{}')"
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
    -- Capture time; 'archiveSnapshot' stamps history rows with it.
    now <- nowStamp
    execute conn
        "INSERT INTO meta (key, value) VALUES ('saved_at', ?) \
        \ON CONFLICT(key) DO UPDATE SET value = excluded.value"
        (Only now)
    mapM_ insertSession (zip [0 ..] snap.sessions)
  where
    insertSession :: (Int, SessionSnap) -> IO ()
    insertSession (sseq, s) = do
        execute conn
            "INSERT INTO session (seq, name, start_cwd, current_ix, extra) \
            \VALUES (?, ?, ?, ?, ?)"
            (sseq, s.name, s.startCwd, s.currentIx, encodeSessionExtra s.windowHist)
        mapM_ (insertWindow sseq) s.windows
    insertWindow :: Int -> WindowSnap -> IO ()
    insertWindow sseq w = do
        execute conn
            "INSERT INTO window (session_seq, ix, name, layout, active, extra) \
            \VALUES (?, ?, ?, ?, ?, ?)"
            (sseq, w.ix, w.name, w.layout, w.active, encodeWindowExtra w.paneHist w.autoRename)
        mapM_ (insertPane sseq w.ix) (zip [0 ..] w.panes)
    insertPane :: Int -> Int -> (Int, PaneSnap) -> IO ()
    insertPane sseq wix (ord, p) =
        execute conn
            "INSERT INTO pane (session_seq, window_ix, ordinal, cwd, extra) \
            \VALUES (?, ?, ?, ?, ?)"
            (sseq, wix, ord, p.cwd, encodeExtra p.command p.shellSpawned)

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
            , windowHist = decodeSessionExtra sex, windows = ws }
    loadWindow :: Int -> (Int, Text, Text, Int, Text) -> IO WindowSnap
    loadWindow sseq (wix, nm, lay, act, wex) = do
        prows <- query conn
            "SELECT cwd, extra FROM pane \
            \WHERE session_seq = ? AND window_ix = ? ORDER BY ordinal"
            (sseq, wix) :: IO [(Text, Text)]
        let (paneOrds, auto) = decodeWindowExtra wex
        pure WindowSnap
            { ix = wix, name = nm, layout = lay, active = act
            , paneHist = paneOrds, autoRename = auto
            , panes = [ PaneSnap { cwd = c, command = mc, shellSpawned = shellSp }
                      | (c, ex) <- prows, let (mc, shellSp) = decodeExtra ex ] }

-- Snapshot history --------------------------------------------------------

-- | One history generation: its id (unique for the store's life), capture
-- time (the store's ISO-8601 UTC timestamp format), and decoded tree.
data Archived = Archived
    { gen      :: Int
    , savedAt  :: Text
    , snapshot :: Snapshot
    }
    deriving (Eq, Show)

-- | Append @snap@ to the history table as a new generation — stamped with
-- the mirror's capture time — and prune to the newest @limit@ generations.
-- Skipped for an empty tree, a tree identical to the newest generation
-- (no churn across idle restarts), or a limit ≤ 0 (history off).
archiveSnapshot :: Connection -> Int -> Snapshot -> IO ()
archiveSnapshot conn limit snap
    | limit <= 0 || null snap.sessions = pure ()
    | otherwise = withTransaction conn $ do
        newest <- query_ conn
            "SELECT data FROM snapshot ORDER BY gen DESC LIMIT 1"
            :: IO [Only Text]
        let unchanged = case newest of
                (Only d : _) -> decodeSnapshotJson d == Just snap
                []           -> False
        unless unchanged $ do
            stamp <- metaValue conn "saved_at" >>= maybe nowStamp pure
            execute conn
                "INSERT INTO snapshot (saved_at, data) VALUES (?, ?)"
                (stamp, encodeSnapshotJson snap)
            execute conn
                "DELETE FROM snapshot WHERE gen NOT IN \
                \(SELECT gen FROM snapshot ORDER BY gen DESC LIMIT ?)"
                (Only limit)

-- | Every archived generation, newest first. A row whose payload no
-- longer decodes is skipped, not fatal.
listArchived :: Connection -> IO [Archived]
listArchived conn = do
    rows <- query_ conn
        "SELECT gen, saved_at, data FROM snapshot ORDER BY gen DESC"
        :: IO [(Int, Text, Text)]
    pure [ Archived { gen = g, savedAt = at, snapshot = s }
         | (g, at, d) <- rows, Just s <- [decodeSnapshotJson d] ]

-- | One archived generation's tree, if it exists and decodes.
loadArchived :: Connection -> Int -> IO (Maybe Snapshot)
loadArchived conn g = do
    rows <- query conn "SELECT data FROM snapshot WHERE gen = ?" (Only g)
        :: IO [Only Text]
    pure $ case rows of
        (Only d : _) -> decodeSnapshotJson d
        []           -> Nothing

-- | Drop the live tree (a later 'loadSnapshot' is empty), leaving the
-- history untouched.
clearLive :: Connection -> IO ()
clearLive conn = withTransaction conn $ do
    execute_ conn "DELETE FROM pane"
    execute_ conn "DELETE FROM window"
    execute_ conn "DELETE FROM session"
    execute conn "DELETE FROM meta WHERE key = ?"
        (Only ("last_active_session" :: Text))

-- | UTC wall-clock now in the store's timestamp format.
nowStamp :: IO Text
nowStamp =
    T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime

metaValue :: Connection -> Text -> IO (Maybe Text)
metaValue conn key = do
    rows <- query conn "SELECT value FROM meta WHERE key = ?" (Only key)
        :: IO [Only Text]
    pure $ case rows of
        (Only v : _) -> Just v
        []           -> Nothing

-- The history row's @data@ payload: the whole tree as JSON, evolved the
-- same way as the @extra@ columns — append optional keys, default the
-- absent, ignore the unknown. Instances are written by hand (explicit
-- keys, never derived layout).

encodeSnapshotJson :: Snapshot -> Text
encodeSnapshotJson = TE.decodeUtf8 . BL.toStrict . encode

decodeSnapshotJson :: Text -> Maybe Snapshot
decodeSnapshotJson = decode . BL.fromStrict . TE.encodeUtf8

instance ToJSON Snapshot where
    toJSON s = object $
        ["sessions" .= s.sessions]
        <> maybe [] (\n -> ["last_active_session" .= n]) s.lastActiveSession

instance FromJSON Snapshot where
    parseJSON = withObject "snapshot" $ \o -> do
        ss <- fromMaybe [] <$> o .:? "sessions"
        la <- o .:? "last_active_session"
        pure Snapshot { sessions = ss, lastActiveSession = la }

instance ToJSON SessionSnap where
    toJSON s = object $
        [ "name" .= s.name, "start_cwd" .= s.startCwd
        , "current_ix" .= s.currentIx ]
        <> ["last_ix" .= h | h <- take 1 s.windowHist]
        <> ["last_stack" .= s.windowHist | not (null s.windowHist)]
        <> ["windows" .= s.windows]

instance FromJSON SessionSnap where
    parseJSON = withObject "session" $ \o -> do
        nm     <- fromMaybe "" <$> o .:? "name"
        cwd0   <- fromMaybe "" <$> o .:? "start_cwd"
        curIx  <- fromMaybe 0 <$> o .:? "current_ix"
        stack  <- o .:? "last_stack"
        legacy <- o .:? "last_ix"
        ws     <- fromMaybe [] <$> o .:? "windows"
        pure SessionSnap
            { name = nm, startCwd = cwd0, currentIx = curIx
            , windowHist = fromMaybe (maybeToList legacy) stack, windows = ws }

instance ToJSON WindowSnap where
    toJSON w = object $
        [ "ix" .= w.ix, "name" .= w.name, "layout" .= w.layout
        , "active" .= w.active ]
        <> ["last_active" .= h | h <- take 1 w.paneHist]
        <> ["last_active_stack" .= w.paneHist | not (null w.paneHist)]
        <> ["auto_rename" .= True | w.autoRename]
        <> ["panes" .= w.panes]

instance FromJSON WindowSnap where
    parseJSON = withObject "window" $ \o -> do
        wix    <- fromMaybe 0 <$> o .:? "ix"
        nm     <- fromMaybe "" <$> o .:? "name"
        lay    <- fromMaybe "" <$> o .:? "layout"
        act    <- fromMaybe 0 <$> o .:? "active"
        stack  <- o .:? "last_active_stack"
        legacy <- o .:? "last_active"
        auto   <- fromMaybe False <$> o .:? "auto_rename"
        ps     <- fromMaybe [] <$> o .:? "panes"
        pure WindowSnap
            { ix = wix, name = nm, layout = lay, active = act
            , paneHist = fromMaybe (maybeToList legacy) stack, autoRename = auto, panes = ps }

instance ToJSON PaneSnap where
    toJSON p = object $
        ["cwd" .= p.cwd]
        <> maybe [] (\argv -> ["command" .= argv]) p.command
        <> ["shell_spawned" .= True | p.shellSpawned]

instance FromJSON PaneSnap where
    parseJSON = withObject "pane" $ \o -> do
        c       <- fromMaybe "" <$> o .:? "cwd"
        mc      <- o .:? "command"
        shellSp <- fromMaybe False <$> o .:? "shell_spawned"
        pure PaneSnap { cwd = c, command = mc, shellSpawned = shellSp }
