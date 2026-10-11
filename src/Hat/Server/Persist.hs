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
    , ModalSnap (..)
    , PaneSnap (..)
    , plainPane
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
    , encodeSnapshotJson
    , decodeSnapshotJson
    ) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson
    ( FromJSON (..), Key, Object, ToJSON (..), Value (String), decode, encode
    , object, withObject, (.:?), (.=) )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe, maybeToList)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Database.SQLite.Simple

import Hat.Model.PaneKind (PaneKind (..))

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
    , createdAt :: Maybe POSIXTime
                           -- ^ when the session was first created, at epoch-
                           --   second granularity ('Nothing' in a capture that
                           --   predates the field); carried in the session
                           --   row's @extra@ JSON.
    , restores  :: Int     -- ^ times the session was rebuilt from serialized
                           --   state; carried in the session row's @extra@
                           --   JSON. See 'Hat.Server.Rebuild.rebuildSession'.
    , group     :: Maybe Text
                           -- ^ the session group's name; carried in the
                           --   session row's @extra@ JSON. See Note [Captured
                           --   links].
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
    , key        :: Maybe Int
                           -- ^ the window's identity within this capture, for
                           --   a later 'linkOf' to name; carried in the window
                           --   row's @extra@ JSON.
    , linkOf     :: Maybe Int
                           -- ^ the 'key' of an earlier-captured window this
                           --   entry links rather than carries; carried in the
                           --   window row's @extra@ JSON. See Note [Captured
                           --   links].
    , modal      :: Maybe ModalSnap
                           -- ^ the window's modal pane; carried in the window
                           --   row's @extra@ JSON. Its floating panes ride in
                           --   'layout' ('Hat.Server.LayoutString.emitLayoutZ').
    , panes      :: [PaneSnap] -- ^ in layout order
    }
    deriving (Eq, Show)

-- | A window's modal pane ('Hat.Server.Floating.Modal'), its panes named by
-- ordinal in 'panes' order.
data ModalSnap = ModalSnap
    { pane     :: Int
    , returnTo :: Maybe Int
    , rezoom   :: Bool
    , closeOnClick :: Bool  -- ^ a click outside closes it (@new-pane -C@)
    }
    deriving (Eq, Show)

instance ToJSON ModalSnap where
    toJSON m = object $
        ["pane" .= m.pane]
        <> ["return_to" .= r | Just r <- [m.returnTo]]
        <> ["rezoom" .= True | m.rezoom]
        <> ["close_on_click" .= True | m.closeOnClick]

instance FromJSON ModalSnap where
    parseJSON = withObject "modal" $ \o -> do
        p <- fromMaybe 0 <$> o .:? "pane"
        r <- o .:? "return_to"
        z <- fromMaybe False <$> o .:? "rezoom"
        c <- fromMaybe False <$> o .:? "close_on_click"
        pure ModalSnap { pane = p, returnTo = r, rezoom = z, closeOnClick = c }

-- Note [Captured links]
-- ~~~~~~~~~~~~~~~~~~~~~
-- A window linked into several sessions, and the window map a session group
-- shares, are captured once: the first session (in capture order) to hold a
-- window carries it whole, with its 'key'; a later link of it is a pane-less
-- entry naming that key in 'linkOf'; and a group member after the first
-- carries no windows at all. A rebuild walks the capture in order, so every
-- link and member finds what it shares already rebuilt. Each pane is thus
-- captured once, which the reload handover's pane-for-pane pairing relies on.
-- A reader predating links skips pane-less entries and window-less sessions:
-- it loses the extra links and members, but never adopts a pane twice.

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
    , kind     :: PaneKind      -- ^ an empty pane runs nothing; carried in
                               --   the row's @extra@ JSON, absent meaning a
                               --   program pane
    , inputOff :: Bool          -- ^ @select-pane -d@ was in effect; @extra@
                               --   JSON, absent meaning 'False'
    , title    :: Maybe Text    -- ^ the pane title, when one was set; @extra@
                               --   JSON
    }
    deriving (Eq, Show)

-- | The session row's @extra@ JSON payload: the MRU window indices (head
-- first), the creation stamp, and the restore count. Evolving, optional
-- fields live here rather than in core columns, so old and new binaries
-- interoperate; a store written before a key existed omits it and the
-- reader defaults it.
data SessionExtra = SessionExtra [Int] (Maybe POSIXTime) Int (Maybe Text)

-- @last_ix@ mirrors the head for readers predating the stack; @last_stack@
-- carries the whole history. A reader prefers the stack, else lifts the head.
instance ToJSON SessionExtra where
    toJSON (SessionExtra hist created rest grp) = object $
        ["last_ix" .= h | h <- take 1 hist]
        ++ ["last_stack" .= hist | not (null hist)]
        ++ ["created_at" .= (floor c :: Integer) | Just c <- [created]]
        ++ ["restores" .= rest | rest /= 0]
        ++ ["group" .= g | Just g <- [grp]]

instance FromJSON SessionExtra where
    parseJSON = withObject "session extra" $ \o -> do
        stack  <- o .:? "last_stack"
        legacy <- o .:? "last_ix"
        created <- fmap fromInteger <$> o .:? "created_at"
        rest   <- fromMaybe 0 <$> o .:? "restores"
        grp    <- o .:? "group"
        pure (SessionExtra (fromMaybe (maybeToList legacy) stack) created rest grp)

encodeSessionExtra :: SessionSnap -> Text
encodeSessionExtra s = TE.decodeUtf8 (BL.toStrict (encode
    (SessionExtra s.windowHist s.createdAt s.restores s.group)))

decodeSessionExtra :: Text -> SessionExtra
decodeSessionExtra t = fromMaybe (SessionExtra [] Nothing 0 Nothing)
    (decode (BL.fromStrict (TE.encodeUtf8 t)))

-- | The window row's @extra@ JSON payload: the last-active pane ordinal and
-- the automatic-rename flag. A store written before @auto_rename@ existed
-- omits the key; it defaults to 'False', matching the old restore behavior
-- of pinning a restored window's name.
data WindowExtra = WindowExtra [Int] Bool (Maybe Int) (Maybe Int) (Maybe ModalSnap)
    -- ^ MRU pane ordinals (head first), auto-rename, key, link, modal pane

-- @last_active@ mirrors the head for readers predating the stack;
-- @last_active_stack@ carries the whole history. See 'SessionExtra'.
instance ToJSON WindowExtra where
    toJSON (WindowExtra hist auto k l m) =
        object (["last_active" .= h | h <- take 1 hist]
                ++ ["last_active_stack" .= hist | not (null hist)]
                ++ ["auto_rename" .= auto | auto]
                ++ ["key" .= v | Just v <- [k]]
                ++ ["link_of" .= v | Just v <- [l]]
                ++ ["modal" .= v | Just v <- [m]])

instance FromJSON WindowExtra where
    parseJSON = withObject "window extra" $ \o -> do
        stack  <- o .:? "last_active_stack"
        legacy <- o .:? "last_active"
        auto   <- fromMaybe False <$> o .:? "auto_rename"
        k      <- o .:? "key"
        l      <- o .:? "link_of"
        m      <- o .:? "modal"
        pure (WindowExtra (fromMaybe (maybeToList legacy) stack) auto k l m)

encodeWindowExtra :: WindowSnap -> Text
encodeWindowExtra w = TE.decodeUtf8 (BL.toStrict (encode
    (WindowExtra w.paneHist w.autoRename w.key w.linkOf w.modal)))

decodeWindowExtra :: Text -> WindowExtra
decodeWindowExtra t = fromMaybe (WindowExtra [] False Nothing Nothing Nothing)
    (decode (BL.fromStrict (TE.encodeUtf8 t)))

-- | The pane row's @extra@ JSON payload: every 'PaneSnap' field but the
-- @cwd@ core column, as the snapshot JSON writes them ('paneFields'). Evolving,
-- optional fields live here rather than in core columns, so old and new
-- binaries interoperate; a key a store predates defaults ('parsePaneFields').
encodeExtra :: PaneSnap -> Text
encodeExtra p = TE.decodeUtf8 (BL.toStrict (encode (object (paneFields p))))

decodeExtra :: Text -> Text -> PaneSnap
decodeExtra c t = fromMaybe (plainPane c) $ do
    v <- decode (BL.fromStrict (TE.encodeUtf8 t))
    parseMaybe (withObject "pane extra" (parsePaneFields c)) v

-- | A pane snapshot with every evolving field at its default.
plainPane :: Text -> PaneSnap
plainPane c = PaneSnap
    { cwd = c, command = Nothing, shellSpawned = False, kind = ProgramPane
    , inputOff = False, title = Nothing }

-- | The evolving pane fields, as JSON keys; a default is left out.
paneFields :: PaneSnap -> [(Key, Value)]
paneFields p =
    maybe [] (\argv -> ["command" .= argv]) p.command
    <> ["shell_spawned" .= True | p.shellSpawned]
    <> ["empty" .= True | p.kind == EmptyPane]
    <> ["input_off" .= True | p.inputOff]
    <> maybe [] (\t -> ["title" .= t]) p.title

-- | Read 'paneFields' back, defaulting any key a writer predates. A command
-- may be a bare string, as a store written by an older binary that persisted
-- only the program name has it.
parsePaneFields :: Text -> Object -> Parser PaneSnap
parsePaneFields c o = do
    mv <- o .:? "command"
    mc <- traverse parseArgv mv
    shellSp <- fromMaybe False <$> o .:? "shell_spawned"
    emptyP <- fromMaybe False <$> o .:? "empty"
    off <- fromMaybe False <$> o .:? "input_off"
    t <- o .:? "title"
    pure PaneSnap
        { cwd = c, command = mc, shellSpawned = shellSp
        , kind = if emptyP then EmptyPane else ProgramPane
        , inputOff = off, title = t }
  where
    parseArgv (String s) = pure [s]
    parseArgv v          = parseJSON v

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
            (sseq, s.name, s.startCwd, s.currentIx, encodeSessionExtra s)
        mapM_ (insertWindow sseq) s.windows
    insertWindow :: Int -> WindowSnap -> IO ()
    insertWindow sseq w = do
        execute conn
            "INSERT INTO window (session_seq, ix, name, layout, active, extra) \
            \VALUES (?, ?, ?, ?, ?, ?)"
            (sseq, w.ix, w.name, w.layout, w.active, encodeWindowExtra w)
        mapM_ (insertPane sseq w.ix) (zip [0 ..] w.panes)
    insertPane :: Int -> Int -> (Int, PaneSnap) -> IO ()
    insertPane sseq wix (ord, p) =
        execute conn
            "INSERT INTO pane (session_seq, window_ix, ordinal, cwd, extra) \
            \VALUES (?, ?, ?, ?, ?)"
            (sseq, wix, ord, p.cwd, encodeExtra p)

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
        let SessionExtra hist created rest grp = decodeSessionExtra sex
        pure SessionSnap
            { name = nm, startCwd = cwd0, currentIx = curIx
            , windowHist = hist, createdAt = created, restores = rest
            , group = grp, windows = ws }
    loadWindow :: Int -> (Int, Text, Text, Int, Text) -> IO WindowSnap
    loadWindow sseq (wix, nm, lay, act, wex) = do
        prows <- query conn
            "SELECT cwd, extra FROM pane \
            \WHERE session_seq = ? AND window_ix = ? ORDER BY ordinal"
            (sseq, wix) :: IO [(Text, Text)]
        let WindowExtra paneOrds auto k l m = decodeWindowExtra wex
        pure WindowSnap
            { ix = wix, name = nm, layout = lay, active = act
            , paneHist = paneOrds, autoRename = auto, key = k, linkOf = l, modal = m
            , panes = [ decodeExtra c ex | (c, ex) <- prows ] }

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

-- | The whole tree as one tolerant JSON document, evolved the same way as
-- the @extra@ columns — append optional keys, default the absent, ignore
-- the unknown. Instances are written by hand (explicit keys, never derived
-- layout), so no field's meaning rides on record order.
encodeSnapshotJson :: Snapshot -> Text
encodeSnapshotJson = TE.decodeUtf8 . BL.toStrict . encode

-- | Read back a tree written by 'encodeSnapshotJson', at any vintage;
-- 'Nothing' only for text that is not a tree document at all.
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
        <> ["created_at" .= (floor c :: Integer) | Just c <- [s.createdAt]]
        <> ["restores" .= s.restores | s.restores /= 0]
        <> ["group" .= g | Just g <- [s.group]]
        <> ["windows" .= s.windows]

instance FromJSON SessionSnap where
    parseJSON = withObject "session" $ \o -> do
        nm     <- fromMaybe "" <$> o .:? "name"
        cwd0   <- fromMaybe "" <$> o .:? "start_cwd"
        curIx  <- fromMaybe 0 <$> o .:? "current_ix"
        stack  <- o .:? "last_stack"
        legacy <- o .:? "last_ix"
        created <- fmap fromInteger <$> o .:? "created_at"
        rest   <- fromMaybe 0 <$> o .:? "restores"
        grp    <- o .:? "group"
        ws     <- fromMaybe [] <$> o .:? "windows"
        pure SessionSnap
            { name = nm, startCwd = cwd0, currentIx = curIx
            , windowHist = fromMaybe (maybeToList legacy) stack
            , createdAt = created, restores = rest, group = grp
            , windows = ws }

instance ToJSON WindowSnap where
    toJSON w = object $
        [ "ix" .= w.ix, "name" .= w.name, "layout" .= w.layout
        , "active" .= w.active ]
        <> ["last_active" .= h | h <- take 1 w.paneHist]
        <> ["last_active_stack" .= w.paneHist | not (null w.paneHist)]
        <> ["auto_rename" .= True | w.autoRename]
        <> ["key" .= k | Just k <- [w.key]]
        <> ["link_of" .= l | Just l <- [w.linkOf]]
        <> ["modal" .= m | Just m <- [w.modal]]
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
        k      <- o .:? "key"
        l      <- o .:? "link_of"
        m      <- o .:? "modal"
        ps     <- fromMaybe [] <$> o .:? "panes"
        pure WindowSnap
            { ix = wix, name = nm, layout = lay, active = act
            , paneHist = fromMaybe (maybeToList legacy) stack, autoRename = auto
            , key = k, linkOf = l, modal = m, panes = ps }

instance ToJSON PaneSnap where
    toJSON p = object (("cwd" .= p.cwd) : paneFields p)

instance FromJSON PaneSnap where
    parseJSON = withObject "pane" $ \o -> do
        c <- fromMaybe "" <$> o .:? "cwd"
        parsePaneFields c o
