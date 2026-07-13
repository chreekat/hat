-- | The on-disk store for continuous session persistence: a pure
-- 'Snapshot' of the session\/window\/pane tree and its SQLite codec.
--
-- The compatibility surface is the schema. Core columns never change
-- meaning; evolving fields live in a per-row @extra@ JSON column, and DDL
-- is additive only ('bootstrap' uses @CREATE TABLE IF NOT EXISTS@). Reads
-- select explicit core columns and default anything missing, so a new
-- binary can read an old store and vice versa.
module Hat.Persist
    ( Snapshot (..)
    , SessionSnap (..)
    , WindowSnap (..)
    , PaneSnap (..)
    , schemaVersion
    , withStore
    , saveSnapshot
    , loadSnapshot
    ) where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple

-- | A point-in-time capture of the whole tree, in restore order:
-- sessions in creation order, each window by its index, each pane in its
-- layout-string order.
newtype Snapshot = Snapshot
    { sessions :: [SessionSnap] }
    deriving (Eq, Show)

data SessionSnap = SessionSnap
    { name      :: Text    -- ^ session name
    , startCwd  :: Text    -- ^ default working directory for new windows
    , currentIx :: Int     -- ^ index of the current window
    , windows   :: [WindowSnap]
    }
    deriving (Eq, Show)

data WindowSnap = WindowSnap
    { ix     :: Int        -- ^ window index within the session (sparse)
    , name   :: Text
    , layout :: Text       -- ^ tmux @window_layout@ string ('Hat.Server.LayoutString.emitLayout')
    , active :: Int        -- ^ ordinal (in 'panes' order) of the active pane
    , panes  :: [PaneSnap] -- ^ in layout order
    }
    deriving (Eq, Show)

newtype PaneSnap = PaneSnap
    { cwd :: Text }        -- ^ the pane's working directory at capture
    deriving (Eq, Show)

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
    mapM_ insertSession (zip [0 ..] snap.sessions)
  where
    insertSession :: (Int, SessionSnap) -> IO ()
    insertSession (sseq, s) = do
        execute conn
            "INSERT INTO session (seq, name, start_cwd, current_ix) \
            \VALUES (?, ?, ?, ?)"
            (sseq, s.name, s.startCwd, s.currentIx)
        mapM_ (insertWindow sseq) s.windows
    insertWindow :: Int -> WindowSnap -> IO ()
    insertWindow sseq w = do
        execute conn
            "INSERT INTO window (session_seq, ix, name, layout, active) \
            \VALUES (?, ?, ?, ?, ?)"
            (sseq, w.ix, w.name, w.layout, w.active)
        mapM_ (insertPane sseq w.ix) (zip [0 ..] w.panes)
    insertPane :: Int -> Int -> (Int, PaneSnap) -> IO ()
    insertPane sseq wix (ord, p) =
        execute conn
            "INSERT INTO pane (session_seq, window_ix, ordinal, cwd) \
            \VALUES (?, ?, ?, ?)"
            (sseq, wix, ord, p.cwd)

-- | Read the whole tree back. Returns an empty snapshot for a fresh store.
loadSnapshot :: Connection -> IO Snapshot
loadSnapshot conn = do
    srows <- query_ conn
        "SELECT seq, name, start_cwd, current_ix FROM session ORDER BY seq"
        :: IO [(Int, Text, Text, Int)]
    Snapshot <$> mapM loadSession srows
  where
    loadSession :: (Int, Text, Text, Int) -> IO SessionSnap
    loadSession (sseq, nm, cwd0, curIx) = do
        wrows <- query conn
            "SELECT ix, name, layout, active FROM window \
            \WHERE session_seq = ? ORDER BY ix"
            (Only sseq) :: IO [(Int, Text, Text, Int)]
        ws <- mapM (loadWindow sseq) wrows
        pure SessionSnap
            { name = nm, startCwd = cwd0, currentIx = curIx, windows = ws }
    loadWindow :: Int -> (Int, Text, Text, Int) -> IO WindowSnap
    loadWindow sseq (wix, nm, lay, act) = do
        prows <- query conn
            "SELECT cwd FROM pane \
            \WHERE session_seq = ? AND window_ix = ? ORDER BY ordinal"
            (sseq, wix) :: IO [Only Text]
        pure WindowSnap
            { ix = wix, name = nm, layout = lay, active = act
            , panes = [ PaneSnap { cwd = c } | Only c <- prows ] }
