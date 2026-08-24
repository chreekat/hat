-- | Session-tree persistence: capture the live tree into a 'Snapshot',
-- mirror it to the store as it changes, and rebuild it — at startup from
-- the saved tree, or on demand from an archived generation.
module Hat.Server.Snapshot
    ( persistEnabled
    , storePathFor
    , retireStore
    , persistLoop
    , persistDecision
    , PersistDecision (..)
    , StorePin (..)
    , saveNow
    , captureSnapshot
    , captureTree
    , restoreSaved
    , restoreSnapshot
    , snapshotHistoryLimit
    , cmdListSnapshots
    , cmdRestoreSnapshot
    , uniquifySessionNames
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception
    (IOException, SomeException, catch)
import Database.SQLite.Simple (SQLError)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import System.Directory
    (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeFileName)

import Hat.Geometry
import Hat.Log
import Hat.Model
import Hat.Model.Options
import Hat.Path (hatPath, render, (</:>))
import Hat.Server.Persist
    (Archived (..), PaneSnap (..), SessionSnap (..), Snapshot (..)
    , WindowSnap (..), archiveSnapshot, clearLive, listArchived
    , loadArchived, loadSnapshot, saveSnapshot, withStore)
import Hat.Term.Pty qualified
import Hat.Server.Command.Types (CommandImpl, Reply (..))
import Hat.Server.WindowStruct (WindowStruct (..), windowStruct)
import Hat.Server.Pane
import Hat.Server.Rebuild (rebuildSession)

-- Persistence ----------------------------------------------------------

-- | Whether to persist the session tree. On by default; @HAT_PERSIST=0@
-- turns it off (tests set this so each server starts from a clean slate).
persistEnabled :: IO Bool
persistEnabled = (/= Just "0") <$> lookupEnv "HAT_PERSIST"

-- | The SQLite store for a socket: @$HAT_STORE_DIR/<socket>.db@ when that
-- override is set, else @$XDG_DATA_HOME/hat/<socket>.db@ falling back to
-- @~/.local/share@. It lives in a reboot-surviving location (not beside
-- the socket under @/tmp@) and is keyed per socket, so @-L foo@ and
-- @-L bar@ never clobber each other. The directory is created if absent.
storePathFor :: FilePath -> IO FilePath
storePathFor sockPath = do
    dir <- storeDir
    createDirectoryIfMissing True (render dir)
    pure (render (dir </:> (takeFileName sockPath <> ".db")))
  where
    storeDir = lookupEnv "HAT_STORE_DIR" >>= \case
        Just d | not (null d) -> pure (hatPath d)
        _ -> do
            base <- lookupEnv "XDG_DATA_HOME" >>= \case
                Just d | not (null d) -> pure (hatPath d)
                _ -> do
                    home <- fromMaybe "/tmp" <$> lookupEnv "HOME"
                    pure (hatPath home </:> ".local" </:> "share")
            pure (base </:> "hat")

-- | Retire the store at a natural drain: archive the last mirrored tree,
-- then clear the live tables so the next start is pristine while the
-- history stays restorable. Falls back to deleting the store outright if
-- it cannot be cleared (a stale tree must never resurrect); a store that
-- was never created is left absent.
retireStore :: ServerState -> FilePath -> IO ()
retireStore st p = do
    exists <- doesFileExist p
    when exists $
        (do limit <- snapshotHistoryLimit st
            withStore p $ \conn -> do
                archiveSnapshot conn limit =<< loadSnapshot conn
                clearLive conn)
        `catch` \(_ :: SQLError) -> tryRemove
        `catch` \(_ :: IOException) -> tryRemove
  where
    tryRemove = removeFile p `catch` \(_ :: IOException) -> pure ()

-- | Whether the store already holds an explicitly saved final tree.
-- See 'persistDecision'.
data StorePin = Pinned | Unpinned
    deriving (Eq, Show)

-- | The mirror's per-tick verdict on the freshly captured snapshot.
data PersistDecision
    = PinnedSkip     -- ^ store pinned; the last tree is final, never overwrite
    | EmptySkip      -- ^ an empty tree is never mirrored
    | UnchangedSkip  -- ^ identical to the last write, nothing to do
    | WriteSnapshot  -- ^ changed, non-empty, and unpinned: write it
    deriving (Eq, Show)

-- | Decide whether the mirror should write a captured snapshot. A pin
-- (set by @kill-server@) wins over everything: the explicit quit already
-- saved the final tree, so a stray fresh session on the dying server can
-- never clobber it. Otherwise an empty tree is skipped (shutdown, not the
-- mirror, decides an empty store's fate) as is a snapshot unchanged since
-- the last write.
persistDecision :: StorePin -> Maybe Snapshot -> Snapshot -> PersistDecision
persistDecision Pinned _ _ = PinnedSkip
persistDecision Unpinned prev snap
    | null snap.sessions = EmptySkip
    | prev == Just snap  = UnchangedSkip
    | otherwise          = WriteSnapshot

-- | Poll the live tree and write a fresh snapshot whenever it changes.
-- The tree is tiny, so we rewrite it wholesale rather than diffing, and
-- skip writes when nothing changed. A change to a pane's working
-- directory (a bare @cd@, which fires no event) is caught here too. Once
-- the store is pinned by @kill-server@ the loop stops writing for good
-- (see 'persistDecision'), so a fresh session on the dying server cannot
-- overwrite the saved tree.
persistLoop :: ServerState -> FilePath -> IO ()
persistLoop st path = go Nothing
  where
    go prev = do
        threadDelay 2_000_000
        -- Never mirror a tree still being restored or rebuilt: a snapshot of
        -- a half-adopted tree would overwrite the good saved one.
        atomically (readTVar st.startupPhase >>= check . (== Ready))
        snap <- captureSnapshot st
        pinned <- readTVarIO st.preserveStore
        let pin = if pinned then Pinned else Unpinned
        next <- case persistDecision pin prev snap of
            WriteSnapshot -> saveSnapshotNow path snap >> pure (Just snap)
            _             -> pure prev
        go next

-- | Capture and persist immediately. Called at 'cmdKillServer' so an
-- explicit quit never loses a last-moment change. This is the pinning
-- write: it runs after 'preserveStore' is set, directly rather than via
-- 'persistLoop', so the pin never suppresses it. A no-op when persistence
-- is off. An empty tree is never written here: whether an empty store
-- survives shutdown is decided by 'preserveStore' (kill-server keeps the
-- tree; a natural drain deletes the store, see 'runServer').
saveNow :: ServerState -> IO ()
saveNow st = forM_ st.store $ \path -> do
    snap <- captureSnapshot st
    unless (null snap.sessions) (saveSnapshotNow path snap)

-- Best-effort write; persistence must never take down the server, so a store
-- failure (a SQLite error, a lost lock race, a filesystem error) is swallowed
-- rather than raised. Only these synchronous failures are caught: an async
-- exception (the persist daemon being cancelled at shutdown) must pass through,
-- or 'cancel' would hang waiting on a loop that ate its own cancellation.
saveSnapshotNow :: FilePath -> Snapshot -> IO ()
saveSnapshotNow path snap =
    (withStore path $ \conn -> saveSnapshot conn snap)
        `catch` \(_ :: SQLError) -> pure ()
        `catch` \(_ :: IOException) -> pure ()

-- | Read the whole session tree into a pure 'Snapshot': sessions in id
-- order, windows by index, panes in layout order with their live cwd.
captureSnapshot :: ServerState -> IO Snapshot
captureSnapshot = fmap fst . captureTree

-- | 'captureSnapshot', plus the live panes the walk visited — flat, in the
-- snapshot's own pane order. One walk yields both, so a caller that needs
-- per-pane state alongside the tree cannot pair them up wrong.
captureTree :: ServerState -> IO (Snapshot, [Pane])
captureTree st = do
    (sess, laName) <- atomically $ do
        sessMap <- readTVar st.sessions
        laId    <- readTVar st.lastActiveSession
        laName  <- traverse (readTVar . (.name)) (laId >>= (`Map.lookup` sessMap))
        pure (Map.elems sessMap, laName)
    (snaps, panes) <- unzip <$> mapM captureSession sess
    pure (Snapshot snaps laName, concat panes)

captureSession :: Session -> IO (SessionSnap, [Pane])
captureSession s = do
    (nm, cwd, curIx, winHist, wstructs) <- atomically $ do
        nm    <- readTVar s.name
        cwd   <- readTVar s.startCwd
        curIx <- readTVar s.currentIx
        winHist <- readTVar s.windowHist
        eff   <- readTVar s.lastSize
        ws    <- Map.toAscList <$> readTVar s.windows
        wstructs <- mapM (windowStruct eff) ws
        pure (nm, cwd, curIx, winHist, wstructs)
    (wsnaps, panes) <- unzip <$> mapM captureWindow wstructs
    pure ( SessionSnap
             { name = nm, startCwd = T.pack cwd
             , currentIx = curIx, windowHist = winHist, windows = wsnaps }
         , concat panes )

captureWindow :: WindowStruct -> IO (WindowSnap, [Pane])
captureWindow ws = do
    psnaps <- forM ws.wsPanes $ \pane -> do
        dir  <- paneCurrentPath pane
        -- The whole argv, so a restore re-opens the same file; the
        -- whitelist (see 'restoreRun') decides whether it is re-run.
        argv <- Hat.Term.Pty.foregroundArgv pane.pty
        -- Whether that program was a child of the pane's interactive shell,
        -- so a restore can relaunch it through the shell (see 'restoreRun').
        shellSp <- Hat.Term.Pty.foregroundIsChild pane.pty
        pure PaneSnap { cwd = T.pack dir, command = argv, shellSpawned = shellSp }
    pure ( WindowSnap
             { ix = ws.wsIx, name = ws.wsName, layout = ws.wsLayout
             , active = ws.wsActive, paneHist = ws.wsLastActive
             , autoRename = ws.wsAutoRename, panes = psnaps }
         , ws.wsPanes )

-- Persistence restore ----------------------------------------------------

-- | Rebuild any previously-saved session tree. An absent store or a read
-- failure yields an empty snapshot, i.e. a normal fresh start.
restoreSaved :: ServerState -> FilePath -> IO ()
restoreSaved st path = do
    limit <- snapshotHistoryLimit st
    snap <- (withStore path $ \conn -> do
                s <- loadSnapshot conn
                -- Archive the tree the previous run left before this run's
                -- mirror can overwrite it; best-effort, never blocks restore.
                archiveSnapshot conn limit s
                    `catch` \(_ :: SQLError) -> pure ()
                pure s)
        `catch` \(_ :: SomeException) ->
            pure (Snapshot { sessions = [], lastActiveSession = Nothing })
    restoreSnapshot st snap

-- | How many history generations the store keeps: the @\@snapshot-limit@
-- option (≤ 0 turns history off), defaulting to 10. An unparsable value
-- is logged and treated as the default, never silently accepted.
snapshotHistoryLimit :: ServerState -> IO Int
snapshotHistoryLimit st = do
    opts <- readTVarIO st.options
    case Map.lookup "@snapshot-limit" opts.user of
        Nothing -> pure defaultLimit
        Just t -> case TR.signed TR.decimal t of
            Right (n, rest) | T.null rest -> pure n
            _ -> do
                logEvent st.logger DaemonFault
                    { daemon = "persist"
                    , err = "invalid @snapshot-limit: " <> t }
                pure defaultLimit
  where
    defaultLimit = 10

-- | Recreate every session in the snapshot, spawning a fresh shell in
-- each pane's saved working directory and reapplying the saved layout.
restoreSnapshot :: ServerState -> Snapshot -> IO ()
restoreSnapshot st snap = do
    forM_ snap.sessions (restoreSession st)
    -- Point the next attach at the session that was focused before the
    -- restart. Names are the stable key across restart (ids are fresh).
    forM_ snap.lastActiveSession $ \nm -> do
        sessMap <- readTVarIO st.sessions
        hits <- filterM (fmap (== nm) . readTVarIO . (.name)) (Map.elems sessMap)
        forM_ (listToMaybe hits) $ \s ->
            atomically (writeTVar st.lastActiveSession (Just s.id))

restoreSession :: ServerState -> SessionSnap -> IO ()
restoreSession st ssnap = do
    env <- globalSpawnEnv st =<< restoreEnv
    whitelist <- restoreWhitelist st
    let shellCmd = maybe "/bin/sh" T.unpack (List.lookup "SHELL" env)
    rebuildSession st env (restorePane st shellCmd env whitelist) ssnap

restorePane
    :: ServerState -> FilePath -> [(Text, Text)] -> [Text]
    -> SessionId -> Size -> PaneSnap -> IO Pane
restorePane st shellCmd env whitelist sid sz psnap = do
    pid <- PaneId <$> atomically (freshId st.nextPane)
    let origin = if psnap.shellSpawned then ShellSpawned else Direct
    spawnPane st pid sid shellCmd (restoreRun whitelist origin psnap.command)
        (T.unpack psnap.cwd) env sz

-- | @list-snapshots@: the store's history generations, newest first —
-- one line each: generation, capture time, session\/window counts.
cmdListSnapshots :: CommandImpl
cmdListSnapshots st _ args = case (st.store, args) of
    (_, _ : _) -> pure [RErr "usage: list-snapshots"]
    (Nothing, _) -> pure [RErr "list-snapshots: persistence is disabled"]
    (Just path, _) ->
        (map (ROutput . describeArchived) <$> withStore path listArchived)
            `catch` \(e :: SQLError) ->
                pure [RErr ("list-snapshots: " <> tshow e)]
  where
    describeArchived a = tshow a.gen <> ": " <> a.savedAt
        <> " (" <> tshow (length a.snapshot.sessions) <> " sessions, "
        <> tshow (length (concatMap (.windows) a.snapshot.sessions))
        <> " windows)"

-- | @restore-snapshot generation@: recreate the sessions archived as that
-- generation alongside the live tree, renaming any whose name is taken.
cmdRestoreSnapshot :: CommandImpl
cmdRestoreSnapshot st _ args = case args of
    [t] | Right (g, rest) <- TR.decimal t, T.null rest ->
        case st.store of
            Nothing -> pure [RErr "restore-snapshot: persistence is disabled"]
            Just path -> (do
                msnap <- withStore path (\conn -> loadArchived conn g)
                case msnap of
                    Nothing -> pure [RErr ("no such snapshot: " <> t)]
                    Just snap -> restoreArchived st snap)
                `catch` \(e :: SQLError) ->
                    pure [RErr ("restore-snapshot: " <> tshow e)]
    _ -> pure [RErr "usage: restore-snapshot generation"]

-- | Bring an archived tree back next to the live one: each saved session
-- is recreated under a free name ('uniquifySessionNames'), reported one
-- line per session.
restoreArchived :: ServerState -> Snapshot -> IO [Reply]
restoreArchived st snap = do
    taken <- atomically $
        mapM (readTVar . (.name)) . Map.elems =<< readTVar st.sessions
    let olds = map (.name) snap.sessions
        news = uniquifySessionNames taken olds
        rename s n = SessionSnap
            { name = n, startCwd = s.startCwd, currentIx = s.currentIx
            , windowHist = s.windowHist, windows = s.windows }
        renamed = zipWith rename snap.sessions news
    restoreSnapshot st
        Snapshot { sessions = renamed, lastActiveSession = Nothing }
    pure [ ROutput (restoredLine old new) | (old, new) <- zip olds news ]
  where
    restoredLine old new
        | old == new = "restored session '" <> old <> "'"
        | otherwise = "restored session '" <> old <> "' as '" <> new <> "'"

-- | Final names for restored sessions, in order: a saved name that a live
-- session (or an earlier entry) already holds gets the first free @-2@,
-- @-3@, … suffix.
uniquifySessionNames :: [Text] -> [Text] -> [Text]
uniquifySessionNames = go
  where
    go _ [] = []
    go used (n : ns) =
        let n' = freshName used n
        in n' : go (n' : used) ns
    freshName used n
        | n `notElem` used = n
        | otherwise = suffixed used n (2 :: Int)
    suffixed used n k =
        let cand = n <> "-" <> tshow k
        in if cand `elem` used then suffixed used n (k + 1) else cand