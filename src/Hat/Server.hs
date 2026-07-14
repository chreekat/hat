-- | The server: owns PTYs, emulators, and the state tree; accepts
-- clients, streams frame diffs at them, and runs the command engine
-- that configs, bindings, and @hat <command>@ all share.
module Hat.Server
    ( runServer
    , setOption  -- ^ exported for the config-load burn-down test
    , send       -- ^ exported for the greeting-ordering test
    , finallyClearRestoring  -- ^ exported for the restore-gate test
    , readConfigUtf8  -- ^ exported for the config-encoding test
    , cmdAttachSession  -- ^ exported for the session re-anchor test
    , PaneStart (..)  -- ^ exported for the restore-argv test
    , restoreRun      -- ^ exported for the restore-argv test
    , chooseCurrentOnClose  -- ^ exported for the close-to-last-window test
    ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (race, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
    (IOException, SomeException, bracket, catch, finally, try)
import Control.Monad (foldM, forM, forM_, forever, unless, void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (getZonedTime)
import qualified Data.Vector as V
import qualified Network.Socket as N
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..), exitSuccess)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (Handle, SeekMode (AbsoluteSeek), hClose, hFlush)
import qualified System.Posix.Files as PFiles
import qualified System.Posix.IO as PIO
import System.Posix.Process (getProcessID)
import System.Posix.Unistd (SystemID (nodeName), getSystemID)
import System.Process
    (CreateProcess (..), StdStream (..), createProcess, proc,
     readCreateProcess, readCreateProcessWithExitCode, shell,
     terminateProcess, waitForProcess, withCreateProcess)

import Hat.Command.Parser (parseCommandLine, parseConfig)
import Hat.Geometry
import Hat.Log
import Hat.Model
import Hat.Model.Options
import Hat.Persist
    (PaneSnap (..), SessionSnap (..), Snapshot (..), WindowSnap (..)
    , loadSnapshot, saveSnapshot, withStore)
import qualified Hat.Pty
import qualified Hat.Server.CopyMode as CopyMode
import Hat.Server.ColorScheme
    (ColorScheme (..), applyPalette, parseSchemeLine, schemeName)
import Hat.Server.Format (FormatEnv, renderFormat)
import Hat.Server.Keys
import Hat.Server.Layout
import Hat.Server.LayoutString (emitLayout, layoutFromString)
import qualified Hat.Server.Picker as Picker
import qualified Hat.Server.Prompt as Prompt
import Hat.Server.Render
import Hat.Server.Style (parseStyle)
import Hat.Server.Target (PaneTarget (..), parsePaneTarget)
import Hat.Server.Title (TitleParts (..), composeTitle)
import Hat.Socket (ensureSocketDir, listenOn)
import qualified Hat.Term.Cell as Cell
import qualified Hat.Term.Emulator as Emu
import Hat.Wire

runServer :: FilePath -> Maybe FilePath -> IO ()
runServer path mconfig = do
    -- The lock and log files live next to the socket; the directory
    -- must exist before any of them are touched.
    ensureSocketDir path
    locked <- acquireLock (path <> ".lock")
    unless locked exitSuccess  -- another server won the race
    lg <- newLogger (takeDirectory path <> "/server.log")
    persistOn <- persistEnabled
    mstore <- if persistOn then Just <$> storePathFor path else pure Nothing
    st <- newServerState defaultKeymap lg path mstore
    bracket (listenOn path) N.close $ \lsock -> do
        logEvent lg ServerStarted { socket = path }
        -- Load the config in a background thread so shell conditions
        -- like `if '$TMUX run ...' ...` can reach the accept loop while
        -- the config is still running. configLoading suppresses the
        -- empty-idle exit until the config has drained.
        atomically $ do
            writeTVar st.configLoading True
            -- Armed before the accept loop can serve, so a client that
            -- autostarts us and attaches waits for the restore to finish
            -- (see 'ensureSession') and joins the restored tree.
            when persistOn (writeTVar st.restoring True)
        _ <- forkIO $ do
            finallyClearRestoring st $
                (loadConfig st mconfig >> forM_ mstore (restoreSaved st))
                    `catch` \(e :: SomeException) ->
                        logEvent lg ServerCrash
                            { err = "startup restore failed: " <> T.pack (show e) }
            -- No fixed grace: the idle-exit now waits on 'served' (a real
            -- connection), so the autostarting client is always counted
            -- before we can drain, whatever the config-load timing.
            atomically (writeTVar st.configLoading False)
        -- Keep status-line clocks fresh.
        _ <- forkIO $ forever $ do
            threadDelay 15_000_000
            atomically (bumpDirty st)
        -- Track foreground commands for automatic-rename windows and the
        -- clients' desktop titles.
        titlesRef <- newIORef Map.empty
        _ <- forkIO $ forever $ do
            threadDelay 500_000
            refreshAutoNames st
            refreshTitles st titlesRef
        -- Continuously mirror the session tree into the SQLite store so a
        -- restart can rebuild it (see 'persistLoop').
        mpersist <- forM mstore $ \p -> forkIO (persistLoop st p)
        -- Follow the desktop light/dark preference (waits out the config
        -- load internally). Killed below so its monitor subprocess dies
        -- with the server rather than lingering as an orphan.
        schemeTid <- forkIO (watchColorScheme st)
        r <- race (acceptLoop st lsock) (waitIdle st)
        killThread schemeTid
        case r of
            Left () -> pure ()
            Right () -> do
                logEvent lg ServerStopping { reason = "no sessions left" }
                -- Stop the mirror first so no in-flight write can recreate
                -- the store after we drop it. The tree drained (every
                -- window closed), so the next start must be pristine —
                -- unless kill-server asked to keep the tree for a restore.
                forM_ mpersist killThread
                preserve <- readTVarIO st.preserveStore
                unless preserve $ forM_ mstore $ \p ->
                    removeFile p `catch` \(_ :: IOException) -> pure ()
                removeFile path `catch` \(_ :: IOException) -> pure ()

-- | Run a startup action (config load + restore), then clear the
-- @restoring@ gate — always, even if it throws. A gate left set parks
-- every attach forever on 'ensureSession'\'s retry, so the clear must be
-- structural (a @finally@), never a line a crash can skip.
finallyClearRestoring :: ServerState -> IO a -> IO a
finallyClearRestoring st act =
    act `finally` atomically (writeTVar st.restoring False)

-- flock-style: O_CREAT + posix write lock, held for the server's life.
acquireLock :: FilePath -> IO Bool
acquireLock lockPath = do
    fd <- PIO.createFile lockPath 0o600
    r <- try $ PIO.setLock fd (PIO.WriteLock, AbsoluteSeek, 0, 0)
    pure $ case r of
        Left (_ :: IOException) -> False
        Right () -> True

-- Exit once every session is gone AND every attached client has
-- drained and disconnected, so nobody's final Exited message is cut off.
waitIdle :: ServerState -> IO ()
waitIdle st = atomically $ do
    armed <- readTVar st.everAttached
    served <- readTVar st.served
    loading <- readTVar st.configLoading
    sess <- readTVar st.sessions
    cs <- readTVar st.clients
    check (armed && served && not loading && Map.null sess && Map.null cs)

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
    createDirectoryIfMissing True dir
    pure (dir </> (takeFileName sockPath <> ".db"))
  where
    storeDir = lookupEnv "HAT_STORE_DIR" >>= \case
        Just d | not (null d) -> pure d
        _ -> do
            base <- lookupEnv "XDG_DATA_HOME" >>= \case
                Just d | not (null d) -> pure d
                _ -> do
                    home <- fromMaybe "/tmp" <$> lookupEnv "HOME"
                    pure (home </> ".local" </> "share")
            pure (base </> "hat")

-- | Poll the live tree and write a fresh snapshot whenever it changes.
-- The tree is tiny, so we rewrite it wholesale rather than diffing, and
-- skip writes when nothing changed. A change to a pane's working
-- directory (a bare @cd@, which fires no event) is caught here too.
persistLoop :: ServerState -> FilePath -> IO ()
persistLoop st path = go Nothing
  where
    go prev = do
        threadDelay 2_000_000
        snap <- captureSnapshot st
        next <- if not (null snap.sessions) && prev /= Just snap
            then saveSnapshotNow path snap >> pure (Just snap)
            else pure prev
        go next

-- | Capture and persist immediately. Called at 'cmdKillServer' so an
-- explicit quit never loses a last-moment change. A no-op when
-- persistence is off. An empty tree is never written here: whether an
-- empty store survives shutdown is decided by 'preserveStore' (kill-server
-- keeps the tree; a natural drain deletes the store, see 'runServer').
saveNow :: ServerState -> IO ()
saveNow st = forM_ st.store $ \path -> do
    snap <- captureSnapshot st
    unless (null snap.sessions) (saveSnapshotNow path snap)

-- Best-effort write; persistence must never take down the server, so any
-- store failure (I/O, a lost lock race) is swallowed rather than raised.
saveSnapshotNow :: FilePath -> Snapshot -> IO ()
saveSnapshotNow path snap =
    (withStore path $ \conn -> saveSnapshot conn snap)
        `catch` \(_ :: SomeException) -> pure ()

-- | Read the whole session tree into a pure 'Snapshot': sessions in id
-- order, windows by index, panes in layout order with their live cwd.
captureSnapshot :: ServerState -> IO Snapshot
captureSnapshot st = do
    sess <- Map.elems <$> readTVarIO st.sessions
    Snapshot <$> mapM captureSession sess

captureSession :: Session -> IO SessionSnap
captureSession s = do
    nm    <- readTVarIO s.name
    cwd   <- readTVarIO s.startCwd
    curIx <- readTVarIO s.currentIx
    eff   <- readTVarIO s.lastSize
    ws    <- Map.toAscList <$> readTVarIO s.windows
    wsnaps <- mapM (captureWindow eff) ws
    pure SessionSnap
        { name = nm, startCwd = T.pack cwd
        , currentIx = curIx, windows = wsnaps }

captureWindow :: Size -> (Int, Window) -> IO WindowSnap
captureWindow eff (wix, w) = do
    nm       <- readTVarIO w.name
    lay      <- readTVarIO w.layout
    activeId <- readTVarIO w.activeId
    paneMap  <- readTVarIO w.panes
    let order = layoutPanes lay
        activeOrd = fromMaybe 0 (List.elemIndex activeId order)
    psnaps <- fmap catMaybes . forM order $ \pid ->
        forM (Map.lookup pid paneMap) $ \pane -> do
            dir  <- paneCurrentPath pane
            -- The whole argv, so a restore re-opens the same file; the
            -- whitelist (see 'restoreRun') decides whether it is re-run.
            argv <- Hat.Pty.foregroundArgv pane.pty
            pure PaneSnap { cwd = T.pack dir, command = argv }
    pure WindowSnap
        { ix = wix, name = nm
        , layout = emitLayout (sizeRect (windowArea eff)) lay
        , active = activeOrd, panes = psnaps }

-- Color scheme -----------------------------------------------------------

-- | Follow the desktop's light\/dark preference: read it once, then tail
-- @gsettings monitor@ for changes. Runs after the config has loaded so
-- the @\@color-scheme-*@ options are set before the initial apply. On a
-- host without gsettings (or outside a desktop session) the first call
-- fails and the feature stays inert. Killed at server shutdown, which
-- also terminates the monitor subprocess (withCreateProcess's cleanup).
watchColorScheme :: ServerState -> IO ()
watchColorScheme st = do
    atomically (readTVar st.configLoading >>= check . not)
    r <- try $ do
        out <- readCreateProcess
            (proc "gsettings" ["get", schemaKey, key]) { close_fds = True } ""
        forM_ (parseSchemeLine (T.strip (T.pack out))) (applyScheme st)
        withCreateProcess
            (proc "gsettings" ["monitor", schemaKey, key])
                { std_out = CreatePipe, close_fds = True } $ \_ mout _ _ ->
            forM_ mout $ \h -> forever $ do
                line <- TIO.hGetLine h
                forM_ (parseSchemeLine line) (applyScheme st)
    case r of
        Left (_ :: SomeException) -> pure ()
        Right () -> pure ()
  where
    schemaKey = "org.gnome.desktop.interface"
    key = "color-scheme"

-- | Record a (possibly unchanged) scheme; on a change, source the
-- config file the user pointed at it (@set -g \@color-scheme-dark
-- \<file\>@, likewise @-light@) and redraw.
applyScheme :: ServerState -> ColorScheme -> IO ()
applyScheme st scheme = do
    old <- atomically $ swapTVar st.colorScheme (Just scheme)
    unless (old == Just scheme) $ do
        -- Default chrome first (skips user-set options), then the user's
        -- per-scheme config on top.
        atomically $ modifyTVar' st.options (applyPalette scheme)
        opts <- readTVarIO st.options
        let optName = case scheme of
                SchemeDark -> "@color-scheme-dark"
                SchemeLight -> "@color-scheme-light"
        forM_ (Map.lookup optName opts.user) $ \path ->
            unless (T.null (T.strip path)) $
                void $ runArgv st Nothing ["source-file", T.strip path]
        atomically (bumpDirty st)

-- Persistence restore ----------------------------------------------------

-- | Rebuild any previously-saved session tree. An absent store or a read
-- failure yields an empty snapshot, i.e. a normal fresh start.
restoreSaved :: ServerState -> FilePath -> IO ()
restoreSaved st path = do
    snap <- withStore path loadSnapshot
        `catch` \(_ :: SomeException) -> pure (Snapshot { sessions = [] })
    restoreSnapshot st snap

-- | Recreate every session in the snapshot, spawning a fresh shell in
-- each pane's saved working directory and reapplying the saved layout.
restoreSnapshot :: ServerState -> Snapshot -> IO ()
restoreSnapshot st snap = forM_ snap.sessions (restoreSession st)

restoreSession :: ServerState -> SessionSnap -> IO ()
restoreSession st ssnap = do
    let wins = filter (not . null . (.panes)) ssnap.windows
    unless (null wins) $ do
        sid <- SessionId <$> atomically (freshId st.nextSession)
        env <- restoreEnv
        whitelist <- restoreWhitelist st
        let shellCmd = maybe "/bin/sh" T.unpack (List.lookup "SHELL" env)
            sz = Size { rows = 24, cols = 80 }  -- resized on client attach
        built <- forM wins $ \wsnap -> do
            (win, panes) <- restoreWindow st sid shellCmd env sz whitelist wsnap
            pure (wsnap.ix, win, panes)
        let winMap = Map.fromList [(wix, win) | (wix, win, _) <- built]
            curIx | Map.member ssnap.currentIx winMap = ssnap.currentIx
                  | otherwise = maybe ssnap.currentIx fst (Map.lookupMin winMap)
        nameVar    <- newTVarIO ssnap.name
        windowsVar <- newTVarIO winMap
        currentVar <- newTVarIO curIx
        lastVar    <- newTVarIO Nothing
        sizeVar    <- newTVarIO sz
        environVar <- newTVarIO env
        cwdVar     <- newTVarIO (T.unpack ssnap.startCwd)
        let sess = Session
                { id = sid, name = nameVar, windows = windowsVar
                , currentIx = currentVar, lastIx = lastVar
                , lastSize = sizeVar, environ = environVar
                , startCwd = cwdVar }
        atomically $ modifyTVar' st.sessions (Map.insert sid sess)
        forM_ built $ \(_, win, panes) ->
            forM_ panes (startPaneReader st sid win)

restoreWindow
    :: ServerState -> SessionId -> FilePath -> [(Text, Text)] -> Size
    -> [Text] -> WindowSnap -> IO (Window, [Pane])
restoreWindow st sid shellCmd env sz whitelist wsnap = do
    wid <- WindowId <$> atomically (freshId st.nextWindow)
    panes <- forM wsnap.panes $ restorePane st sid shellCmd env sz whitelist
    let pids = map (.id) panes
        paneMap = Map.fromList [(p.id, p) | p <- panes]
        -- Our own emitted string round-trips; the named layout is only a
        -- fallback for a corrupt string, and still contains every pane.
        lay = fromMaybe (namedLayout EvenHorizontal (1 % 2) pids)
                        (layoutFromString wsnap.layout pids)
        activePid = pids !! max 0 (min (length pids - 1) wsnap.active)
    nameVar       <- newTVarIO wsnap.name
    layoutVar     <- newTVarIO lay
    panesVar      <- newTVarIO paneMap
    activeVar     <- newTVarIO activePid
    lastActiveVar <- newTVarIO Nothing
    bellVar       <- newTVarIO False
    activityVar   <- newTVarIO False
    zoomVar       <- newTVarIO Nothing
    -- The restored name is explicit; don't let auto-rename clobber it.
    autoRenameVar <- newTVarIO False
    let win = Window
            { id = wid, name = nameVar, layout = layoutVar
            , panes = panesVar, activeId = activeVar
            , lastActive = lastActiveVar, bellFlag = bellVar
            , activity = activityVar, zoomed = zoomVar
            , autoRename = autoRenameVar }
    pure (win, panes)

restorePane
    :: ServerState -> SessionId -> FilePath -> [(Text, Text)] -> Size
    -> [Text] -> PaneSnap -> IO Pane
restorePane st sid shellCmd env sz whitelist psnap = do
    pid <- PaneId <$> atomically (freshId st.nextPane)
    spawnPane st pid sid shellCmd (restoreRun whitelist psnap.command)
        (T.unpack psnap.cwd) env sz

-- The server's own environment seeds restored panes; spawnPane strips and
-- re-adds the hat-specific vars (TERM, TMUX, HAT, …).
restoreEnv :: IO [(Text, Text)]
restoreEnv = map (\(k, v) -> (T.pack k, T.pack v)) <$> getEnvironment

-- | Commands worth re-running when a pane is restored, rather than
-- dropping to a fresh shell. Overridable via the @\@restore-commands@
-- user option (a space-separated list).
defaultRestoreCommands :: [Text]
defaultRestoreCommands =
    [ "vim", "nvim", "vi", "view", "emacs", "nano"
    , "less", "man", "tail", "watch"
    , "top", "htop", "atop", "btop" ]

restoreWhitelist :: ServerState -> IO [Text]
restoreWhitelist st = do
    opts <- readTVarIO st.options
    pure $ case Map.lookup "@restore-commands" opts.user of
        Just v | not (T.null (T.strip v)) -> T.words v
        _                                 -> defaultRestoreCommands

-- | Re-exec the captured argv only when its program is whitelisted;
-- otherwise the pane comes back as a plain shell. Exec'ing the argv
-- directly (never through a shell) is what keeps an argument with spaces —
-- @vim "Foo Bar.txt"@ — from being re-split on restore.
restoreRun :: [Text] -> Maybe [Text] -> PaneStart
restoreRun whitelist mcmd = case mcmd of
    Just argv@(prog : _) | commandName prog `elem` whitelist -> ExecArgv argv
    _                                                        -> FreshShell

-- | The program a captured foreground command names: its last path
-- segment with NixOS's @.<name>-wrapped@ decoration stripped, so a pane
-- running @\/nix\/store\/…\/.vim-wrapped@ is recorded (and matched) as
-- @vim@.
commandName :: Text -> Text
commandName raw =
    let base  = T.takeWhileEnd (/= '/') (firstWord raw)
        undot = T.dropWhile (== '.') base
    in fromMaybe undot (T.stripSuffix "-wrapped" undot)
  where
    firstWord t = case T.words t of
        (w : _) -> w
        []      -> ""

-- Configuration --------------------------------------------------------

defaultKeymap :: Keymap
defaultKeymap = Map.fromList
    [ ("prefix", Map.fromList (map bindArgv prefixBindings))
    , ("root", Map.empty)
    , ("copy-mode", Map.fromList (map copyBind copyModeBindings))
    , ("copy-mode-vi", Map.fromList (map copyBind copyModeViBindings <> digitBinds <> searchBinds))
    ]
  where
    bindArgv (k, cmd) = (k, [cmd])
    -- A copy-mode key runs a single @send-keys -X <name>@ command.
    copyBind (k, name) = (k, [["send-keys", "-X", name]])
    -- Digit keys feed the vi @[count]@ prefix; a bare @0@ is start-of-line.
    digitBinds =
        [ (tshow d, [["send-keys", "-X", "digit", tshow d]]) | d <- [0 .. 9 :: Int] ]
    -- @/@ and @?@ open the command prompt to collect a search query, then
    -- run the search on submit (the @%%@ splice carries the typed line).
    searchBinds =
        [ ("/", [["command-prompt", "-p", "(search down)"
                 , "send-keys -X search-forward '%%'"]])
        , ("?", [["command-prompt", "-p", "(search up)"
                 , "send-keys -X search-backward '%%'"]])
        ]
    prefixBindings =
        [ ("d", ["detach-client"])
        , ("c", ["new-window"])
        , ("w", ["choose-tree", "-Zw"])
        , ("%", ["split-window", "-h"])
        , ("\"", ["split-window", "-v"])
        , ("x", ["kill-pane"])
        , ("&", ["kill-window"])
        , (",", ["command-prompt", "-I", "#W", "rename-window '%%'"])
        , (".", ["command-prompt", "-p", "index", "move-window -t '%%'"])
        , ("$", ["command-prompt", "-I", "#S", "rename-session '%%'"])
        , ("z", ["resize-pane", "-Z"])
        , ("n", ["next-window"])
        , ("p", ["previous-window"])
        , ("l", ["last-window"])
        , (";", ["last-pane"])
        , ("C-b", ["send-prefix"])
        , ("[", ["copy-mode"])
        , ("]", ["paste-buffer"])
        , (":", ["command-prompt"])
        , ("Left", ["select-pane", "-L"])
        , ("Right", ["select-pane", "-R"])
        , ("Up", ["select-pane", "-U"])
        , ("Down", ["select-pane", "-D"])
        ]
        <> [ (tshow i, ["select-window", "-t", tshow (i :: Int)])
           | i <- [0 .. 9]
           ]
    copyModeViBindings =
        [ ("h", "cursor-left"), ("j", "cursor-down")
        , ("k", "cursor-up"), ("l", "cursor-right")
        , ("$", "end-of-line")
        , ("w", "next-word"), ("b", "previous-word"), ("e", "next-word-end")
        , ("W", "next-space"), ("B", "previous-space"), ("E", "next-space-end")
        , ("^", "back-to-indentation")
        , ("{", "previous-paragraph"), ("}", "next-paragraph")
        , ("f", "jump-forward"), ("F", "jump-backward")
        , ("t", "jump-to-forward"), ("T", "jump-to-backward")
        , (";", "jump-again"), (",", "jump-reverse")
        , ("n", "search-again"), ("N", "search-reverse")
        , ("g", "history-top"), ("G", "history-bottom")
        , ("H", "top-line"), ("M", "middle-line"), ("L", "bottom-line")
        , ("C-f", "page-down"), ("PgDn", "page-down")
        , ("C-b", "page-up"), ("PgUp", "page-up")
        , ("C-d", "halfpage-down"), ("C-u", "halfpage-up")
        , ("v", "begin-selection"), ("V", "select-line")
        , ("o", "other-end"), ("Escape", "clear-selection")
        , ("y", "copy-selection-and-cancel")
        , ("Enter", "copy-selection-and-cancel")
        , ("q", "cancel")
        , ("Left", "cursor-left"), ("Right", "cursor-right")
        , ("Up", "cursor-up"), ("Down", "cursor-down")
        ]
    copyModeBindings =
        [ ("C-b", "cursor-left"), ("C-f", "cursor-right")
        , ("C-p", "cursor-up"), ("C-n", "cursor-down")
        , ("C-a", "start-of-line"), ("C-e", "end-of-line")
        , ("M-f", "next-word-end"), ("M-b", "previous-word")
        , ("M-<", "history-top")
        , ("Space", "begin-selection"), ("C-g", "clear-selection")
        , ("M-w", "copy-selection-and-cancel")
        , ("Enter", "copy-selection-and-cancel")
        , ("q", "cancel"), ("Escape", "cancel")
        , ("Left", "cursor-left"), ("Right", "cursor-right")
        , ("Up", "cursor-up"), ("Down", "cursor-down")
        ]

-- | Read a config file as UTF-8, independent of the process locale and
-- tolerant of malformed bytes. 'TIO.readFile' decodes with the locale
-- encoding, so under a non-UTF-8 locale a config with any non-ASCII byte
-- (a @·@ separator, a @👀@ marker) threw mid-read and aborted the whole
-- startup — read the bytes and decode UTF-8 leniently instead.
readConfigUtf8 :: FilePath -> IO Text
readConfigUtf8 p = TE.decodeUtf8With TEE.lenientDecode <$> B.readFile p

loadConfig :: ServerState -> Maybe FilePath -> IO ()
loadConfig st mconfig =
    forM_ mconfig $ \p -> do
        exists <- doesFileExist p
        when exists $ do
            contents <- readConfigUtf8 p
            case parseConfig contents of
                Left err -> logEvent st.logger ConfigError
                    { file = p, err = err }
                Right cmds -> forM_ cmds $ \argv -> do
                    replies <- runArgv st Nothing argv
                    forM_ [e | RErr e <- replies] $ \e ->
                        logEvent st.logger ConfigError { file = p, err = e }

-- Connections ----------------------------------------------------------

acceptLoop :: ServerState -> N.Socket -> IO ()
acceptLoop st lsock = forever $ do
    (conn, _) <- N.accept lsock
    -- The autostarting client has now reached us; the idle-exit may
    -- consider draining (see 'waitIdle'). This is what lets us drop the
    -- old fixed-delay grace period without racing that client.
    atomically $ writeTVar st.served True
    void . forkIO $
        handleConn st conn
            `catch` (\(e :: SomeException) ->
                logEvent st.logger ServerCrash { err = T.pack (show e) })
            `finally` (N.close conn `catch` \(_ :: SomeException) -> pure ())

handleConn :: ServerState -> N.Socket -> IO ()
handleConn st conn = do
    m <- recvMessage conn
    case m of
        Just (Right (ClientHello h))
            | h.protoVersion == protocolVersion -> welcome st conn h
            | otherwise -> sendMessage conn $ ServerError $
                "protocol mismatch: server " <> tshow protocolVersion
                <> ", client " <> tshow h.protoVersion
        _ -> sendMessage conn (ServerError "expected hello")

welcome :: ServerState -> N.Socket -> Hello -> IO ()
welcome st conn h = do
    client <- newClient st conn h
    case h.intent of
        ControlIntent -> do
            atomically $ modifyTVar' st.clients (Map.insert client.id client)
            sendMessage conn (Welcome "")
            atomically $ writeTVar client.ready True
            controlLoop st client `finally` removeClient st client
        AttachIntent setupCmds -> do
            -- Register early so the setup commands (new-session,
            -- attach-session -t) act on a live client and can switch it.
            atomically $ modifyTVar' st.clients (Map.insert client.id client)
            setupErr <- attachSetup st client setupCmds
            msess <- case setupErr of
                Just _ -> pure Nothing
                Nothing -> currentSession st client
            case (setupErr, msess) of
                (Just e, _) ->
                    sendMessage conn (ServerError e) >> removeClient st client
                (_, Nothing) -> do
                    sendMessage conn (ServerError "no session to attach")
                    removeClient st client
                (_, Just sess) -> do
                    refreshSessionEnv st sess client
                    sname <- readTVarIO sess.name
                    atomically $ writeTVar st.everAttached True
                    applySessionSize st sess.id
                    sendMessage conn (Welcome sname)
                    atomically $ writeTVar client.ready True
                    logEvent st.logger ClientConnected
                        { client = rawClient client.id, term = h.term }
                    withAsync (renderLoop st client) $ \_ ->
                        inputLoop st client
                            `finally` removeClient st client

-- | Run an attaching client's setup commands, leaving @client.session@
-- pointing at the session it should render. An empty command list means a
-- plain attach: reuse an existing session or create one. Returns the
-- first error, if any, so 'welcome' can reject the attach.
attachSetup :: ServerState -> Client -> [[Text]] -> IO (Maybe Text)
attachSetup st client [] = do
    sess <- ensureSession st client
    atomically $ writeTVar client.session sess.id
    pure Nothing
attachSetup st client cmds = do
    replies <- runCommands st (Just client) cmds
    pure (listToMaybe [e | RErr e <- replies])

-- | The session @client.session@ currently names, if it still exists.
currentSession :: ServerState -> Client -> IO (Maybe Session)
currentSession st client = do
    sid <- readTVarIO client.session
    Map.lookup sid <$> readTVarIO st.sessions

newClient :: ServerState -> N.Socket -> Hello -> IO Client
newClient st conn h = do
    cid <- atomically (freshId st.nextClient)
    sendLock <- newMVar ()
    sizeVar <- newTVarIO h.size
    sessVar <- newTVarIO (SessionId (-1))
    lastSessVar <- newTVarIO Nothing
    keyVar <- newIORef NoPrefix
    frameVar <- newIORef (blankFrame h.size)
    cursorVar <- newIORef (Pos 0 0, True)
    fullVar <- newTVarIO True
    toastVar <- newTVarIO Nothing
    promptVar <- newTVarIO Nothing
    pickerVar <- newTVarIO Nothing
    readyVar <- newTVarIO False
    pure Client
        { id = ClientId cid
        , sock = conn
        , sendLock = sendLock
        , size = sizeVar
        , session = sessVar
        , lastSession = lastSessVar
        , ready = readyVar
        , keyState = keyVar
        , lastFrame = frameVar
        , lastCursor = cursorVar
        , needsFull = fullVar
        , toast = toastVar
        , prompt = promptVar
        , picker = pickerVar
        , env = h.env
        , cwd = h.cwd
        }

removeClient :: ServerState -> Client -> IO ()
removeClient st client = do
    sid <- readTVarIO client.session
    atomically $ modifyTVar' st.clients (Map.delete client.id)
    applySessionSize st sid
    logEvent st.logger ClientDetached
        { client = rawClient client.id, reason = "gone" }

-- Every server-initiated message except the Welcome/ServerError handshake
-- (sent raw on the socket) goes through here — both broadcasts and a
-- client's own render frames. Dropping anything before the client is
-- 'ready' guarantees Welcome is the first byte it sees, even when it is
-- attaching to an already-busy (e.g. restored) session.
send :: Client -> ServerToClient -> IO ()
send client msg = do
    isReady <- readTVarIO client.ready
    when isReady $
        withMVar client.sendLock (\_ -> sendMessage client.sock msg)
            `catch` \(_ :: SomeException) -> pure ()

broadcast :: ServerState -> SessionId -> ServerToClient -> IO ()
broadcast st sid msg = do
    cs <- atomically (sessionClients st sid)
    forM_ cs $ \c -> send c msg

-- Sessions --------------------------------------------------------------

-- | On attach, copy the @update-environment@ vars from the attaching
-- client's env into the session, so panes spawned afterward see fresh
-- values (e.g. a new @DISPLAY@ after reconnecting over @ssh -X@).
refreshSessionEnv :: ServerState -> Session -> Client -> IO ()
refreshSessionEnv st sess client = do
    vars <- (.updateEnvironment) <$> readTVarIO st.options
    atomically $ modifyTVar' sess.environ $ \env0 ->
        List.foldl' (\env v -> case List.lookup v client.env of
            Just val -> (v, val) : filter ((/= v) . fst) env
            Nothing  -> env) env0 vars

ensureSession :: ServerState -> Client -> IO Session
ensureSession st client = do
    -- Let any restore finish first, so we attach to the restored tree
    -- rather than racing it and creating a redundant fresh session.
    atomically $ readTVar st.restoring >>= \r -> when r retry
    existing <- readTVarIO st.sessions
    case Map.lookupMin existing of
        Just (_, sess) -> pure sess
        Nothing -> createSession st Nothing Nothing client.env
            (T.unpack client.cwd) =<< readTVarIO client.size

createSession
    :: ServerState -> Maybe Text -> Maybe Text -> [(Text, Text)]
    -> FilePath -> Size -> IO Session
createSession st mname mrun environ dir sz = do
    sid <- atomically (freshId st.nextSession)
    opts <- readTVarIO st.options
    let shellCmd = maybe "/bin/sh" T.unpack (List.lookup "SHELL" environ)
    (win, pane) <- newWindowWithPane st (SessionId sid) shellCmd mrun
        dir environ (windowArea sz)
    nameVar <- newTVarIO (fromMaybe (tshow sid) mname)
    windowsVar <- newTVarIO (Map.singleton opts.baseIndex win)
    currentVar <- newTVarIO opts.baseIndex
    lastVar <- newTVarIO Nothing
    sizeVar <- newTVarIO sz
    environVar <- newTVarIO environ
    cwdVar <- newTVarIO dir
    let sess = Session
            { id = SessionId sid
            , name = nameVar
            , windows = windowsVar
            , currentIx = currentVar
            , lastIx = lastVar
            , lastSize = sizeVar
            , environ = environVar
            , startCwd = cwdVar
            }
    atomically $ modifyTVar' st.sessions (Map.insert sess.id sess)
    startPaneReader st sess.id win pane
    pure sess

newWindowWithPane
    :: ServerState -> SessionId -> FilePath -> Maybe Text -> FilePath
    -> [(Text, Text)] -> Size -> IO (Window, Pane)
newWindowWithPane st sid shellCmd mrun dir environ sz = do
    (wid, pid) <- atomically $
        (,) <$> freshId st.nextWindow <*> freshId st.nextPane
    pane <- spawnPane st (PaneId pid) sid shellCmd (shellStart mrun) dir environ sz
    nameVar <- newTVarIO $ case mrun of
        Just cmd -> T.takeWhile (/= ' ') cmd
        Nothing -> T.pack (baseName shellCmd)
    layoutVar <- newTVarIO (Leaf pane.id)
    panesVar <- newTVarIO (Map.singleton pane.id pane)
    activeVar <- newTVarIO pane.id
    lastActiveVar <- newTVarIO Nothing
    bellVar <- newTVarIO False
    activityVar <- newTVarIO False
    zoomVar <- newTVarIO Nothing
    autoRenameVar <- newTVarIO . (.automaticRename) =<< readTVarIO st.options
    let win = Window
            { id = WindowId wid
            , name = nameVar
            , layout = layoutVar
            , panes = panesVar
            , activeId = activeVar
            , lastActive = lastActiveVar
            , bellFlag = bellVar
            , activity = activityVar
            , zoomed = zoomVar
            , autoRename = autoRenameVar
            }
    pure (win, pane)
  where
    baseName = Prelude.reverse . takeWhile (/= '/') . Prelude.reverse

-- | How a freshly-spawned pane chooses its process.
data PaneStart
    = FreshShell         -- ^ the session's login shell, no command
    | ShellCommand Text  -- ^ a user-supplied command line, run via @sh -c@
                         --   (@new-window@\/@split-window@ with an argument)
    | ExecArgv [Text]    -- ^ exec this argv directly, no shell — used by
                         --   restore so an argument with spaces survives
    deriving (Eq, Show)

-- | The shell-command spawn semantics for the user-facing @new-window@ and
-- @split-window@: an argument is a command line for the shell to interpret.
shellStart :: Maybe Text -> PaneStart
shellStart = maybe FreshShell ShellCommand

spawnPane
    :: ServerState -> PaneId -> SessionId -> FilePath -> PaneStart
    -> FilePath -> [(Text, Text)] -> Size -> IO Pane
spawnPane st pid sid shellCmd mrun dir environ sz = do
    serverPid <- getProcessID
    opts <- readTVarIO st.options
    let cleanEnv =
            [ (T.unpack k, T.unpack v)
            | (k, v) <- environ
            , k `notElem` ["TERM", "TMUX", "TMUX_PANE", "HAT", "HAT_PANE"]
            ]
        hatVar = st.sockPath <> "," <> show serverPid <> ","
            <> show (rawSession sid)
        term = T.unpack opts.defaultTerminal
        paneEnv = cleanEnv <>
            [ ("TERM", term)
            , ("TMUX", hatVar)
            , ("HAT", hatVar)
            , ("TMUX_PANE", "%" <> show (rawPane pid))
            , ("HAT_PANE", "%" <> show (rawPane pid))
            ]
        (cmd, args) = case mrun of
            FreshShell        -> (shellCmd, [])
            ShellCommand run  -> ("/bin/sh", ["-c", T.unpack run])
            ExecArgv (p:rest) -> (T.unpack p, map T.unpack rest)
            ExecArgv []       -> (shellCmd, [])
    pty <- Hat.Pty.spawn Hat.Pty.Spawn
        { cmd = cmd
        , args = args
        , env = paneEnv
        , cwd = Just dir
        , size = sz
        }
    emu <- Emu.newEmulator sz opts.historyLimit
    sizeVar <- newTVarIO sz
    deadVar <- newTVarIO False
    modeVar <- newTVarIO Nothing
    pipeVar <- newTVarIO Nothing
    logEvent st.logger PaneSpawned
        { pane = rawPane pid, cmd = T.pack cmd }
    pure Pane
        { id = pid
        , pty = pty
        , emulator = emu
        , size = sizeVar
        , dead = deadVar
        , startCwd = dir
        , mode = modeVar
        , pipe = pipeVar
        }

-- | The reader thread owns a pane's lifetime: it pumps pty output into
-- the emulator until end-of-file, and 'closePane' runs in a @finally@ so
-- the pane's resources and model entry are released however the loop ends
-- — clean EOF, a hang-up from a kill command, or an exception.
startPaneReader :: ServerState -> SessionId -> Window -> Pane -> IO ()
startPaneReader st sid win pane = void . forkIO $
    readLoop `finally` closePane st sid win pane
  where
    readLoop = do
        bs <- Hat.Pty.readPty pane.pty
        unless (B8.null bs) $ do
            forwardToPipe pane bs
            events <- Emu.feed pane.emulator bs
            forM_ events $ \case
                Emu.Output out -> Hat.Pty.writePty pane.pty out
                -- The pane's own OSC title only feeds #{pane_title} (the
                -- emulator stores it); the client's desktop title is
                -- composed in 'refreshTitles'.
                Emu.TitleChanged _ -> pure ()
                Emu.Bell -> do
                    atomically $ do
                        writeTVar win.bellFlag True
                        bumpDirty st
                    broadcast st sid RingBell
                Emu.ScreenChanged -> atomically $ do
                    markActivity st sid win
                    bumpDirty st
            readLoop

-- | Flag a background window as having activity, when @monitor-activity@
-- is on. The current window is exempt — you are already watching it.
markActivity :: ServerState -> SessionId -> Window -> STM ()
markActivity st sid win = do
    opts <- readTVar st.options
    when opts.monitorActivity $ do
        msess <- Map.lookup sid <$> readTVar st.sessions
        forM_ msess $ \sess -> do
            cur <- readTVar sess.currentIx
            ws <- readTVar sess.windows
            let isCurrent = maybe False (\w -> w.id == win.id) (Map.lookup cur ws)
            unless isCurrent $ writeTVar win.activity True

-- Pane rects and borders for a window, honoring zoom.
windowArrange :: Size -> Window -> STM ([(PaneId, Rect)], [(Pos, Char)])
windowArrange eff win = do
    mz <- readTVar win.zoomed
    lay <- readTVar win.layout
    ps <- readTVar win.panes
    pure $ case mz of
        Just zpid | Map.member zpid ps -> ([(zpid, sizeRect eff)], [])
        _ -> arrange (sizeRect eff) lay

-- | Release everything a pane owns and remove it from the model. Runs
-- exactly once, in the reader thread's @finally@, so no teardown path can
-- forget a resource. (The emulator frees itself via its finalizer.)
closePane :: ServerState -> SessionId -> Window -> Pane -> IO ()
closePane st sid win pane = do
    stopPipe pane
    _ <- Hat.Pty.waitExit pane.pty
    Hat.Pty.closePty pane.pty
    logEvent st.logger PaneExited { pane = rawPane pane.id }
    sessionGone <- atomically $ do
        writeTVar pane.dead True
        modifyTVar' win.panes (Map.delete pane.id)
        lay <- readTVar win.layout
        mz <- readTVar win.zoomed
        when (mz == Just pane.id) $ writeTVar win.zoomed Nothing
        case removeLeaf pane.id lay of
            Just lay' -> do
                writeTVar win.layout lay'
                active <- readTVar win.activeId
                when (active == pane.id) $
                    case layoutPanes lay' of
                        (next : _) -> writeTVar win.activeId next
                        [] -> pure ()
                bumpDirty st
                pure Nothing
            Nothing -> do
                msess <- Map.lookup sid <$> readTVar st.sessions
                case msess of
                    Nothing -> pure Nothing
                    Just sess -> do
                        ws <- readTVar sess.windows
                        let ws' = Map.filter (\w -> w.id /= win.id) ws
                        writeTVar sess.windows ws'
                        if Map.null ws'
                            then do
                                modifyTVar' st.sessions (Map.delete sid)
                                pure (Just sess)
                            else do
                                cur <- readTVar sess.currentIx
                                mlast <- readTVar sess.lastIx
                                let survivors = Map.keysSet ws'
                                forM_ (chooseCurrentOnClose survivors cur mlast) $ \ix -> do
                                    writeTVar sess.currentIx ix
                                    writeTVar sess.lastIx Nothing
                                bumpDirty st
                                pure Nothing
    forM_ sessionGone $ \_ -> broadcast st sid Exited
    applySessionSize st sid

-- | Pick the window to make current after one is closed. 'Nothing' means
-- leave the current window as-is (it survived the close). Otherwise, when
-- the current window is gone, prefer the session's last-active window (as
-- tmux does), falling back to the lowest-numbered survivor when there is
-- no last-active window or it too has been closed.
chooseCurrentOnClose
    :: Set.Set Int   -- ^ indices of the windows that remain
    -> Int           -- ^ the current window index
    -> Maybe Int     -- ^ the last-active window index, if any
    -> Maybe Int
chooseCurrentOnClose survivors cur mlast
    | Set.member cur survivors = Nothing
    | Just lastIx <- mlast, Set.member lastIx survivors = Just lastIx
    | otherwise = Set.lookupMin survivors

-- Sizing ----------------------------------------------------------------

-- | The pane area of the screen: everything except the status line.
windowArea :: Size -> Size
windowArea sz = sz { rows = max 1 (sz.rows - 1) }

-- Effective session size = smallest attached client; panes follow.
applySessionSize :: ServerState -> SessionId -> IO ()
applySessionSize st sid = do
    work <- atomically $ do
        msess <- Map.lookup sid <$> readTVar st.sessions
        case msess of
            Nothing -> pure Nothing
            Just sess -> do
                cs <- sessionClients st sid
                sizes <- mapM (\c -> readTVar c.size) cs
                eff <- case sizes of
                    [] -> readTVar sess.lastSize
                    _ -> pure Size
                        { rows = minimum (map (.rows) sizes)
                        , cols = minimum (map (.cols) sizes)
                        }
                writeTVar sess.lastSize eff
                ws <- readTVar sess.windows
                resizes <- forM (Map.elems ws) $ \win -> do
                    (rects, _) <- windowArrange (windowArea eff) win
                    ps <- readTVar win.panes
                    pure [ (p, rectSize rect)
                         | (pidL, rect) <- rects
                         , Just p <- [Map.lookup pidL ps]
                         ]
                forM_ cs $ \c -> writeTVar c.needsFull True
                bumpDirty st
                pure (Just (concat resizes))
    forM_ (fromMaybe [] work) $ \(pane, sz) -> do
        old <- readTVarIO pane.size
        when (old /= sz) $ do
            atomically $ writeTVar pane.size sz
            Hat.Pty.resize pane.pty sz
            Emu.resize pane.emulator sz
            atomically (bumpDirty st)

rectSize :: Rect -> Size
rectSize r = Size
    { rows = fromIntegral (max 1 (r.endRow - r.startRow))
    , cols = fromIntegral (max 1 (r.endCol - r.startCol))
    }

-- Rendering ---------------------------------------------------------------

renderLoop :: ServerState -> Client -> IO ()
renderLoop st client = loop (-1)
  where
    loop lastGen = do
        gen <- atomically $ do
            g <- readTVar st.dirty
            full <- readTVar client.needsFull
            check (g /= lastGen || full)
            pure g
        renderOnce st client
        loop gen

renderOnce :: ServerState -> Client -> IO ()
renderOnce st client = do
    csize <- readTVarIO client.size
    opts <- readTVarIO st.options
    let rowOff = case opts.statusPosition of
            StatusTop -> 1
            StatusBottom -> 0
        statusRowIx = case opts.statusPosition of
            StatusTop -> 0
            StatusBottom -> fromIntegral csize.rows - 1
    view <- atomically $ do
        sid <- readTVar client.session
        msess <- Map.lookup sid <$> readTVar st.sessions
        case msess of
            Nothing -> pure Nothing
            Just sess -> do
                mwin <- currentWindow sess
                case mwin of
                    Nothing -> pure Nothing
                    Just win -> do
                        eff <- readTVar sess.lastSize
                        (rects, borders) <- windowArrange (windowArea eff) win
                        ps <- readTVar win.panes
                        active <- readTVar win.activeId
                        pure (Just (sess, rects, borders, ps, active))
    (frame, cursor, mActiveRect) <- case view of
        Nothing -> pure (blankFrame csize, (Pos 0 0, False), Nothing)
        Just (sess, rects, borders, ps, active) -> do
            let shiftRect r = r
                    { startRow = r.startRow + rowOff
                    , endRow = r.endRow + rowOff
                    }
                base0 = applyBorders (blankFrame csize)
                    (borderCells opts (List.lookup active rects) rowOff borders)
            base <- foldM' base0 rects $ \acc (pidL, rect) ->
                case Map.lookup pidL ps of
                    Nothing -> pure acc
                    Just pane -> do
                        cells <- paneViewCells st pane
                        pure (overlayGrid acc (shiftRect rect) cells)
            mprompt <- readTVarIO client.prompt
            mtoast <- readTVarIO client.toast
            statusRow <- case (mprompt, mtoast) of
                (Just pr, _) -> pure (promptCells pr (fromIntegral csize.cols))
                (Nothing, Just t) -> pure (toastCells t (fromIntegral csize.cols))
                (Nothing, Nothing) -> statusCells st sess (fromIntegral csize.cols)
            let withStatus
                    | csize.rows >= 2 = base V.// [(statusRowIx, statusRow)]
                    | otherwise = base
            cur <- case mprompt of
                Just pr | csize.rows >= 2 ->
                    pure (Pos { row = statusRowIx
                              , col = min (fromIntegral csize.cols - 1)
                                          (promptCursorCol pr) }, True)
                _ -> case Map.lookup active ps of
                    Nothing -> pure (Pos 0 0, False)
                    Just pane -> do
                        let origin = paneOrigin rects active
                        paneCursor pane origin rowOff
            pure (withStatus, cur, shiftRect <$> List.lookup active rects)
    -- A chooser overlay, when open, is drawn in the active pane's rect
    -- (or the whole window under -Z), with a live preview of the
    -- highlighted node's pane beside the list.
    mpicker <- readTVarIO client.picker
    (frame', cursor') <- case mpicker of
        Nothing -> pure (frame, cursor)
        Just pk -> do
            mPreview <- pickerPreviewCells st pk
            let region = Picker.pickerRegion pk.zoomed csize rowOff mActiveRect
            pure (overlayPicker region pk mPreview frame, (Pos 0 0, False))
    full <- atomically (swapTVar client.needsFull False)
    old <- readIORef client.lastFrame
    oldCursor <- readIORef client.lastCursor
    let ops = if full then fullRedraw frame' else diffFrame old frame'
        cursorOp = CursorAt (fst cursor') (snd cursor')
        needSend = not (null ops) || cursor' /= oldCursor || full
    writeIORef client.lastFrame frame'
    writeIORef client.lastCursor cursor'
    when needSend $ send client (Draw (ops <> [cursorOp]))
  where
    foldM' z xs f = foldM f z xs

-- | The rendered cells of the pane previewing the highlighted node, or
-- 'Nothing' when the node has no preview pane (or it no longer exists).
pickerPreviewCells
    :: ServerState -> PickerState -> IO (Maybe (V.Vector (V.Vector Cell.Cell)))
pickerPreviewCells st pk = case Picker.selectedPreview pk of
    Nothing -> pure Nothing
    Just pid -> do
        mpane <- atomically (findPaneById st (rawPane pid))
        traverse (paneViewCells st) mpane

-- | Paint a chooser into @region@: the list on the left and, when wide
-- enough, a preview of the highlighted node's pane on the right, divided
-- by a vertical rule. Cells outside @region@ (other panes, borders, the
-- status line) are left untouched.
overlayPicker
    :: Rect -> PickerState -> Maybe (V.Vector (V.Vector Cell.Cell))
    -> Frame -> Frame
overlayPicker region pk mPreview frame = overlayGrid frame region grid
  where
    rows = region.endRow - region.startRow
    width = region.endCol - region.startCol
    rendered = Picker.pickerLines rows pk
    padded = take rows (rendered <> repeat (False, ""))
    -- Split only when a preview pane exists and the width allows it.
    split = case (mPreview, Picker.pickerSplit width) of
        (Just previewCells, Just listW) -> Just (listW, previewCells)
        _                               -> Nothing
    grid = V.fromList [ rowCells k | k <- [0 .. rows - 1] ]
    rowCells k =
        let (sel, txt) = padded !! k
            sty = if sel then pickerSelStyle else pickerStyle
        in case split of
            Nothing -> lineCells sty width txt
            Just (listW, previewCells) ->
                lineCells sty listW txt
                    <> V.singleton dividerCell
                    <> previewRow previewCells k (width - listW - 1)

-- | Row @k@ of a preview pane's cells, padded or clipped to @w@ columns.
previewRow :: V.Vector (V.Vector Cell.Cell) -> Int -> Int -> V.Vector Cell.Cell
previewRow grid k w =
    let row = fromMaybe V.empty (grid V.!? k)
    in V.generate w (\c -> fromMaybe Cell.blankCell (row V.!? c))

dividerCell :: Cell.Cell
dividerCell = Cell.Cell { Cell.text = "\x2502", Cell.width = 1, Cell.style = pickerStyle }

pickerStyle :: Cell.Style
pickerStyle = Cell.defaultStyle

pickerSelStyle :: Cell.Style
pickerSelStyle = Cell.defaultStyle { Cell.reverse = True }

paneOrigin :: [(PaneId, Rect)] -> PaneId -> Pos
paneOrigin rects pidL = case List.lookup pidL rects of
    Just r -> Pos { row = r.startRow, col = r.startCol }
    Nothing -> Pos 0 0

-- | Turn @arrange@'s raw border glyphs into styled cells: the active
-- pane's border takes @pane-active-border-style@ (when
-- @pane-border-indicators@ colours it) and gains direction arrows (when
-- it uses arrows); everything else takes @pane-border-style@. Glyphs are
-- remapped per @pane-border-lines@. Positions are shifted by @rowOff@ for
-- a top status line.
borderCells
    :: Options -> Maybe Rect -> Int -> [(Pos, Char)] -> [(Pos, Cell.Cell)]
borderCells opts mActive rowOff borders =
    [ (p { row = p.row + rowOff }, cellAt p ch) | (p, ch) <- borders ]
  where
    (useColor, useArrows) = case opts.paneBorderIndicators of
        IndicatorsOff     -> (False, False)
        IndicatorsColour  -> (True, False)
        IndicatorsArrows  -> (False, True)
        IndicatorsBoth    -> (True, True)
    activeAt p = maybe False (`onPerimeter` p) mActive
    arrows = if useArrows then maybe Map.empty edgeArrows mActive else Map.empty
    cellAt p ch =
        let active = activeAt p
            sty | active && useColor = opts.paneActiveBorderStyle
                | otherwise          = opts.paneBorderStyle
            glyph = case (active, Map.lookup p arrows) of
                (True, Just arr) -> arr
                _                -> mapGlyph opts.paneBorderLines ch
        in Cell.Cell { Cell.text = T.singleton glyph, Cell.width = 1, Cell.style = sty }

-- | Is a position on the (border) perimeter just outside a pane's rect?
onPerimeter :: Rect -> Pos -> Bool
onPerimeter r p =
    ((p.col == r.endCol || p.col == r.startCol - 1)
        && p.row >= r.startRow && p.row < r.endRow)
    || ((p.row == r.endRow || p.row == r.startRow - 1)
        && p.col >= r.startCol && p.col < r.endCol)

-- | An arrow at the midpoint of each of a pane's four border edges,
-- pointing inward.
edgeArrows :: Rect -> Map.Map Pos Char
edgeArrows r = Map.fromList $ concat
    [ vEdge (r.startCol - 1) '\x25b6'  -- ▶ on the left border
    , vEdge r.endCol         '\x25c0'  -- ◀ on the right border
    , hEdge (r.startRow - 1) '\x25bc'  -- ▼ on the top border
    , hEdge r.endRow         '\x25b2'  -- ▲ on the bottom border
    ]
  where
    vEdge col arr = case midOf [r.startRow .. r.endRow - 1] of
        Just row -> [(Pos { row = row, col = col }, arr)]
        Nothing  -> []
    hEdge row arr = case midOf [r.startCol .. r.endCol - 1] of
        Just col -> [(Pos { row = row, col = col }, arr)]
        Nothing  -> []

midOf :: [a] -> Maybe a
midOf xs = case drop (length xs `div` 2) xs of
    (x : _) -> Just x
    []      -> Nothing

-- | Remap the single-line glyphs @arrange@ emits to the chosen line set.
mapGlyph :: BorderLines -> Char -> Char
mapGlyph bl ch = case bl of
    BorderSingle -> ch
    BorderHeavy  -> heavy ch
    BorderDouble -> dbl ch
    BorderSimple -> simple ch
  where
    -- Straight runs and every junction the layout can emit (see
    -- 'Hat.Server.Layout.junction': │ ─ ┼ ┤ ├ ┬ ┴).
    heavy '\x2502' = '\x2503'; heavy '\x2500' = '\x2501'
    heavy '\x253c' = '\x254b'; heavy '\x2524' = '\x252b'
    heavy '\x251c' = '\x2523'; heavy '\x252c' = '\x2533'
    heavy '\x2534' = '\x253b'; heavy c = c
    dbl '\x2502' = '\x2551'; dbl '\x2500' = '\x2550'
    dbl '\x253c' = '\x256c'; dbl '\x2524' = '\x2563'
    dbl '\x251c' = '\x2560'; dbl '\x252c' = '\x2566'
    dbl '\x2534' = '\x2569'; dbl c = c
    simple '\x2502' = '|'; simple '\x2500' = '-'
    simple '\x253c' = '+'; simple '\x2524' = '+'; simple '\x251c' = '+'
    simple '\x252c' = '+'; simple '\x2534' = '+'; simple c = c

-- | The cells a pane contributes to a frame. Normally its live screen;
-- in copy mode, a viewport over scrollback+screen with the selection
-- reverse-videoed.
paneViewCells :: ServerState -> Pane -> IO (V.Vector (V.Vector Cell.Cell))
paneViewCells st pane = do
    scr <- Emu.snapshot pane.emulator
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure scr.cells
        Just s -> do
            hsize <- Emu.scrollbackLength pane.emulator
            opts <- readTVarIO st.options
            let sy = V.length scr.cells
                sx = fromIntegral scr.size.cols
                top = hsize - s.viewportOffY
            rows <- mapM (viewportRow pane scr hsize sx top) [0 .. sy - 1]
            let overlaid = CopyMode.overlaySelection opts.modeKeys top s
                    (V.fromList rows)
                label = "[" <> tshow s.viewportOffY <> "/" <> tshow hsize <> "]"
            pure (stampTopRight label copyIndicatorStyle overlaid)
  where
    padTo sx v = V.generate sx (\c -> fromMaybe Cell.blankCell (v V.!? c))
    viewportRow pane' scr hsize sx top i =
        let a = top + i
        in if a >= hsize
            then pure (padTo sx (fromMaybe V.empty (scr.cells V.!? (a - hsize))))
            else do
                mline <- Emu.scrollbackLine pane'.emulator a
                pure (padTo sx (maybe V.empty V.fromList mline))

-- | tmux's copy-mode position indicator: black on yellow, like the
-- default @mode-style@.
copyIndicatorStyle :: Cell.Style
copyIndicatorStyle = Cell.defaultStyle
    { Cell.fg = Cell.Indexed 0, Cell.bg = Cell.Indexed 3 }

-- | Overlay a label onto the top-right corner of a grid (e.g. the
-- @[scroll/history]@ copy-mode indicator), clipped to the first row.
stampTopRight
    :: Text -> Cell.Style
    -> V.Vector (V.Vector Cell.Cell) -> V.Vector (V.Vector Cell.Cell)
stampTopRight label sty grid
    | V.null grid = grid
    | otherwise = grid V.// [(0, row0 V.// updates)]
  where
    row0 = grid V.! 0
    w = V.length row0
    start = max 0 (w - T.length label)
    updates =
        [ (start + i, cell c)
        | (i, c) <- zip [0 ..] (T.unpack label), start + i < w ]
    cell c = Cell.Cell { Cell.text = T.singleton c, Cell.width = 1, Cell.style = sty }

-- | The cursor a pane shows: its shell cursor, or the copy cursor when
-- in copy mode (hidden when scrolled off the viewport).
paneCursor :: Pane -> Pos -> Int -> IO (Pos, Bool)
paneCursor pane origin rowOff = do
    scr <- Emu.snapshot pane.emulator
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure (place scr.cursor, scr.cursorVisible)
        Just s -> do
            hsize <- Emu.scrollbackLength pane.emulator
            let top = hsize - s.viewportOffY
            case CopyMode.copyCursorPos top (V.length scr.cells) s of
                Just p -> pure (place p, True)
                Nothing -> pure (Pos 0 0, False)
  where
    place p = Pos { row = p.row + origin.row + rowOff
                  , col = p.col + origin.col }

-- Status line -------------------------------------------------------------

lineCells :: Cell.Style -> Int -> Text -> V.Vector Cell.Cell
lineCells style width line = V.fromList (take width (cells <> repeat blank))
  where
    cells = [ Cell.Cell { Cell.text = T.singleton ch
                        , Cell.width = 1
                        , Cell.style = style }
            | ch <- T.unpack line ]
    blank = Cell.Cell { Cell.text = " ", Cell.width = 1, Cell.style = style }

toastCells :: Text -> Int -> V.Vector Cell.Cell
toastCells t width = lineCells toastStyle width t
  where
    toastStyle = Cell.defaultStyle
        { Cell.fg = Cell.Indexed 0, Cell.bg = Cell.Indexed 3 }

-- | The command-prompt status row: the @:@ prefix followed by the line
-- being edited, styled like tmux's message line.
promptCells :: PromptState -> Int -> V.Vector Cell.Cell
promptCells pr width = lineCells promptStyle width (pr.promptLabel <> pr.input)
  where
    promptStyle = Cell.defaultStyle
        { Cell.fg = Cell.Indexed 0, Cell.bg = Cell.Indexed 3 }

-- | The screen column of the prompt's edit cursor.
promptCursorCol :: PromptState -> Int
promptCursorCol pr = T.length pr.promptLabel + pr.cursor

-- Session-level format environment for the active window and pane.
sessionFormatEnv :: ServerState -> Session -> IO FormatEnv
sessionFormatEnv st sess = do
    hostname <- nodeName <$> getSystemID
    (sname, wEnv, mactive, nclients) <- atomically $ do
        sname <- readTVar sess.name
        mwin <- currentWindow sess
        cur <- readTVar sess.currentIx
        wEnv <- case mwin of
            Nothing -> pure []
            Just win -> do
                wname <- readTVar win.name
                pure [ ("window_index", tshow cur)
                     , ("window_name", wname)
                     ]
        mactive <- maybe (pure Nothing) activePane mwin
        cs <- sessionClients st sess.id
        pure (sname, wEnv, mactive, length cs)
    pEnv <- case mactive of
        Nothing -> pure []
        Just pane -> do
            dir <- paneCurrentPath pane
            title <- Emu.title pane.emulator
            modeEnv <- paneModeEnv pane
            pure $ [ ("pane_current_path", T.pack dir)
                   , ("pane_title", title)
                   , ("pane_id", "%" <> tshow (rawPane pane.id))
                   ] <> modeEnv
    sz <- readTVarIO sess.lastSize
    -- @-options are readable as #{@foo}, so if-shell theme conditionals
    -- (@#{@pane-theme}@) resolve.
    userOpts <- (.user) <$> readTVarIO st.options
    msch <- readTVarIO st.colorScheme
    pure . Map.union userOpts . Map.fromList $
        [ ("session_name", sname)
        , ("host", T.pack hostname)
        , ("window_active_clients", tshow nclients)
        , ("window_width", tshow sz.cols)
        , ("window_height", tshow sz.rows)
        , ("color_scheme", maybe "" schemeName msch)
        ]
        <> wEnv <> pEnv

-- | Copy-mode format variables for a pane: @pane_in_mode@/@pane_mode@,
-- plus @copy_cursor_{x,y,line}@ while in mode.
paneModeEnv :: Pane -> IO [(Text, Text)]
paneModeEnv pane = do
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure [("pane_in_mode", "0"), ("pane_mode", "")]
        Just s -> do
            hsize <- Emu.scrollbackLength pane.emulator
            let top = hsize - s.viewportOffY
            pure [ ("pane_in_mode", "1")
                 , ("pane_mode", "copy-mode")
                 , ("copy_cursor_x", tshow s.cursorCol)
                 , ("copy_cursor_y", tshow (s.cursorRow - top))
                 , ("copy_cursor_line", tshow s.cursorRow)
                 ]

-- Resolve #(cmd) through a 15-second cache; refreshes happen in the
-- background so the status line never blocks on a slow script.
resolveShell :: ServerState -> Text -> IO Text
resolveShell st cmdText = do
    now <- getCurrentTime
    cache <- readTVarIO st.shellCache
    case Map.lookup cmdText cache of
        Just (at, val)
            | diffUTCTime now at < 15 -> pure val
            | otherwise -> refresh now val
        Nothing -> refresh now ""
  where
    refresh now oldVal = do
        -- Optimistically bump the timestamp so only one refresh runs.
        atomically $ modifyTVar' st.shellCache
            (Map.insert cmdText (now, oldVal))
        void . forkIO $ do
            r <- try (readCreateProcessWithExitCode
                (shell (T.unpack cmdText)) { close_fds = True } "")
            let val = case r of
                    Right (ExitSuccess, out, _) ->
                        T.strip (T.takeWhile (/= '\n') (T.pack out))
                    Right (ExitFailure _, _, _) -> ""
                    Left (_ :: SomeException) -> ""
            done <- getCurrentTime
            atomically $ modifyTVar' st.shellCache
                (Map.insert cmdText (done, val))
            atomically (bumpDirty st)
        pure oldVal

-- | Expand a format string fully: #{...}, cached #(...), then strftime.
expandFormat :: ServerState -> FormatEnv -> Text -> IO Text
expandFormat st env fmt = do
    -- Pre-resolve shell segments so `evaluate` stays pure.
    resolved <- newIORef Map.empty
    let collect t = case T.breakOn "#(" t of
            (_, rest) | T.null rest -> pure ()
            (_, rest) -> do
                let inner = fst (breakBalanced (T.drop 2 rest))
                val <- resolveShell st inner
                modifyIORef' resolved (Map.insert inner val)
                collect (T.drop (2 + T.length inner + 1) rest)
    collect fmt
    vals <- readIORef resolved
    now <- getZonedTime
    pure (renderFormat env (\c -> Map.findWithDefault "" c vals) now fmt)
  where
    breakBalanced = go (0 :: Int) ""
      where
        go depth acc t = case T.uncons t of
            Nothing -> (acc, "")
            Just (')', rest) | depth == 0 -> (acc, rest)
            Just (c, rest)
                | c == '(' -> go (depth + 1) (acc <> T.singleton c) rest
                | c == ')' -> go (depth - 1) (acc <> T.singleton c) rest
                | otherwise -> go depth (acc <> T.singleton c) rest

statusCells :: ServerState -> Session -> Int -> IO (V.Vector Cell.Cell)
statusCells st sess width = do
    opts <- readTVarIO st.options
    env <- sessionFormatEnv st sess
    let leftFmt = opts.statusLeft
        rightFmt = opts.statusRight
        winFmt = opts.windowStatusFormat
        winCurFmt = opts.windowStatusCurrentFormat
    entries <- do
        ws <- readTVarIO sess.windows
        cur <- readTVarIO sess.currentIx
        mlast <- readTVarIO sess.lastIx
        clientCount <- length <$> atomically (sessionClients st sess.id)
        forM (Map.toAscList ws) $ \(ix, win) -> do
            (wname, bell, act) <- atomically $ (,,)
                <$> readTVar win.name <*> readTVar win.bellFlag
                <*> readTVar win.activity
            let flags = T.concat
                    [ if ix == cur then "*"
                      else if Just ix == mlast then "-" else ""
                    , if bell then "!" else ""
                    , if act then "#" else ""
                    ]
                -- A session's clients all view its current window, so only
                -- that window has active clients; the rest have none.
                activeClients = if ix == cur then clientCount else 0
                wenv = Map.union (Map.fromList
                    [ ("window_index", tshow ix)
                    , ("window_name", wname)
                    , ("window_flags", flags)
                    , ("window_active_clients", tshow activeClients)
                    ]) env
                fmt = if ix == cur then winCurFmt else winFmt
                style | ix == cur = opts.windowStatusCurrentStyle
                      | bell      = opts.windowStatusBellStyle
                      | otherwise = opts.windowStatusStyle
            txt <- expandFormat st wenv fmt
            pure (txt, style)
    left <- T.take opts.statusLeftLength <$> expandFormat st env leftFmt
    right <- T.take opts.statusRightLength <$> expandFormat st env rightFmt
    let sty = opts.statusStyle
        blank = blankOf sty
        sep = styledCells sty " "
        leftCells = styledCells sty left
        rightCells = styledCells sty right
        entryCells =
            List.intercalate sep [ styledCells est etxt | (etxt, est) <- entries ]
        body = leftCells <> entryCells
        pad = width - length body - length rightCells
        cells
            | pad >= 0 = body <> replicate pad blank <> rightCells
            | otherwise = take width (body <> sep <> rightCells)
    pure (V.fromList (take width (cells <> repeat blank)))

-- | One cell per character, all in the given style.
styledCells :: Cell.Style -> Text -> [Cell.Cell]
styledCells sty t =
    [ Cell.Cell { Cell.text = T.singleton c, Cell.width = 1, Cell.style = sty }
    | c <- T.unpack t ]

blankOf :: Cell.Style -> Cell.Cell
blankOf sty = Cell.Cell { Cell.text = " ", Cell.width = 1, Cell.style = sty }

-- Input ---------------------------------------------------------------------

inputLoop :: ServerState -> Client -> IO ()
inputLoop st client = loop
  where
    loop = do
        m <- recvMessage client.sock
        case m of
            Nothing -> pure ()
            Just (Left err) -> logEvent st.logger ProtocolError
                { client = rawClient client.id, err = T.pack err }
            Just (Right msg) -> case msg of
                Input bs -> do
                    handleInput st client bs
                    loop
                Resize sz -> do
                    atomically (writeTVar client.size sz)
                    sid <- readTVarIO client.session
                    applySessionSize st sid
                    loop
                Detach -> send client DetachOk
                Command cmds -> do
                    replies <- runCommands st (Just client) cmds
                    forM_ replies $ \case
                        ROutput out -> showToast st client out
                        RErr e -> showToast st client ("error: " <> e)
                    loop
                ClientHello {} -> pure ()

handleInput :: ServerState -> Client -> B.ByteString -> IO ()
handleInput st client bs = do
    mpicker <- readTVarIO client.picker
    mprompt <- readTVarIO client.prompt
    case (mpicker, mprompt) of
        (Just pk, _) -> handlePickerInput st client pk (tokenizeKeys bs)
        (_, Just pr) -> handlePromptInput st client pr bs
        _            -> handleKeys st client bs

-- | While a chooser is open it owns every keystroke: navigate/search
-- until Enter (run the item's command and close) or Escape (close).
handlePickerInput
    :: ServerState -> Client -> PickerState -> [Key] -> IO ()
handlePickerInput _ _ _ [] = pure ()
handlePickerInput st client pk0 keys = do
    let step acc k = case acc of
            Picker.PickerStay pk -> Picker.editPicker pk k
            done -> done
        result = List.foldl' step (Picker.PickerStay pk0) keys
    case result of
        Picker.PickerStay pk -> atomically $ do
            writeTVar client.picker (Just pk)
            bumpDirty st
        Picker.PickerCancel -> closePicker st client
        Picker.PickerRun line -> do
            closePicker st client
            replies <- runCommandText st (Just client) line
            forM_ replies $ \case
                ROutput out -> showToast st client out
                RErr e -> showToast st client ("error: " <> e)

closePicker :: ServerState -> Client -> IO ()
closePicker st client = atomically $ do
    writeTVar client.picker Nothing
    bumpDirty st

-- | While the command prompt is open it owns every keystroke: the line
-- editor consumes them until Enter (run and close) or Escape (close).
handlePromptInput
    :: ServerState -> Client -> PromptState -> B.ByteString -> IO ()
handlePromptInput st client pr0 bs = do
    history <- readTVarIO st.cmdHistory
    let step acc k = case acc of
            Prompt.Editing pr -> Prompt.editPrompt history pr k
            done -> done
        result = List.foldl' step (Prompt.Editing pr0) (tokenizeKeys bs)
    case result of
        Prompt.Editing pr -> atomically $ do
            writeTVar client.prompt (Just pr)
            bumpDirty st
        Prompt.Cancel -> atomically $ do
            writeTVar client.prompt Nothing
            bumpDirty st
        Prompt.Submit line -> do
            let templated = not (T.null pr0.template)
                cmd = Prompt.applyTemplate pr0.template line
            atomically $ do
                writeTVar client.prompt Nothing
                -- Templated prompts (rename-window, …) keep the : history
                -- clean; only bare command lines are remembered.
                unless templated $
                    modifyTVar' st.cmdHistory (Prompt.pushHistory line)
                bumpDirty st
            -- A blank submission cancels: never rename a window to "".
            unless (T.null (T.strip line)) $ do
                replies <- runCommandText st (Just client) cmd
                forM_ replies $ \case
                    ROutput out -> showToast st client out
                    RErr e -> showToast st client ("error: " <> e)

-- Keys are routed and run ONE AT A TIME, re-resolving the active pane and
-- its copy-mode table before each. A key that enters or leaves copy mode
-- therefore changes how the very next key in the same input chunk is
-- routed, so @prefix [@ followed immediately by motions never leaks the
-- motions to the shell (and vice versa on exit).
handleKeys :: ServerState -> Client -> B.ByteString -> IO ()
handleKeys st client bs = do
    opts <- readTVarIO st.options
    km <- readTVarIO st.keymap
    let loop kst [] = writeIORef client.keyState kst
        loop kst (k0 : rest) = do
            mpane <- clientActivePane st client
            -- A pending vi char search (f/F/t/T) captures this key as its
            -- target, ahead of any keymap lookup.
            searchFed <- maybe (pure False) (feedPendingSearch k0) mpane
            if searchFed then loop kst rest else do
              modeTable <- case mpane of
                Just pane -> fmap (fmap (.keyTable)) (readTVarIO pane.mode)
                Nothing -> pure Nothing
              k <- reencodeCursor mpane k0
              keep <- keepKey opts mpane k
              if not keep
                then loop kst rest
                else do
                    let (kst', actions) =
                            routeKeys opts.prefix km modeTable kst [k]
                    forM_ actions $ \case
                        Passthrough raw ->
                            forM_ mpane $ \pane -> Hat.Pty.writePty pane.pty raw
                        RunCommands cmds -> forM_ cmds $ \argv -> do
                            replies <- runArgv st (Just client) argv
                            forM_ replies $ \case
                                ROutput out -> showToast st client out
                                RErr e -> showToast st client ("error: " <> e)
                    loop kst' rest
    st0 <- readIORef client.keyState
    loop st0 (tokenizeKeys bs)
  where
    -- Focus in/out reach the pane only when focus-events is on AND the
    -- pane's app has requested focus reporting (?1004). A bare shell never
    -- asks, so the report is dropped rather than echoed as a stray "^[[I".
    keepKey opts mpane k
        | k.name `notElem` ["FocusIn", "FocusOut"] = pure True
        | not opts.focusEvents = pure False
        | otherwise = case mpane of
            Just pane -> (.focusReport) <$> Emu.modes pane.emulator
            Nothing -> pure False
    -- When the pane's copy mode is waiting for a char-search target, this
    -- key IS the target: a single printable char runs the search, anything
    -- else (Escape, Enter, an arrow) cancels it. Returns whether it was
    -- consumed here.
    feedPendingSearch key pane = do
        mmode <- readTVarIO pane.mode
        case mmode of
            Just s | Just _ <- s.pendingSearch -> do
                if T.length key.name == 1
                    then runCopyModeCommand st pane "apply-search" [key.name]
                    else runCopyModeCommand st pane "cancel-search" []
                pure True
            _ -> pure False

-- | Cursor keys ('\ESC[A' vs '\ESCOA') depend on the pane's DECCKM mode,
-- so re-encode them via the pane's emulator instead of forwarding the raw
-- bytes the client's terminal happened to send.
reencodeCursor :: Maybe Pane -> Key -> IO Key
reencodeCursor mpane key = case (mpane, cursorKeyOf key.name) of
    (Just pane, Just ck) -> do
        enc <- Emu.encodeKey pane.emulator ck
        pure Key { name = key.name, raw = enc }
    _ -> pure key
  where
    cursorKeyOf n = case n of
        "Up"    -> Just Emu.CursorUp
        "Down"  -> Just Emu.CursorDown
        "Left"  -> Just Emu.CursorLeft
        "Right" -> Just Emu.CursorRight
        "Home"  -> Just Emu.CursorHome
        "End"   -> Just Emu.CursorEnd
        _       -> Nothing

showToast :: ServerState -> Client -> Text -> IO ()
showToast st client t = do
    atomically $ do
        writeTVar client.toast (Just t)
        bumpDirty st
    displayMs <- (.displayTime) <$> readTVarIO st.options
    void . forkIO $ do
        threadDelay (displayMs * 1000)
        atomically $ do
            cur <- readTVar client.toast
            when (cur == Just t) $ do
                writeTVar client.toast Nothing
                bumpDirty st

clientActivePane :: ServerState -> Client -> IO (Maybe Pane)
clientActivePane st client = atomically $ do
    mv <- clientView st client
    case mv of
        Nothing -> pure Nothing
        Just (_, win) -> activePane win

-- | The session and current window of a client, if both exist.
clientView :: ServerState -> Client -> STM (Maybe (Session, Window))
clientView st client = do
    sid <- readTVar client.session
    msess <- Map.lookup sid <$> readTVar st.sessions
    case msess of
        Nothing -> pure Nothing
        Just sess -> do
            mwin <- currentWindow sess
            pure ((,) sess <$> mwin)

-- The command engine ---------------------------------------------------------

data Reply = ROutput Text | RErr Text

runCommandText :: ServerState -> Maybe Client -> Text -> IO [Reply]
runCommandText st mclient input = case parseCommandLine input of
    Left err -> pure [RErr err]
    Right cmds -> runCommands st mclient cmds

runCommands :: ServerState -> Maybe Client -> [[Text]] -> IO [Reply]
runCommands st mclient cmds = concat <$> mapM (runArgv st mclient) cmds

runArgv :: ServerState -> Maybe Client -> [Text] -> IO [Reply]
runArgv _ _ [] = pure []
runArgv st mclient (name : args) = do
    forM_ mclient $ \c -> logEvent st.logger CommandRun
        { client = rawClient c.id, command = T.unwords (name : args) }
    case Map.lookup name commandTable of
        Nothing -> pure [RErr ("unknown command: " <> name)]
        Just impl -> impl st mclient args
            `catch` \(e :: SomeException) ->
                pure [RErr (name <> ": " <> T.pack (show e))]

type CommandImpl = ServerState -> Maybe Client -> [Text] -> IO [Reply]

commandTable :: Map.Map Text CommandImpl
commandTable = Map.fromList $ concatMap expand
    [ (["bind-key", "bind"], cmdBind)
    , (["unbind-key", "unbind"], cmdUnbind)
    , (["set-option", "set", "set-window-option", "setw"], cmdSet)
    , (["show-options", "show", "show-option"], cmdShow)
    , (["source-file", "source"], cmdSourceFile)
    , (["new-window", "neww"], cmdNewWindow)
    , (["select-window", "selectw"], cmdSelectWindow)
    , (["next-window", "next"], cmdNextWindow)
    , (["previous-window", "prev"], cmdPrevWindow)
    , (["last-window", "last"], cmdLastWindow)
    , (["kill-window", "killw"], cmdKillWindow)
    , (["rename-window", "renamew"], cmdRenameWindow)
    , (["move-window", "movew"], cmdMoveWindow)
    , (["split-window", "splitw"], cmdSplitWindow)
    , (["select-pane", "selectp"], cmdSelectPane)
    , (["kill-pane", "killp"], cmdKillPane)
    , (["swap-pane", "swapp"], cmdSwapPane)
    , (["clear-history", "clearhist"], cmdClearHistory)
    , (["break-pane", "breakp"], cmdBreakPane)
    , (["join-pane", "joinp"], cmdJoinPane)
    , (["select-layout", "selectl"], cmdSelectLayout)
    , (["resize-pane", "resizep"], cmdResizePane)
    , (["last-pane", "lastp"], cmdLastPane)
    , (["detach-client", "detach"], cmdDetachClient)
    , (["send-prefix"], cmdSendPrefix)
    , (["send-keys", "send"], cmdSendKeys)
    , (["copy-mode"], cmdCopyMode)
    , (["command-prompt"], cmdCommandPrompt)
    , (["choose-tree"], cmdChooseTree)
    , (["choose-window", "choosew"], cmdChooseWindow)
    , (["show-buffer", "showb"], cmdShowBuffer)
    , (["set-buffer", "setb"], cmdSetBuffer)
    , (["list-buffers", "lsb"], cmdListBuffers)
    , (["delete-buffer", "deleteb"], cmdDeleteBuffer)
    , (["save-buffer", "saveb"], cmdSaveBuffer)
    , (["paste-buffer", "pasteb"], cmdPasteBuffer)
    , (["pipe-pane", "pipep"], cmdPipePane)
    , (["new-session", "new"], cmdNewSession)
    , (["attach-session", "attach"], cmdAttachSession)
    , (["kill-session"], cmdKillSession)
    , (["has-session", "has"], cmdHasSession)
    , (["start-server", "start"], cmdStartServer)
    , (["rename-session", "rename"], cmdRenameSession)
    , (["list-sessions", "ls"], cmdListSessions)
    , (["list-windows", "lsw"], cmdListWindows)
    , (["list-panes", "lsp"], cmdListPanes)
    , (["capture-pane", "capturep"], cmdCapturePane)
    , (["resize-window", "resizew"], cmdResizeWindow)
    , (["switch-client", "switchc"], cmdSwitchClient)
    , (["kill-server"], cmdKillServer)
    , (["display-message", "display"], cmdDisplayMessage)
    , (["run-shell", "run"], cmdRunShell)
    , (["if-shell", "if"], cmdIfShell)
    ]
  where
    expand (names, impl) = [(n, impl) | n <- names]

-- getopt-style flag parser: @spec@ lists the letters that take a
-- value. Bundled forms work like tmux: @-dsfoo@ is @-d -s foo@.
-- Returns (value flags as ("-s", value), boolean flags as "-d",
-- positional args).
parseArgs :: [Char] -> [Text] -> ([(Text, Text)], [Text], [Text])
parseArgs spec = go [] []
  where
    go opts flags = \case
        [] -> (opts, flags, [])
        ("--" : rest) -> (opts, flags, rest)   -- end-of-flags separator
        (a : rest)
            | Just bundle <- T.stripPrefix "-" a
            , not (T.null bundle)
            , not (isNumber a) ->
                let (opts', flags', rest') = scanBundle bundle rest
                in go (opts' <> opts) (flags' <> flags) rest'
            | otherwise -> (opts, flags, a : rest)
    scanBundle bundle rest = case T.uncons bundle of
        Nothing -> ([], [], rest)
        Just (c, more)
            | c `elem` spec ->
                let val = fromMaybe more (T.stripPrefix "=" more)
                in case (T.null val, rest) of
                    (False, _) -> ([(dash c, val)], [], rest)
                    (True, v : rest') -> ([(dash c, v)], [], rest')
                    (True, []) -> ([(dash c, "")], [], [])
            | otherwise ->
                let (opts', flags', rest') = scanBundle more rest
                in (opts', dash c : flags', rest')
    dash c = T.pack ['-', c]
    isNumber a = case TR.signed TR.decimal a of
        Right (_ :: Int, restT) -> T.null restT
        Left _ -> False

targetSession :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Session)
targetSession st mclient mtarget = atomically $ do
    sessions <- readTVar st.sessions
    case mtarget of
        Just t -> do
            let stripped = fromMaybe t (T.stripSuffix ":" t)
            found <- forM (Map.elems sessions) $ \sess -> do
                nm <- readTVar sess.name
                pure $ if nm == stripped
                    || tshow (rawSession sess.id) == stripped
                    || ("$" <> tshow (rawSession sess.id)) == stripped
                    then Just sess else Nothing
            pure (foldr (\m acc -> maybe acc Just m) Nothing found)
        Nothing -> case mclient of
            Just client -> do
                sid <- readTVar client.session
                case Map.lookup sid sessions of
                    Just sess -> pure (Just sess)
                    Nothing -> pure (snd <$> Map.lookupMax sessions)
            Nothing -> pure (snd <$> Map.lookupMax sessions)

withTargetSession
    :: ServerState -> Maybe Client -> Maybe Text
    -> (Session -> IO [Reply]) -> IO [Reply]
withTargetSession st mclient mtarget body = do
    msess <- targetSession st mclient mtarget
    case msess of
        Nothing -> pure [RErr "no such session"]
        Just sess -> body sess

withCurrentWindow
    :: ServerState -> Maybe Client
    -> (Session -> Window -> IO [Reply]) -> IO [Reply]
withCurrentWindow st mclient body = do
    mv <- atomically (maybe (pure Nothing) (clientView st) mclient)
    view <- case mv of
        Just v -> pure (Just v)
        Nothing -> do
            msess <- targetSession st mclient Nothing
            case msess of
                Nothing -> pure Nothing
                Just sess -> do
                    mwin <- atomically (currentWindow sess)
                    pure ((,) sess <$> mwin)
    case view of
        Nothing -> pure [RErr "no current window"]
        Just (sess, win) -> body sess win

-- Command implementations.

cmdBind :: CommandImpl
cmdBind st _ args = do
    let (opts, flags, pos) = parseArgs "TN" args
        table
            | "-n" `elem` flags = "root"
            | Just t <- lookup "-T" opts = t
            | otherwise = "prefix"
    case pos of
        (keyName : rest)
            | Just key <- parseKeyName keyName
            , not (null rest) -> do
                let cmds = splitBinding rest
                atomically $ modifyTVar' st.keymap $
                    Map.insertWith Map.union table
                        (Map.singleton key.name cmds)
                pure []
        (keyName : _) ->
            pure [RErr ("bind: bad key or command: " <> keyName)]
        _ -> pure [RErr "usage: bind [-n] [-T table] key command..."]

-- A binding's command part: one brace block to re-parse, or argv split
-- on ";" tokens (from escaped semicolons).
splitBinding :: [Text] -> [[Text]]
splitBinding = \case
    [block] | T.any (\c -> c == ' ' || c == ';' || c == '\n') block ->
        case parseConfig block of
            Right cmds -> cmds
            Left _ -> [[block]]
    rest -> filter (not . null) (splitOnSemis rest)
  where
    splitOnSemis xs = case break (== ";") xs of
        (before, []) -> [before]
        (before, _ : after) -> before : splitOnSemis after

cmdUnbind :: CommandImpl
cmdUnbind st _ args = do
    let (opts, flags, pos) = parseArgs "T" args
        table
            | "-n" `elem` flags = "root"
            | Just t <- lookup "-T" opts = t
            | otherwise = "prefix"
    case pos of
        [keyName] | Just key <- parseKeyName keyName -> do
            atomically $ modifyTVar' st.keymap $
                Map.adjust (Map.delete key.name) table
            pure []
        _ -> pure [RErr "usage: unbind [-n] [-T table] key"]

cmdSet :: CommandImpl
cmdSet st _ args = do
    let (_, flags, pos) = parseArgs "t" args
        append = "-a" `elem` flags
    case pos of
        (nameT : rest) -> do
            let value = T.unwords rest
            r <- atomically $ do
                opts <- readTVar st.options
                case setOption append opts nameT value of
                    Left err -> pure (Just err)
                    Right opts' -> do
                        writeTVar st.options opts'
                        bumpDirty st
                        pure Nothing
            pure $ case r of
                Just err -> [RErr err]
                Nothing -> []
        [] -> pure [RErr "usage: set [-g] option value"]

cmdShow :: CommandImpl
cmdShow st _ args = do
    let (_, flags, pos) = parseArgs "t" args
        valueOnly = "-v" `elem` flags
        quiet = "-q" `elem` flags
    opts <- readTVarIO st.options
    case pos of
        [name] -> case lookupOption opts name of
            Just v -> pure [ROutput (if valueOnly then v else name <> " " <> v)]
            Nothing
                | quiet -> pure []
                | otherwise -> pure [RErr ("unknown option: " <> name)]
        _ -> pure [RErr "usage: show-options [-gsvq] name"]

lookupOption :: Options -> Text -> Maybe Text
lookupOption opts name = case name of
    "prefix" -> Just opts.prefix
    "base-index" -> Just (tshow opts.baseIndex)
    "pane-base-index" -> Just (tshow opts.paneBaseIndex)
    "history-limit" -> Just (tshow opts.historyLimit)
    "default-terminal" -> Just opts.defaultTerminal
    "word-separators" -> Just opts.wordSeparators
    "status-position" -> Just (case opts.statusPosition of
        StatusTop -> "top"; StatusBottom -> "bottom")
    "mode-keys" -> Just (case opts.modeKeys of
        KeysVi -> "vi"; KeysEmacs -> "emacs")
    "status-left" -> Just opts.statusLeft
    "status-left-length" -> Just (tshow opts.statusLeftLength)
    "status-right" -> Just opts.statusRight
    "status-right-length" -> Just (tshow opts.statusRightLength)
    "window-status-format" -> Just opts.windowStatusFormat
    "window-status-current-format" -> Just opts.windowStatusCurrentFormat
    "automatic-rename" -> Just (if opts.automaticRename then "on" else "off")
    "automatic-rename-format" -> Just opts.automaticRenameFormat
    _
        | "@" `T.isPrefixOf` name -> Map.lookup name opts.user
        | otherwise -> Nothing

-- | Apply a @set-option@. @append@ is tmux's @-a@: for string-valued
-- options it concatenates onto the current value (used to build up
-- @status-right@ across several lines). Unknown non-@\@@ options are
-- rejected so a config never looks supported when its behavior is not
-- yet implemented.
setOption :: Bool -> Options -> Text -> Text -> Either Text Options
setOption append opts name value =
    mark <$> setOptionRaw append opts name value
  where
    -- Successful sets are remembered so scheme palettes ('applyPalette')
    -- never override an option the user chose.
    mark o = o { explicit = Set.insert name o.explicit }

setOptionRaw :: Bool -> Options -> Text -> Text -> Either Text Options
setOptionRaw append opts name value = case name of
    "prefix" -> case parseKeyName value of
        Just k -> Right opts { prefix = k.name }
        Nothing -> Left ("bad prefix key: " <> value)
    "base-index" -> withInt $ \n -> opts { baseIndex = n }
    "pane-base-index" -> withInt $ \n -> opts { paneBaseIndex = n }
    "history-limit" -> withInt $ \n -> opts { historyLimit = n }
    "default-terminal" -> Right opts { defaultTerminal = value }
    "word-separators" -> Right opts { wordSeparators = value }
    "status-position" -> case value of
        "top" -> Right opts { statusPosition = StatusTop }
        "bottom" -> Right opts { statusPosition = StatusBottom }
        _ -> Left "status-position: top or bottom"
    "mode-keys" -> case value of
        "vi" -> Right opts { modeKeys = KeysVi }
        "emacs" -> Right opts { modeKeys = KeysEmacs }
        _ -> Left "mode-keys: vi or emacs"
    "status-left" -> Right opts { statusLeft = withAppend opts.statusLeft }
    "status-left-length" -> withInt $ \n -> opts { statusLeftLength = n }
    "status-right" -> Right opts { statusRight = withAppend opts.statusRight }
    "status-right-length" -> withInt $ \n -> opts { statusRightLength = n }
    "window-status-format" ->
        Right opts { windowStatusFormat = withAppend opts.windowStatusFormat }
    "window-status-current-format" ->
        Right opts { windowStatusCurrentFormat =
            withAppend opts.windowStatusCurrentFormat }
    "status-style" -> Right opts { statusStyle = parseStyle value }
    "window-status-style" -> Right opts { windowStatusStyle = parseStyle value }
    "window-status-current-style" ->
        Right opts { windowStatusCurrentStyle = parseStyle value }
    "window-status-bell-style" ->
        Right opts { windowStatusBellStyle = parseStyle value }
    "pane-border-style" -> Right opts { paneBorderStyle = parseStyle value }
    "pane-active-border-style" ->
        Right opts { paneActiveBorderStyle = parseStyle value }
    "pane-border-lines" -> case value of
        "single" -> Right opts { paneBorderLines = BorderSingle }
        "heavy"  -> Right opts { paneBorderLines = BorderHeavy }
        "double" -> Right opts { paneBorderLines = BorderDouble }
        "simple" -> Right opts { paneBorderLines = BorderSimple }
        _ -> Left "pane-border-lines: single, heavy, double, or simple"
    "pane-border-indicators" -> case value of
        "off"     -> Right opts { paneBorderIndicators = IndicatorsOff }
        "colour"  -> Right opts { paneBorderIndicators = IndicatorsColour }
        "color"   -> Right opts { paneBorderIndicators = IndicatorsColour }
        "arrows"  -> Right opts { paneBorderIndicators = IndicatorsArrows }
        "both"    -> Right opts { paneBorderIndicators = IndicatorsBoth }
        _ -> Left "pane-border-indicators: off, colour, arrows, or both"
    "set-titles" -> withOnOff $ \b -> opts { setTitles = b }
    "escape-time" -> withInt $ \n -> opts { escapeTime = n }
    "display-time" -> withInt $ \n -> opts { displayTime = n }
    "focus-events" -> withOnOff $ \b -> opts { focusEvents = b }
    "aggressive-resize" -> withOnOff $ \b -> opts { aggressiveResize = b }
    "monitor-activity" -> withOnOff $ \b -> opts { monitorActivity = b }
    "automatic-rename" -> withOnOff $ \b -> opts { automaticRename = b }
    "automatic-rename-format" -> Right opts { automaticRenameFormat = value }
    "update-environment" -> Right opts { updateEnvironment = T.words value }
    "main-pane-width" -> withInt $ \n -> opts { mainPaneWidth = n }
    "main-pane-height" -> withInt $ \n -> opts { mainPaneHeight = n }
    _
        | "@" `T.isPrefixOf` name ->
            Right opts { user = Map.insert name value opts.user }
        | otherwise -> Left ("unimplemented option: " <> name)
  where
    withInt f = case TR.decimal value of
        Right (n, restT) | T.null restT -> Right (f n)
        _ -> Left (name <> ": not a number: " <> value)
    withOnOff f = case value of
        "on"  -> Right (f True)
        "off" -> Right (f False)
        _ -> Left (name <> ": on or off")
    withAppend old = if append then old <> value else value

cmdSourceFile :: CommandImpl
cmdSourceFile st mclient args = case pos of
    [path] -> do
        let p = T.unpack path
        exists <- doesFileExist p
        if not exists
            then if "-q" `elem` flags
                then pure []
                else pure [RErr ("no such file: " <> path)]
            else do
                contents <- readConfigUtf8 p
                case parseConfig contents of
                    Left err -> pure [RErr err]
                    Right cmds
                        -- -n: check syntax, do not execute
                        | "-n" `elem` flags -> pure []
                        | otherwise ->
                            concat <$> mapM (runArgv st mclient) cmds
    _ -> pure [RErr "usage: source-file path"]
  where
    (_, flags, pos) = parseArgs "" args

cmdNewWindow :: CommandImpl
cmdNewWindow st mclient args = do
    let (opts, flags, pos) = parseArgs "nct" args
    withTargetSession st mclient Nothing $ \sess -> do
        eff <- readTVarIO sess.lastSize
        srvOpts <- readTVarIO st.options
        environ <- readTVarIO sess.environ
        let shellCmd = maybe "/bin/sh" T.unpack (List.lookup "SHELL" environ)
            mrun = case pos of
                [] -> Nothing
                ws -> Just (T.unwords ws)
        dir <- case lookup "-c" opts of
            Nothing -> readTVarIO sess.startCwd
            Just d -> do
                env <- sessionFormatEnv st sess
                T.unpack <$> expandFormat st env d
        (win, pane) <- newWindowWithPane st sess.id shellCmd mrun dir
            environ (windowArea eff)
        forM_ (lookup "-n" opts) $ \nm -> atomically $ do
            writeTVar win.name nm
            writeTVar win.autoRename False
        atomically $ do
            ws <- readTVar sess.windows
            cur <- readTVar sess.currentIx
            let requested = do
                    t <- lookup "-t" opts
                    case TR.decimal t of
                        Right (n, restT) | T.null restT -> Just n
                        _ -> Nothing
                nextFreeFrom n = until (\i -> not (Map.member i ws)) (+ 1) n
                ix = case requested of
                    Just n
                        | "-a" `elem` flags -> nextFreeFrom (n + 1)
                        | otherwise -> n
                    Nothing
                        | "-a" `elem` flags -> nextFreeFrom (cur + 1)
                        | otherwise -> nextFreeFrom srvOpts.baseIndex
                ix' = if Map.member ix ws then nextFreeFrom srvOpts.baseIndex else ix
            modifyTVar' sess.windows (Map.insert ix' win)
            unless ("-d" `elem` flags) $ do
                writeTVar sess.lastIx (Just cur)
                writeTVar sess.currentIx ix'
            bumpDirty st
        startPaneReader st sess.id win pane
        applySessionSize st sess.id
        pure []

cmdSelectWindow :: CommandImpl
cmdSelectWindow st mclient args = do
    let (opts, _, pos) = parseArgs "t" args
        target = case (lookup "-t" opts, pos) of
            (Just t, _) -> Just t
            (Nothing, [t]) -> Just t
            _ -> Nothing
    mres <- resolveWindowTarget st mclient target
    case mres of
        Nothing -> pure [RErr "usage: select-window -t index"]
        Just (sess, ix) -> do
            atomically (switchTo st sess ix)
            pure []

-- Accepts @[session][:window]@ where session may be a name or @$id@ and
-- window may be a number, @$@ for the last window, or omitted to mean
-- the session's current window. A bare token without @:@ is a window
-- spec in the current session (or a session spec if it starts with @$@).
resolveWindowTarget
    :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe (Session, Int))
resolveWindowTarget st mclient mtarget = case mtarget of
    Nothing -> do
        msess <- targetSession st mclient Nothing
        traverse currentPair msess
    Just t
        | ":" `T.isInfixOf` t -> withColon t
        | "$" `T.isPrefixOf` t -> do
            msess <- targetSession st mclient (Just t)
            traverse currentPair msess
        | otherwise -> do  -- bare window spec in current session
            msess <- targetSession st mclient Nothing
            case msess of
                Nothing -> pure Nothing
                Just sess -> do
                    mix <- parseWinIx sess t
                    pure $ (,) sess <$> mix
  where
    withColon t =
        let (s, rest) = T.break (== ':') t
            w = T.drop 1 rest
        in do
            msess <- targetSession st mclient
                (if T.null s then Nothing else Just s)
            case msess of
                Nothing -> pure Nothing
                Just sess
                    | T.null w -> Just <$> currentPair sess
                    | otherwise -> do
                        mix <- parseWinIx sess w
                        pure $ (,) sess <$> mix
    currentPair sess = (,) sess <$> readTVarIO sess.currentIx
    parseWinIx sess "$" = do
        ws <- readTVarIO sess.windows
        pure (fst <$> Map.lookupMax ws)
    parseWinIx _ w = pure $ case TR.decimal w of
        Right (n, rest) | T.null rest -> Just n
        _ -> Nothing

switchTo :: ServerState -> Session -> Int -> STM ()
switchTo st sess ix = do
    ws <- readTVar sess.windows
    cur <- readTVar sess.currentIx
    when (ix /= cur) $ forM_ (Map.lookup ix ws) $ \win -> do
        writeTVar sess.lastIx (Just cur)
        writeTVar sess.currentIx ix
        writeTVar win.bellFlag False
        writeTVar win.activity False
        bumpDirty st

cmdNextWindow, cmdPrevWindow, cmdLastWindow :: CommandImpl
cmdNextWindow st mclient args
    | "-a" `elem` flags = nextActivityWindow st mclient
    | otherwise = cycleWindow st mclient 1
  where (_, flags, _) = parseArgs "t" args
cmdPrevWindow st mclient _ = cycleWindow st mclient (-1)
cmdLastWindow st mclient _ =
    withTargetSession st mclient Nothing $ \sess -> do
        atomically $ do
            mlast <- readTVar sess.lastIx
            forM_ mlast (switchTo st sess)
        pure []

-- | @next-window -a@: switch to the next window (cyclically) that has an
-- activity flag set.
nextActivityWindow :: ServerState -> Maybe Client -> IO [Reply]
nextActivityWindow st mclient =
    withTargetSession st mclient Nothing $ \sess -> do
        atomically $ do
            ws <- readTVar sess.windows
            cur <- readTVar sess.currentIx
            let ixs = Map.keys ws
                curPos = fromMaybe (-1) (List.elemIndex cur ixs)
                (before, after) = splitAt (curPos + 1) ixs
                ordered = after <> before
            flagged <- forM ordered $ \ix -> do
                a <- maybe (pure False) (readTVar . (.activity)) (Map.lookup ix ws)
                pure (ix, a)
            case [ ix | (ix, True) <- flagged ] of
                (ix : _) -> switchTo st sess ix
                []       -> pure ()
        pure []

cycleWindow :: ServerState -> Maybe Client -> Int -> IO [Reply]
cycleWindow st mclient step =
    withTargetSession st mclient Nothing $ \sess -> do
        atomically $ do
            ws <- readTVar sess.windows
            cur <- readTVar sess.currentIx
            let ixs = Map.keys ws
            case ixs of
                [] -> pure ()
                _ -> do
                    let n = length ixs
                        curPos = fromMaybe 0 (List.elemIndex cur ixs)
                        ix = ixs !! ((curPos + step + n) `mod` n)
                    switchTo st sess ix
        pure []

-- Only @-p@ (print to stdout, plain text) is supported so far; escape,
-- range, and hyperlink flags are ignored — enough for the light-touch
-- @capturep -p@ uses in upstream tests.
cmdCapturePane :: CommandImpl
cmdCapturePane st mclient _ = do
    withCurrentWindow st mclient $ \_ win -> do
        mactive <- atomically (activePane win)
        case mactive of
            Nothing -> pure []
            Just pane -> do
                scr <- Emu.snapshot pane.emulator
                let rows = V.toList scr.cells
                    rowText r = T.stripEnd . T.concat
                        $ [ c.text | c <- V.toList r ]
                    body = T.intercalate "\n" (map rowText rows)
                pure [ROutput body]

cmdResizeWindow :: CommandImpl
cmdResizeWindow st mclient args = do
    let (opts, _, _) = parseArgs "txy" args
        parseInt t = case TR.decimal t of
            Right (n, rest) | T.null rest -> Just n
            _ -> Nothing
    withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
        current <- readTVarIO sess.lastSize
        let sz = current
                { cols = fromMaybe current.cols (parseInt =<< lookup "-x" opts)
                , rows = fromMaybe current.rows (parseInt =<< lookup "-y" opts)
                }
        atomically $ writeTVar sess.lastSize sz
        applySessionSize st sess.id
        pure []

cmdKillWindow :: CommandImpl
cmdKillWindow st mclient _ =
    withCurrentWindow st mclient $ \_ win -> do
        ps <- readTVarIO win.panes
        forM_ (Map.elems ps) $ \p -> Hat.Pty.closePty p.pty
        pure []

cmdRenameWindow :: CommandImpl
cmdRenameWindow st mclient args = do
    let (opts, _, pos) = parseArgs "t" args
    case pos of
        [nm] -> do
            mres <- resolveWindowTarget st mclient (lookup "-t" opts)
            case mres of
                Just (sess, ix) -> do
                    ws <- readTVarIO sess.windows
                    forM_ (Map.lookup ix ws) $ \win ->
                        -- An empty name hands the window back to
                        -- automatic-rename; a real name pins it.
                        if T.null nm
                            then do
                                atomically (writeTVar win.autoRename True)
                                refreshAutoNames st
                            else atomically $ do
                                writeTVar win.name nm
                                writeTVar win.autoRename False
                                bumpDirty st
                    pure []
                Nothing -> pure [RErr "no such window"]
        _ -> pure [RErr "usage: rename-window [-t target] name"]

cmdSplitWindow :: CommandImpl
cmdSplitWindow st mclient args = do
    let (opts, flags, pos) = parseArgs "ctlp" args
        orient
            | "-h" `elem` flags = LeftRight
            | otherwise = TopBottom
        before = "-b" `elem` flags
        -- @-f@: split spans the whole window, not just the active pane.
        full = "-f" `elem` flags
        mrun = case pos of
            [] -> Nothing
            ws -> Just (T.unwords ws)
    withCurrentWindow st mclient $ \sess win -> do
        mactive <- atomically (activePane win)
        case mactive of
            Nothing -> pure [RErr "no active pane"]
            Just active -> do
                eff <- readTVarIO sess.lastSize
                (rects, _) <- atomically (windowArrange (windowArea eff) win)
                let mrect = List.lookup active.id rects
                    wholeRect = sizeRect (windowArea eff)
                    fitRect = if full then Just wholeRect else mrect
                    fits = case (orient, fitRect) of
                        (LeftRight, Just r) -> r.endCol - r.startCol >= 5
                        (TopBottom, Just r) -> r.endRow - r.startRow >= 5
                        _ -> False
                if not fits
                    then pure [RErr "create pane failed: pane too small"]
                    else do
                        pid <- PaneId <$> atomically (freshId st.nextPane)
                        dir <- case lookup "-c" opts of
                            Just d -> do
                                env <- sessionFormatEnv st sess
                                T.unpack <$> expandFormat st env d
                            Nothing -> paneCurrentPath active
                        environ <- readTVarIO sess.environ
                        let shellCmd = maybe "/bin/sh" T.unpack
                                (List.lookup "SHELL" environ)
                        pane <- spawnPane st pid sess.id shellCmd (shellStart mrun)
                            dir environ (windowArea eff)
                        atomically $ do
                            modifyTVar' win.panes (Map.insert pane.id pane)
                            modifyTVar' win.layout $ if full
                                then splitFull orient before pane.id
                                else splitLeaf active.id orient before pane.id
                            lastA <- readTVar win.activeId
                            writeTVar win.lastActive (Just lastA)
                            writeTVar win.activeId pane.id
                            writeTVar win.zoomed Nothing
                            bumpDirty st
                        startPaneReader st sess.id win pane
                        applySessionSize st sess.id
                        pure []

-- | Where is a pane's child process now? /proc, with a fallback.
paneCurrentPath :: Pane -> IO FilePath
paneCurrentPath pane = do
    r <- try (PFiles.readSymbolicLink
        ("/proc/" <> show (Hat.Pty.pid pane.pty) <> "/cwd"))
    pure $ case r of
        Left (_ :: IOException) -> pane.startCwd
        Right dir -> dir

cmdSelectPane :: CommandImpl
cmdSelectPane st mclient args = do
    let (opts, flags, _) = parseArgs "tT" args
        mdir
            | "-L" `elem` flags = Just DirLeft
            | "-R" `elem` flags = Just DirRight
            | "-U" `elem` flags = Just DirUp
            | "-D" `elem` flags = Just DirDown
            | otherwise = Nothing
        parseNum t = case TR.decimal t of
            Right (n, rest) | T.null rest -> Just n
            _ -> Nothing
        -- Bare @-t N@ picks the Nth pane in the current window (0-based).
        mIndex = lookup "-t" opts >>= parseNum
    case mdir of
        Nothing
            | "-M" `elem` flags -> do
                atomically $ writeTVar st.markedPane Nothing >> bumpDirty st
                pure []
            | "-m" `elem` flags -> do
                mp <- targetPane st mclient (lookup "-t" opts)
                forM_ mp $ \pane -> atomically $
                    writeTVar st.markedPane (Just pane.id) >> bumpDirty st
                pure []
            | "-l" `elem` flags -> cmdLastPane st mclient []
            | Just n <- mIndex ->
                withCurrentWindow st mclient $ \_ win -> do
                    atomically $ do
                        ps <- readTVar win.panes
                        let ordered = Map.elems ps
                        case drop n ordered of
                            (p : _) -> do
                                active <- readTVar win.activeId
                                writeTVar win.lastActive (Just active)
                                writeTVar win.activeId p.id
                                bumpDirty st
                            [] -> pure ()
                    pure []
            | otherwise -> pure [RErr "usage: select-pane -L|-R|-U|-D|-l|-t index"]
        Just dir -> withCurrentWindow st mclient $ \sess win -> do
            atomically $ do
                eff <- readTVar sess.lastSize
                (rects, _) <- windowArrange (windowArea eff) win
                active <- readTVar win.activeId
                forM_ (neighbor rects active dir) $ \next -> do
                    writeTVar win.lastActive (Just active)
                    writeTVar win.activeId next
                    bumpDirty st
            pure []

cmdLastPane :: CommandImpl
cmdLastPane st mclient _ =
    withCurrentWindow st mclient $ \_ win -> do
        atomically $ do
            mlast <- readTVar win.lastActive
            ps <- readTVar win.panes
            forM_ mlast $ \lastP -> when (Map.member lastP ps) $ do
                cur <- readTVar win.activeId
                writeTVar win.lastActive (Just cur)
                writeTVar win.activeId lastP
                bumpDirty st
        pure []

cmdKillPane :: CommandImpl
cmdKillPane st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    mpane <- targetPane st mclient (lookup "-t" opts)
    forM_ mpane $ \pane -> Hat.Pty.closePty pane.pty
    pure []

-- | @swap-pane [-s src] [-t dst] [-U|-D] [-d]@: exchange two panes'
-- positions. @src@ defaults to the active pane; without @-d@ the active
-- pane follows to @dst@'s slot, so the config's @splitw … \; swapp -t !
-- \; killp -t !@ edge-move idiom lands the content and kills the emptied
-- slot.
cmdSwapPane :: CommandImpl
cmdSwapPane st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
        keepActive = "-d" `elem` flags
    withCurrentWindow st mclient $ \sess win -> do
        msrc <- targetPane st mclient (lookup "-s" opts)
        mdst <- case lookup "-t" opts of
            Just t -> targetPane st mclient (Just t)
            Nothing
                | "-U" `elem` flags -> siblingPane st win (-1)
                | "-D" `elem` flags -> siblingPane st win 1
                | otherwise -> pure Nothing
        case (msrc, mdst) of
            (Just src, Just dst) | src.id /= dst.id -> do
                atomically $ do
                    ps <- readTVar win.panes
                    when (Map.member src.id ps && Map.member dst.id ps) $ do
                        modifyTVar' win.layout (swapLeaves src.id dst.id)
                        unless keepActive $ do
                            writeTVar win.lastActive (Just src.id)
                            writeTVar win.activeId dst.id
                        writeTVar win.zoomed Nothing
                        bumpDirty st
                applySessionSize st sess.id
                pure []
            _ -> pure []

-- | The pane @step@ positions from the active one in layout order.
siblingPane :: ServerState -> Window -> Int -> IO (Maybe Pane)
siblingPane _ win step = atomically $ do
    lay <- readTVar win.layout
    ps <- readTVar win.panes
    active <- readTVar win.activeId
    let order = layoutPanes lay
    pure $ case List.elemIndex active order of
        Just i | not (null order) ->
            let pid = order !! ((i + step + length order) `mod` length order)
            in Map.lookup pid ps
        _ -> Nothing

-- | @clear-history [-t target]@: drop a pane's scrollback.
cmdClearHistory :: CommandImpl
cmdClearHistory st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    mpane <- targetPane st mclient (lookup "-t" opts)
    forM_ mpane $ \pane -> do
        Emu.clearScrollback pane.emulator
        atomically (bumpDirty st)
    pure []

-- | Detach a pane from whichever window holds it, collapsing the layout
-- and dropping the window if it becomes empty. The pane's pty keeps
-- running — this only re-parents it, backing @break-pane@/@join-pane@.
removePaneFromTree :: ServerState -> PaneId -> STM ()
removePaneFromTree st pid = do
    sessions <- readTVar st.sessions
    forM_ (Map.elems sessions) $ \sess -> do
        ws <- readTVar sess.windows
        forM_ (Map.toList ws) $ \(ix, win) -> do
            ps <- readTVar win.panes
            when (Map.member pid ps) $ do
                writeTVar win.panes (Map.delete pid ps)
                mz <- readTVar win.zoomed
                when (mz == Just pid) $ writeTVar win.zoomed Nothing
                lay <- readTVar win.layout
                case removeLeaf pid lay of
                    Just lay' -> do
                        writeTVar win.layout lay'
                        active <- readTVar win.activeId
                        when (active == pid) $ case layoutPanes lay' of
                            (n : _) -> writeTVar win.activeId n
                            [] -> pure ()
                    Nothing -> do
                        modifyTVar' sess.windows (Map.delete ix)
                        cur <- readTVar sess.currentIx
                        when (cur == ix) $ do
                            ws' <- readTVar sess.windows
                            forM_ (Map.lookupMin ws') $ \(i, _) ->
                                writeTVar sess.currentIx i
                bumpDirty st

-- | Build a fresh single-pane window around an already-running pane.
wrapPaneInWindow :: ServerState -> Pane -> IO Window
wrapPaneInWindow st pane = do
    wid <- atomically (freshId st.nextWindow)
    name <- paneCommandName pane
    nameVar <- newTVarIO name
    layoutVar <- newTVarIO (Leaf pane.id)
    panesVar <- newTVarIO (Map.singleton pane.id pane)
    activeVar <- newTVarIO pane.id
    lastActiveVar <- newTVarIO Nothing
    bellVar <- newTVarIO False
    activityVar <- newTVarIO False
    zoomVar <- newTVarIO Nothing
    autoRenameVar <- newTVarIO . (.automaticRename) =<< readTVarIO st.options
    pure Window
        { id = WindowId wid
        , name = nameVar
        , layout = layoutVar
        , panes = panesVar
        , activeId = activeVar
        , lastActive = lastActiveVar
        , bellFlag = bellVar
        , activity = activityVar
        , zoomed = zoomVar
        , autoRename = autoRenameVar
        }

-- | A pane's foreground program (from @/proc@) as a display name:
-- normalized by 'commandName', so a NixOS-wrapped @vim@ shows as @vim@
-- everywhere (window titles, @#{pane_current_command}@, the persisted
-- tree). Falls back to @sh@.
paneCommandName :: Pane -> IO Text
paneCommandName pane = do
    mfg <- Hat.Pty.foregroundCommand pane.pty
    raw <- case mfg of
        Just cmd -> pure cmd
        Nothing -> do
            r <- try (TIO.readFile ("/proc/" <> show (Hat.Pty.pid pane.pty) <> "/comm"))
            pure $ case r of
                Right s -> let t = T.strip s in if T.null t then "sh" else t
                Left (_ :: IOException) -> "sh"
    pure (commandName raw)

-- | Recompute the names of every @automatic-rename@ window from its
-- active pane's foreground command, bumping the render generation on any
-- change. Driven by a periodic poll so no-output commands (an idle
-- @less@, a waiting @cat@) still get picked up.
refreshAutoNames :: ServerState -> IO ()
refreshAutoNames st = do
    fmt <- (.automaticRenameFormat) <$> readTVarIO st.options
    sessions <- Map.elems <$> readTVarIO st.sessions
    forM_ sessions $ \sess -> do
        ws <- Map.toAscList <$> readTVarIO sess.windows
        forM_ ws $ \(ix, win) -> do
            auto <- readTVarIO win.autoRename
            when auto $ do
                mnew <- autoName st sess ix win fmt
                forM_ mnew $ \newName -> atomically $ do
                    cur <- readTVar win.name
                    when (cur /= newName && not (T.null newName)) $ do
                        writeTVar win.name newName
                        bumpDirty st

-- | Recompute each session's desktop title (see 'composeTitle') from its
-- current window's active pane, broadcasting only on change. Shares the
-- 500ms poll with 'refreshAutoNames' because the same inputs (foreground
-- command, cwd) change without any event. A no-op unless @set-titles@ is
-- on.
refreshTitles :: ServerState -> IORef (Map.Map SessionId Text) -> IO ()
refreshTitles st ref = do
    opts <- readTVarIO st.options
    when opts.setTitles $ do
        homeDir <- maybe "" T.pack <$> lookupEnv "HOME"
        sessions <- readTVarIO st.sessions
        forM_ (Map.toList sessions) $ \(sid, sess) -> do
            (sname, mwin) <- atomically $
                (,) <$> readTVar sess.name <*> currentWindow sess
            forM_ mwin $ \win -> do
                wname <- readTVarIO win.name
                auto <- readTVarIO win.autoRename
                mpane <- atomically (activePane win)
                forM_ mpane $ \pane -> do
                    dir <- paneCurrentPath pane
                    prog <- paneCommandName pane
                    ptitle <- Emu.title pane.emulator
                    let t = composeTitle titleBudget TitleParts
                            { session = sname
                            -- An auto-renamed window just repeats the
                            -- program; only a pinned name adds signal.
                            , window = if auto then "" else wname
                            , path = T.pack dir
                            , home = homeDir
                            -- A title the program set itself (OSC) is
                            -- the most specific component we have.
                            , program = if T.null ptitle then prog else ptitle
                            }
                    prev <- Map.lookup sid <$> readIORef ref
                    unless (prev == Just t) $ do
                        modifyIORef' ref (Map.insert sid t)
                        broadcast st sid (SetTitle t)

-- | Room for the composed desktop title. The title bar's real width is
-- unknowable from here; this keeps the tail visible in any reasonable
-- window.
titleBudget :: Int
titleBudget = 80

-- | The name an @automatic-rename@ window should currently take: the
-- @automatic-rename-format@ expanded against the active pane. The default
-- format is just @#{pane_current_command}@, so it takes a cheap path.
autoName :: ServerState -> Session -> Int -> Window -> Text -> IO (Maybe Text)
autoName st sess ix win fmt = do
    mpane <- atomically (activePane win)
    case mpane of
        Nothing -> pure Nothing
        Just pane
            | fmt == "#{pane_current_command}" -> Just <$> paneCommandName pane
            | otherwise -> do
                pbase <- (.paneBaseIndex) <$> readTVarIO st.options
                env <- paneFormatEnv st sess ix win pbase pane
                Just <$> expandFormat st env fmt

-- | @break-pane [-d] [-t]@: move the active pane into a new window of
-- its own. No-op when it is the window's only pane.
cmdBreakPane :: CommandImpl
cmdBreakPane st mclient args = do
    let (_, flags, _) = parseArgs "t" args
    withCurrentWindow st mclient $ \sess win -> do
        mactive <- atomically (activePane win)
        ps <- readTVarIO win.panes
        case mactive of
            Just pane | Map.size ps > 1 -> do
                win2 <- wrapPaneInWindow st pane
                atomically $ do
                    removePaneFromTree st pane.id
                    ws <- readTVar sess.windows
                    let ix = until (\i -> not (Map.member i ws)) (+ 1) 0
                    modifyTVar' sess.windows (Map.insert ix win2)
                    unless ("-d" `elem` flags) $ do
                        cur <- readTVar sess.currentIx
                        writeTVar sess.lastIx (Just cur)
                        writeTVar sess.currentIx ix
                    bumpDirty st
                applySessionSize st sess.id
                pure []
            _ -> pure [RErr "can't break with only one pane"]

-- | @join-pane [-h|-v] [-b] -s src [-t dst]@: move the @src@ pane into
-- the destination window (default: the current one), splitting its
-- active pane. Backs the config's @choose-window 'join-pane -?s "%%"'@.
cmdJoinPane :: CommandImpl
cmdJoinPane st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
        orient | "-h" `elem` flags = LeftRight
               | otherwise = TopBottom
        before = "-b" `elem` flags
    msrc <- targetPane st mclient (lookup "-s" opts)
    withCurrentWindow st mclient $ \sess dstWin -> do
        dstPanes <- readTVarIO dstWin.panes
        case msrc of
            Just src | not (Map.member src.id dstPanes) -> do
                atomically $ do
                    dstActive <- readTVar dstWin.activeId
                    removePaneFromTree st src.id
                    modifyTVar' dstWin.panes (Map.insert src.id src)
                    modifyTVar' dstWin.layout
                        (splitLeaf dstActive orient before src.id)
                    writeTVar dstWin.lastActive (Just dstActive)
                    writeTVar dstWin.activeId src.id
                    writeTVar dstWin.zoomed Nothing
                    bumpDirty st
                applySessionSize st sess.id
                pure []
            _ -> pure [RErr "no source pane"]

-- | @select-layout <name>@: rearrange the current window's panes into a
-- named layout (@main-vertical@, @even-horizontal@, @tiled@, …). The
-- @main-*@ layouts size their main pane from @main-pane-width@/@-height@.
cmdSelectLayout :: CommandImpl
cmdSelectLayout st mclient args = do
    let (_, _, pos) = parseArgs "t" args
    case pos of
        (nameT : _) -> case parseLayoutName nameT of
            Just lname -> applyNamedLayout st mclient lname
            Nothing -> applyLayoutString st mclient nameT
        [] -> pure [RErr "usage: select-layout name"]

-- | Reshape the current window to a saved tmux layout string, mapping
-- its geometry onto the window's panes in order (resurrect's restore).
applyLayoutString :: ServerState -> Maybe Client -> Text -> IO [Reply]
applyLayoutString st mclient str =
    withCurrentWindow st mclient $ \sess win -> do
        ok <- atomically $ do
            pids <- layoutPanes <$> readTVar win.layout
            case layoutFromString str pids of
                Just lay -> do
                    writeTVar win.layout lay
                    writeTVar win.zoomed Nothing
                    bumpDirty st
                    pure True
                Nothing -> pure False
        if ok
            then applySessionSize st sess.id >> pure []
            else pure [RErr ("invalid layout: " <> str)]

parseLayoutName :: Text -> Maybe LayoutName
parseLayoutName = \case
    "main-vertical"   -> Just MainVertical
    "main-horizontal" -> Just MainHorizontal
    "even-horizontal" -> Just EvenHorizontal
    "even-vertical"   -> Just EvenVertical
    "tiled"           -> Just Tiled
    _                 -> Nothing

applyNamedLayout :: ServerState -> Maybe Client -> LayoutName -> IO [Reply]
applyNamedLayout st mclient lname =
    withCurrentWindow st mclient $ \sess win -> do
        eff <- readTVarIO sess.lastSize
        opts <- readTVarIO st.options
        let area = windowArea eff
            clampR r = max (1 % 10) (min (9 % 10) r) :: Rational
            ratioOf num den = clampR (toInteger num % max 1 (toInteger den))
            mainRatio = case lname of
                MainVertical   -> ratioOf opts.mainPaneWidth area.cols
                MainHorizontal -> ratioOf opts.mainPaneHeight area.rows
                _              -> 1 % 2
        atomically $ do
            pids <- layoutPanes <$> readTVar win.layout
            unless (null pids) $ do
                writeTVar win.layout (namedLayout lname mainRatio pids)
                writeTVar win.zoomed Nothing
                bumpDirty st
        applySessionSize st sess.id
        pure []

-- | @move-window -s src -t dst@: renumber (or relocate) a window to the
-- destination index, possibly in another session. Restore replays this
-- to place windows at their saved indices.
cmdMoveWindow :: CommandImpl
cmdMoveWindow st mclient args = do
    let (opts, _, _) = parseArgs "st" args
    msrc <- resolveWindowTarget st mclient (lookup "-s" opts)
    mdst <- resolveWindowTarget st mclient (lookup "-t" opts)
    case (msrc, mdst) of
        (Just (srcSess, srcIx), Just (dstSess, dstIx)) -> do
            res <- atomically $ do
                sws <- readTVar srcSess.windows
                case Map.lookup srcIx sws of
                    Nothing -> pure (Right ())  -- nothing to move
                    Just win
                        | srcSess.id == dstSess.id, srcIx == dstIx ->
                            pure (Right ())  -- already there
                        | otherwise -> do
                            dws <- readTVar dstSess.windows
                            if Map.member dstIx dws
                                then pure (Left ("can't move window: "
                                    <> tshow dstIx <> " in use"))
                                else do
                                    modifyTVar' srcSess.windows (Map.delete srcIx)
                                    modifyTVar' dstSess.windows (Map.insert dstIx win)
                                    followFocus srcSess dstSess srcIx dstIx
                                    bumpDirty st
                                    pure (Right ())
            pure [RErr e | Left e <- [res]]
        _ -> pure [RErr "usage: move-window -s src -t dst"]
  where
    -- The moved window keeps the focus: within a session the current
    -- index follows it to the destination; across sessions the source
    -- session falls back to its lowest remaining window.
    followFocus srcSess dstSess srcIx dstIx = do
        cur <- readTVar srcSess.currentIx
        when (cur == srcIx) $
            if srcSess.id == dstSess.id
                then writeTVar srcSess.currentIx dstIx
                else do
                    ws' <- readTVar srcSess.windows
                    forM_ (Map.lookupMin ws') $ \(i, _) ->
                        writeTVar srcSess.currentIx i

cmdResizePane :: CommandImpl
cmdResizePane st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        delta = case pos of
            (n : _) | Right (v, restT) <- TR.decimal n, T.null restT -> v
            _ -> 1
    if "-Z" `elem` flags
        then zoomTarget st mclient (lookup "-t" opts) >> pure []
        else do
            let mdir
                    | "-L" `elem` flags = Just DirLeft
                    | "-R" `elem` flags = Just DirRight
                    | "-U" `elem` flags = Just DirUp
                    | "-D" `elem` flags = Just DirDown
                    | otherwise = Nothing
            case mdir of
                Nothing ->
                    pure [RErr "usage: resize-pane -L|-R|-U|-D [n] | -Z"]
                Just dir -> withCurrentWindow st mclient $ \sess win -> do
                    atomically $ do
                        eff <- readTVar sess.lastSize
                        active <- readTVar win.activeId
                        modifyTVar' win.layout
                            (resizeSplit active dir delta
                                (sizeRect (windowArea eff)))
                        bumpDirty st
                    applySessionSize st sess.id
                    pure []

-- | Toggle zoom on the caller's current window. With a @-t@ target the
-- targeted pane becomes active first, so @resize-pane -t ! -Z@ zooms the
-- alternate pane (as the config's @Z@ binding intends).
zoomTarget :: ServerState -> Maybe Client -> Maybe Text -> IO ()
zoomTarget st mclient mtok = do
    mtarget <- targetPane st mclient mtok
    void . withCurrentWindow st mclient $ \sess win -> do
        atomically $ do
            ps <- readTVar win.panes
            forM_ mtarget $ \pane -> when (Map.member pane.id ps) $ do
                active <- readTVar win.activeId
                when (active /= pane.id) $ do
                    writeTVar win.lastActive (Just active)
                    writeTVar win.activeId pane.id
            mz <- readTVar win.zoomed
            newActive <- readTVar win.activeId
            writeTVar win.zoomed $ case mz of
                Just _ -> Nothing
                Nothing -> Just newActive
            bumpDirty st
        applySessionSize st sess.id
        pure []

cmdDetachClient :: CommandImpl
cmdDetachClient _ mclient _ = do
    forM_ mclient $ \client -> send client DetachOk
    pure []

cmdSendPrefix :: CommandImpl
cmdSendPrefix st mclient _ = do
    forM_ mclient $ \client -> do
        opts <- readTVarIO st.options
        forM_ (parseKeyName opts.prefix) $ \key -> do
            mpane <- clientActivePane st client
            forM_ mpane $ \pane -> Hat.Pty.writePty pane.pty key.raw
    pure []

cmdSendKeys :: CommandImpl
cmdSendKeys st mclient args = do
    let (opts, flags, pos) = parseArgs "tN" args
        literal = "-l" `elem` flags
        modeCmd = "-X" `elem` flags
    mpicker <- maybe (pure Nothing) (readTVarIO . (.picker)) mclient
    case (mpicker, mclient) of
        -- An open chooser owns send-keys: they drive its navigation/search
        -- (this is how the config's @… \; send-keys /@ enters search).
        (Just pk, Just client) | not modeCmd -> do
            handlePickerInput st client pk
                (concatMap (tokenizeKeys . argBytes literal) pos)
            pure []
        _ -> do
            mpane <- targetPane st mclient (lookup "-t" opts)
            forM_ mpane $ \pane ->
                if modeCmd
                    then case pos of
                        (name : cmdArgs) -> runCopyModeCommand st pane name cmdArgs
                        [] -> pure ()
                    else Hat.Pty.writePty pane.pty
                        (B.concat (map (argBytes literal) pos))
            pure []
  where
    argBytes True a = TE.encodeUtf8 a
    argBytes False a = case parseKeyName a of
        Just k -> k.raw
        Nothing -> TE.encodeUtf8 a

runCopyModeCommand :: ServerState -> Pane -> Text -> [Text] -> IO ()
runCopyModeCommand st pane name cmdArgs = do
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure ()  -- not in copy mode; -X is a no-op
        Just state
            -- A digit key builds the @[count]@ prefix rather than running
            -- a motion; @0@ with no count pending is @start-of-line@.
            | name == "digit", Just d <- readDigit cmdArgs ->
                atomically $ do
                    writeTVar pane.mode (Just (CopyMode.pushDigit d state))
                    bumpDirty st
            | otherwise -> case Map.lookup name CopyMode.handlers of
                Nothing -> pure ()
                Just h -> do
                    -- Motions repeat [count] times; yanks never do. Every
                    -- command clears the pending count.
                    let count
                            | name `elem` ["copy-selection", "copy-pipe"] = 1
                            | otherwise = min 1000 (maybe 1 (max 1) state.numPrefix)
                    result <- applyN h (state { numPrefix = Nothing }) count
                    result' <- traverse (scrollPaneToCursor pane) result
                    atomically $ do
                        writeTVar pane.mode result'
                        bumpDirty st
  where
    readDigit (a : _) = case TR.decimal a of
        Right (d, rest) | T.null rest, d >= 0, d <= 9 -> Just d
        _ -> Nothing
    readDigit [] = Nothing
    -- Run a handler @n@ times, threading the state and stopping early if
    -- it exits copy mode (@Nothing@).
    applyN _ s 0 = pure (Just s)
    applyN h s n = do
        r <- h st pane s cmdArgs
        case r of
            Nothing -> pure Nothing
            Just s' -> applyN h s' (n - 1)

-- | Re-center a pane's copy-mode viewport on its cursor after a motion.
scrollPaneToCursor :: Pane -> CopyModeState -> IO CopyModeState
scrollPaneToCursor pane s = do
    hsize <- Emu.scrollbackLength pane.emulator
    scr <- Emu.snapshot pane.emulator
    pure (CopyMode.scrollToCursor hsize (V.length scr.cells) s)

-- | Resolve the pane a command should act on from its @-t target@.
-- @!@ is the current window's last-active pane, @~@/@{marked}@ the
-- marked pane, @%N@ a pane by id anywhere; otherwise the current pane
-- of the caller's window.
targetPane :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Pane)
targetPane st mclient mtok = case parsePaneTarget mtok of
    PaneById n -> atomically (findPaneById st n)
    PaneMarked -> atomically $ do
        mpid <- readTVar st.markedPane
        maybe (pure Nothing) (findPaneById st . rawPane) mpid
    tgt -> do
        mwin <- currentWindowOf st mclient
        case mwin of
            Nothing -> pure Nothing
            Just win -> atomically $ do
                ps <- readTVar win.panes
                case tgt of
                    PaneLast -> do
                        ml <- readTVar win.lastActive
                        pure (ml >>= (`Map.lookup` ps))
                    _ -> do
                        a <- readTVar win.activeId
                        pure (Map.lookup a ps)

-- | The window a command acts in: the caller's current window, or (for
-- a clientless control command) the current window of the most-recent
-- session.
currentWindowOf :: ServerState -> Maybe Client -> IO (Maybe Window)
currentWindowOf st mclient = do
    mv <- atomically (maybe (pure Nothing) (clientView st) mclient)
    case mv of
        Just (_, win) -> pure (Just win)
        Nothing -> do
            msess <- targetSession st mclient Nothing
            case msess of
                Nothing -> pure Nothing
                Just sess -> atomically (currentWindow sess)

-- | Find a pane by its numeric id across every session and window.
findPaneById :: ServerState -> Int -> STM (Maybe Pane)
findPaneById st n = do
    sessions <- readTVar st.sessions
    panes <- fmap concat . forM (Map.elems sessions) $ \sess -> do
        ws <- readTVar sess.windows
        fmap concat . forM (Map.elems ws) $ windowPanes
    pure (List.find (\p -> rawPane p.id == n) panes)

cmdCopyMode :: CommandImpl
cmdCopyMode st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
        quit = "-q" `elem` flags
    mpane <- targetPane st mclient (lookup "-t" opts)
    case mpane of
        Nothing -> pure []
        Just pane
            | quit -> do
                atomically $ do
                    writeTVar pane.mode Nothing
                    bumpDirty st
                pure []
            | otherwise -> do
                scr <- Emu.snapshot pane.emulator
                hsize <- Emu.scrollbackLength pane.emulator
                srvOpts <- readTVarIO st.options
                let table = case srvOpts.modeKeys of
                        KeysVi -> "copy-mode-vi"
                        KeysEmacs -> "copy-mode"
                    startRow = hsize + scr.cursor.row
                    startCol = scr.cursor.col
                    state = CopyModeState
                        { cursorRow = startRow
                        , cursorCol = startCol
                        , selection = Nothing
                        , keyTable = table
                        , viewportOffY = 0
                        , numPrefix = Nothing
                        , pendingSearch = Nothing
                        , lastSearch = Nothing
                        , lastQuery = Nothing
                        }
                atomically $ do
                    writeTVar pane.mode (Just state)
                    bumpDirty st
                pure []

-- | Open the interactive command prompt on the invoking client.
-- | @command-prompt [-I initial] [-p prompt] [template]@. Opens the line
-- editor; @-I@ pre-fills it (format-expanded, so @#W@ is the window name),
-- and a @template@ has the submitted line spliced in for @%%@. Backs the
-- @,@ rename binding: @command-prompt -I "#W" "rename-window '%%'"@.
cmdCommandPrompt :: CommandImpl
cmdCommandPrompt st mclient args = do
    let (opts, _flags, pos) = parseArgs "Ip" args
        tmpl = T.unwords pos
        pfx = case lookup "-p" opts of
            Just p -> p
            Nothing
                | T.null tmpl -> ":"
                | otherwise -> "(" <> T.takeWhile (/= ' ') tmpl <> ") "
    forM_ mclient $ \client -> do
        initial <- case lookup "-I" opts of
            Nothing -> pure ""
            Just raw -> do
                env <- clientPromptEnv st client
                expandFormat st env raw
        atomically $ do
            writeTVar client.prompt (Just (Prompt.promptFor pfx initial tmpl))
            bumpDirty st
    pure []

-- | The format environment of a client's current window, for expanding a
-- @command-prompt -I@ initial string. Empty if the client has no window.
clientPromptEnv :: ServerState -> Client -> IO FormatEnv
clientPromptEnv st client = do
    mv <- atomically (clientView st client)
    case mv of
        Nothing -> pure Map.empty
        Just (sess, win) -> do
            ix <- readTVarIO sess.currentIx
            windowFormatEnv st sess ix win

-- | Open a chooser overlay on the invoking client.
openPicker :: ServerState -> Client -> Text -> Bool -> [PickerNode] -> IO ()
openPicker st client titleText isZoomed picked = atomically $ do
    writeTVar client.picker $ Just PickerState
        { title = titleText
        , roots = picked
        , cursor = 0
        , query = ""
        , searching = False
        , zoomed = isZoomed
        }
    bumpDirty st

-- | @choose-tree [-GswZ]@: a filterable tree of every session, its
-- windows and their panes; Enter switches to the chosen one. @-s@ opens
-- with sessions collapsed (sessions only), @-w@ with windows expanded but
-- panes collapsed, and neither fully expanded. The config opens it with
-- @… \; send-keys /@ to jump straight into search.
cmdChooseTree :: CommandImpl
cmdChooseTree st mclient args = do
    let (_, flags, _) = parseArgs "" args
        sessionsOnly = "-s" `elem` flags
        windowsOnly  = "-w" `elem` flags
        expandWindows = not sessionsOnly
        expandPanes   = not sessionsOnly && not windowsOnly
        isZoomed      = "-Z" `elem` flags
    forM_ mclient $ \client -> do
        picked <- buildTreeNodes st expandWindows expandPanes
        openPicker st client "choose a window" isZoomed picked
    pure []

buildTreeNodes :: ServerState -> Bool -> Bool -> IO [PickerNode]
buildTreeNodes st expandWindows expandPanes = do
    sessions <- Map.elems <$> readTVarIO st.sessions
    forM sessions $ \sess -> do
        sname <- readTVarIO sess.name
        curIx <- readTVarIO sess.currentIx
        ws <- Map.toAscList <$> readTVarIO sess.windows
        sessPreview <- case lookup curIx ws of
            Just cur -> Just <$> readTVarIO cur.activeId
            Nothing  -> pure Nothing
        winNodes <- forM ws $ \(ix, win) -> do
            wname <- readTVarIO win.name
            apid <- readTVarIO win.activeId
            ordered <- Map.elems <$> readTVarIO win.panes
            let winCmd = "switch-client -t " <> sname
                    <> " ; select-window -t " <> sname <> ":" <> tshow ix
                paneNodes =
                    [ PickerNode
                        { label = "pane " <> tshow pix
                            <> (if pane.id == apid then "*" else "")
                        , command = winCmd <> " ; select-pane -t " <> tshow pix
                        , preview = Just pane.id
                        , children = []
                        , expanded = False }
                    | (pix, pane) <- zip [0 :: Int ..] ordered ]
            pure PickerNode
                { label = tshow ix <> ":" <> wname
                , command = winCmd
                , preview = Just apid
                , children = paneNodes
                , expanded = expandPanes }
        pure PickerNode
            { label = sname
            , command = "switch-client -t " <> sname
            , preview = sessPreview
            , children = winNodes
            , expanded = expandWindows }

-- | @choose-window <template>@: a list of the current session's windows;
-- selecting one runs @template@ with each @%%@ replaced by that window's
-- active pane id, so @choose-window 'join-pane -hs \"%%\"'@ joins it here.
cmdChooseWindow :: CommandImpl
cmdChooseWindow st mclient args = do
    let (_, _, pos) = parseArgs "" args
    case (mclient, pos) of
        (Just client, template : _) -> do
            picked <- buildWindowItems st client template
            openPicker st client "choose a window" False picked
            pure []
        _ -> pure [RErr "usage: choose-window template"]

buildWindowItems :: ServerState -> Client -> Text -> IO [PickerNode]
buildWindowItems st client template = do
    sid <- readTVarIO client.session
    msess <- Map.lookup sid <$> readTVarIO st.sessions
    case msess of
        Nothing -> pure []
        Just sess -> do
            ws <- Map.toAscList <$> readTVarIO sess.windows
            forM ws $ \(ix, win) -> do
                wname <- readTVarIO win.name
                apid <- readTVarIO win.activeId
                let target = "%" <> tshow (rawPane apid)
                pure $ Picker.leaf (tshow ix <> ":" <> wname)
                    (T.replace "%%" target template)

cmdShowBuffer :: CommandImpl
cmdShowBuffer st _ args = do
    let (opts, _, _) = parseArgs "b" args
    bufs <- readTVarIO st.buffers
    pure $ case bufferBody (lookup "-b" opts) bufs of
        Nothing -> [RErr "no buffers"]
        Just body -> [ROutput body]

cmdSetBuffer :: CommandImpl
cmdSetBuffer st _ args = do
    let (opts, flags, pos) = parseArgs "bn" args
        appendMode = "-a" `elem` flags
        mname = lookup "-b" opts
        body = T.unwords pos
    if null pos
        then pure [RErr "usage: set-buffer [-a] [-b name] data"]
        else atomically $ do
            bufs <- readTVar st.buffers
            case mname of
                Just name -> do
                    let existing = lookupBuffer name bufs
                        newBody = case (appendMode, existing) of
                            (True, Just prev) -> prev <> body
                            _ -> body
                        others = Seq.filter ((/= name) . fst) bufs
                    writeTVar st.buffers ((name, newBody) Seq.<| others)
                Nothing -> do
                    n <- readTVar st.nextBuffer
                    writeTVar st.nextBuffer (n + 1)
                    let name = "buffer" <> T.pack (show n)
                    writeTVar st.buffers ((name, body) Seq.<| bufs)
            bumpDirty st
            pure []

cmdListBuffers :: CommandImpl
cmdListBuffers st _ _ = do
    bufs <- readTVarIO st.buffers
    pure . map row $ toList' bufs
  where
    row (name, body) =
        ROutput (name <> ": " <> tshow (T.length body) <> " bytes")
    toList' s = case Seq.viewl s of
        Seq.EmptyL -> []
        x Seq.:< xs -> x : toList' xs

cmdDeleteBuffer :: CommandImpl
cmdDeleteBuffer st _ args = do
    let (opts, _, _) = parseArgs "b" args
    atomically $ do
        bufs <- readTVar st.buffers
        writeTVar st.buffers (dropBuffer (lookup "-b" opts) bufs)
        pure []

-- | Write the top (or named) buffer to a file. @-a@ appends; the path
-- may start with @~/@.
cmdSaveBuffer :: CommandImpl
cmdSaveBuffer st _ args = do
    let (opts, flags, pos) = parseArgs "b" args
        appendMode = "-a" `elem` flags
    case pos of
        [] -> pure [RErr "usage: save-buffer [-a] [-b name] path"]
        (rawPath : _) -> do
            bufs <- readTVarIO st.buffers
            case bufferBody (lookup "-b" opts) bufs of
                Nothing -> pure [RErr "no buffers"]
                Just body -> do
                    path <- expandTilde (T.unpack rawPath)
                    let write = if appendMode then TIO.appendFile else TIO.writeFile
                    r <- try (write path body)
                    pure $ case r of
                        Left (e :: IOException) -> [RErr (T.pack (show e))]
                        Right () -> []

-- | Paste the top (or named) buffer into a pane's pty. @-d@ deletes the
-- buffer afterwards, @-p@ wraps it in bracketed-paste markers, @-r@
-- turns carriage returns into newlines.
cmdPasteBuffer :: CommandImpl
cmdPasteBuffer st mclient args = do
    let (opts, flags, _) = parseArgs "bt" args
        del = "-d" `elem` flags
        bracketed = "-p" `elem` flags
        crToNl = "-r" `elem` flags
        mname = lookup "-b" opts
    bufs <- readTVarIO st.buffers
    case bufferBody mname bufs of
        Nothing -> pure [RErr "no buffers"]
        Just body0 -> do
            mpane <- targetPane st mclient (lookup "-t" opts)
            case mpane of
                Nothing -> pure [RErr "no target pane"]
                Just pane -> do
                    let body = if crToNl
                            then T.map (\c -> if c == '\r' then '\n' else c) body0
                            else body0
                        payload
                            | bracketed = "\ESC[200~" <> body <> "\ESC[201~"
                            | otherwise = body
                    Hat.Pty.writePty pane.pty (TE.encodeUtf8 payload)
                    when del $ atomically $ do
                        cur <- readTVar st.buffers
                        writeTVar st.buffers (dropBuffer mname cur)
                    pure []

-- | @pipe-pane [-IOo] [-t target] [command]@. With no command (or @-o@
-- while already piping) it stops the pane's pipe. Otherwise it spawns
-- @sh -c command@: @-O@ (the default) feeds pane output to the process's
-- stdin, @-I@ feeds the process's stdout back into the pane.
cmdPipePane :: CommandImpl
cmdPipePane st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        hasI = "-I" `elem` flags
        hasO = "-O" `elem` flags
        wantO = hasO || not hasI   -- default direction is -O
        toggle = "-o" `elem` flags
        cmd = T.strip (T.unwords pos)
    mpane <- targetPane st mclient (lookup "-t" opts)
    case mpane of
        Nothing -> pure []
        Just pane -> do
            wasPiping <- isJust <$> readTVarIO pane.pipe
            stopPipe pane
            if T.null cmd || (toggle && wasPiping)
                then pure []
                else startPipe pane (T.unpack cmd) wantO hasI >> pure []

-- Spawn the pipe subprocess and record it on the pane.
startPipe :: Pane -> String -> Bool -> Bool -> IO ()
startPipe pane cmd wantO wantI = do
    (mIn, mOut, _, ph) <- createProcess (shell cmd)
        { std_in  = if wantO then CreatePipe else Inherit
        , std_out = if wantI then CreatePipe else Inherit
        }
    rtid <- case (wantI, mOut) of
        (True, Just hout) -> Just <$> forkIO (pumpPipeOutput pane hout)
        _ -> pure Nothing
    atomically $ writeTVar pane.pipe $ Just PipeHandle
        { process = ph
        , toStdin = if wantO then mIn else Nothing
        , reader = rtid
        }

-- Read the process's stdout and write it into the pane's pty (@-I@).
pumpPipeOutput :: Pane -> Handle -> IO ()
pumpPipeOutput pane hout = loop `catch` \(_ :: SomeException) -> pure ()
  where
    loop = do
        chunk <- B8.hGetSome hout 4096
        unless (B8.null chunk) $ do
            Hat.Pty.writePty pane.pty chunk
            loop

-- Feed a chunk of pane output to the pipe subprocess (@-O@).
forwardToPipe :: Pane -> B.ByteString -> IO ()
forwardToPipe pane bs = do
    mp <- readTVarIO pane.pipe
    forM_ mp $ \ph -> forM_ ph.toStdin $ \hdl ->
        (B8.hPut hdl bs >> hFlush hdl)
            `catch` \(_ :: SomeException) -> pure ()

-- Stop and reap any pipe subprocess on the pane.
stopPipe :: Pane -> IO ()
stopPipe pane = do
    mp <- atomically $ do
        m <- readTVar pane.pipe
        writeTVar pane.pipe Nothing
        pure m
    forM_ mp $ \ph -> do
        forM_ ph.reader killThread
        forM_ ph.toStdin $ \hdl ->
            hClose hdl `catch` \(_ :: SomeException) -> pure ()
        terminateProcess ph.process `catch` \(_ :: SomeException) -> pure ()
        void . forkIO $
            void (waitForProcess ph.process)
                `catch` \(_ :: SomeException) -> pure ()

-- | @HOME@-relative @~/@ prefix expansion for buffer paths.
expandTilde :: FilePath -> IO FilePath
expandTilde ('~' : '/' : rest) = do
    env <- getEnvironment
    pure $ maybe ('~' : '/' : rest) (\h -> h <> "/" <> rest) (lookup "HOME" env)
expandTilde p = pure p

-- | The top buffer, or a named one.
bufferBody :: Maybe Text -> Seq (Text, Text) -> Maybe Text
bufferBody mname bufs = case mname of
    Just name -> lookupBuffer name bufs
    Nothing -> case bufs of
        Seq.Empty -> Nothing
        (_, body) Seq.:<| _ -> Just body

-- | Drop the top buffer, or a named one.
dropBuffer :: Maybe Text -> Seq (Text, Text) -> Seq (Text, Text)
dropBuffer mname bufs = case mname of
    Just name -> Seq.filter ((/= name) . fst) bufs
    Nothing -> case bufs of
        Seq.Empty -> bufs
        _ Seq.:<| rest -> rest

lookupBuffer :: Text -> Seq (Text, Text) -> Maybe Text
lookupBuffer name = go
  where
    go s = case Seq.viewl s of
        Seq.EmptyL -> Nothing
        (n, b) Seq.:< rest
            | n == name -> Just b
            | otherwise -> go rest

cmdNewSession :: CommandImpl
cmdNewSession st mclient args = do
    let (opts, flags, pos) = parseArgs "sctnxy" args
        mname = lookup "-s" opts
        mrun = case pos of
            [] -> Nothing
            ws -> Just (T.unwords ws)
    dup <- case mname of
        Nothing -> pure False
        Just nm -> atomically $ do
            sessions <- readTVar st.sessions
            names <- mapM (\s -> readTVar s.name) (Map.elems sessions)
            pure (nm `elem` names)
    if dup
        then pure [RErr ("duplicate session: " <> fromMaybe "" mname)]
        else do
            (environ, dir, baseSz) <- case mclient of
                Just c -> do
                    csz <- readTVarIO c.size
                    pure (c.env, T.unpack c.cwd, csz)
                Nothing -> do
                    -- Config-loaded or otherwise clientless: inherit the
                    -- server process env so shells find PATH, SHELL, etc.
                    procEnv <- getEnvironment
                    pure ( [(T.pack k, T.pack v) | (k, v) <- procEnv]
                         , "/"
                         , Size { rows = 24, cols = 80 } )
            let dir' = maybe dir T.unpack (lookup "-c" opts)
                parseInt t = case TR.decimal t of
                    Right (n, rest) | T.null rest -> Just n
                    _ -> Nothing
                sz = baseSz
                    { cols = fromMaybe baseSz.cols (parseInt =<< lookup "-x" opts)
                    , rows = fromMaybe baseSz.rows (parseInt =<< lookup "-y" opts)
                    }
            sess <- createSession st mname mrun environ dir' sz
            atomically $ do
                writeTVar st.everAttached True
                forM_ (lookup "-n" opts) $ \wname -> do
                    ws <- readTVar sess.windows
                    forM_ (Map.elems ws) $ \w -> do
                        writeTVar w.name wname
                        writeTVar w.autoRename False
            unless ("-d" `elem` flags) $
                forM_ mclient $ \client -> switchClientTo st client sess
            pure []

switchClientTo :: ServerState -> Client -> Session -> IO ()
switchClientTo st client sess = do
    old <- readTVarIO client.session
    atomically $ do
        when (old /= sess.id) $ do
            writeTVar client.lastSession (Just old)
            writeTVar client.session sess.id
        writeTVar client.needsFull True
        bumpDirty st
    applySessionSize st old
    applySessionSize st sess.id

-- @-c@ re-anchors the session's default working directory for new
-- windows, so it is useful (and valid) even without a client to attach.
cmdAttachSession :: CommandImpl
cmdAttachSession st mclient args = do
    let (opts, _, _) = parseArgs "tc" args
    withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
        forM_ (lookup "-c" opts) $ \d -> do
            env <- sessionFormatEnv st sess
            dir <- T.unpack <$> expandFormat st env d
            atomically $ do
                writeTVar sess.startCwd dir
                bumpDirty st
        case mclient of
            Just client -> switchClientTo st client sess >> pure []
            Nothing
                | isJust (lookup "-c" opts) -> pure []
                | otherwise -> pure [RErr "no client to attach"]

cmdKillSession :: CommandImpl
cmdKillSession st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
        panes <- atomically $ do
            ws <- readTVar sess.windows
            fmap concat . forM (Map.elems ws) $ windowPanes
        forM_ panes $ \p -> Hat.Pty.closePty p.pty
        pure []

cmdHasSession :: CommandImpl
cmdHasSession st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    msess <- targetSession st mclient (lookup "-t" opts)
    pure $ case msess of
        Just _ -> []
        Nothing -> [RErr $ "can't find session: "
            <> fromMaybe "" (lookup "-t" opts)]

-- The server is necessarily running by the time this executes.
cmdStartServer :: CommandImpl
cmdStartServer _ _ _ = pure []

cmdRenameSession :: CommandImpl
cmdRenameSession st mclient args = do
    let (opts, _, pos) = parseArgs "t" args
    case pos of
        [nm] -> withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
            dup <- atomically $ do
                sessions <- readTVar st.sessions
                names <- mapM (\s -> readTVar s.name)
                    (filter (\s -> s.id /= sess.id) (Map.elems sessions))
                pure (nm `elem` names)
            if dup
                then pure [RErr ("duplicate session: " <> nm)]
                else do
                    atomically $ do
                        writeTVar sess.name nm
                        bumpDirty st
                    pure []
        _ -> pure [RErr "usage: rename-session [-t target] name"]

cmdListSessions :: CommandImpl
cmdListSessions st _ args = do
    let (opts, _, _) = parseArgs "F" args
    sessions <- Map.elems <$> readTVarIO st.sessions
    lines' <- case lookup "-F" opts of
        Just fmt -> forM sessions $ \sess -> do
            env <- sessionFormatEnv st sess
            expandFormat st env fmt
        Nothing -> forM sessions $ \sess -> atomically $ do
            nm <- readTVar sess.name
            ws <- readTVar sess.windows
            pure (nm <> ": " <> tshow (Map.size ws) <> " windows")
    pure (map ROutput lines')

cmdListWindows :: CommandImpl
cmdListWindows st mclient args = do
    let (opts, flags, _) = parseArgs "Ft" args
        mfmt = lookup "-F" opts
        allSessions = "-a" `elem` flags
    sessions <- if allSessions
        then Map.elems <$> readTVarIO st.sessions
        else do
            msess <- targetSession st mclient (lookup "-t" opts)
            pure (maybe [] pure msess)
    fmap (map ROutput . concat) . forM sessions $ \sess -> do
        ws <- Map.toAscList <$> readTVarIO sess.windows
        cur <- readTVarIO sess.currentIx
        forM ws $ \(ix, win) -> case mfmt of
            Just fmt -> do
                env <- windowFormatEnv st sess ix win
                expandFormat st env fmt
            Nothing -> atomically $ do
                nm <- readTVar win.name
                ps <- readTVar win.panes
                let mark = if ix == cur then "*" else ""
                pure $ tshow ix <> ": " <> nm <> mark
                    <> " (" <> tshow (Map.size ps) <> " panes)"

windowFormatEnv :: ServerState -> Session -> Int -> Window -> IO FormatEnv
windowFormatEnv st sess ix win = do
    base <- sessionFormatEnv st sess
    eff <- readTVarIO sess.lastSize
    (wname, lay, cur, bell, act, auto) <- atomically $ (,,,,,)
        <$> readTVar win.name <*> readTVar win.layout
        <*> readTVar sess.currentIx <*> readTVar win.bellFlag
        <*> readTVar win.activity <*> readTVar win.autoRename
    ps <- readTVarIO win.panes
    let flags = T.concat
            [ if ix == cur then "*" else ""
            , if bell then "!" else "", if act then "#" else "" ]
    pure $ Map.union (Map.fromList
        [ ("window_index", tshow ix)
        , ("window_name", wname)
        , ("window_layout", emitLayout (sizeRect (windowArea eff)) lay)
        , ("window_active", if ix == cur then "1" else "0")
        , ("window_flags", flags)
        , ("window_panes", tshow (Map.size ps))
        , ("automatic_rename", if auto then "1" else "0")
        ]) base

-- | The full format environment for a specific pane, including the
-- fields tmux-resurrect's @save.sh@ dumps (pid, command, cursor,
-- history, cwd).
paneFormatEnv
    :: ServerState -> Session -> Int -> Window -> Int -> Pane -> IO FormatEnv
paneFormatEnv st sess wix win pix pane = do
    wenv <- windowFormatEnv st sess wix win
    dir <- paneCurrentPath pane
    cmd <- paneCommandName pane
    scr <- Emu.snapshot pane.emulator
    hsize <- Emu.scrollbackLength pane.emulator
    active <- readTVarIO win.activeId
    sz <- readTVarIO pane.size
    pure $ Map.union (Map.fromList
        [ ("pane_id", "%" <> tshow (rawPane pane.id))
        , ("pane_index", tshow pix)
        , ("pane_pid", tshow (Hat.Pty.pid pane.pty))
        , ("pane_current_path", T.pack dir)
        , ("pane_current_command", cmd)
        , ("pane_active", if pane.id == active then "1" else "0")
        , ("cursor_x", tshow scr.cursor.col)
        , ("cursor_y", tshow scr.cursor.row)
        , ("history_size", tshow hsize)
        , ("pane_width", tshow sz.cols)
        , ("pane_height", tshow sz.rows)
        , ("session_grouped", "0")  -- hat has no session groups
        ]) wenv

cmdListPanes :: CommandImpl
cmdListPanes st mclient args = do
    let (opts, flags, _) = parseArgs "Ft" args
        mfmt = lookup "-F" opts
        allSessions = "-a" `elem` flags
    -- Which (session, window-index, window) triples to list panes from:
    -- -a covers every window of every session; otherwise the target
    -- session's current window.
    targets <- if allSessions
        then do
            sessions <- Map.elems <$> readTVarIO st.sessions
            fmap concat . forM sessions $ \sess -> do
                ws <- Map.toAscList <$> readTVarIO sess.windows
                pure [ (sess, ix, win) | (ix, win) <- ws ]
        else do
            msess <- targetSession st mclient (lookup "-t" opts)
            case msess of
                Nothing -> pure []
                Just sess -> do
                    cur <- readTVarIO sess.currentIx
                    mwin <- atomically (currentWindow sess)
                    pure [ (sess, cur, win) | win <- maybe [] pure mwin ]
    fmap (map ROutput . concat) . forM targets $ \(sess, wix, win) -> do
        ps <- Map.elems <$> readTVarIO win.panes
        forM (zip [0 ..] ps) $ \(pix, pane) -> case mfmt of
            Just fmt -> do
                env <- paneFormatEnv st sess wix win pix pane
                expandFormat st env fmt
            Nothing -> do
                sz <- readTVarIO pane.size
                pure $ "%" <> tshow (rawPane pane.id) <> ": ["
                    <> tshow sz.cols <> "x" <> tshow sz.rows <> "]"

cmdSwitchClient :: CommandImpl
cmdSwitchClient st mclient args = do
    let (opts, flags, _) = parseArgs "t" args
    case mclient of
        Nothing -> pure [RErr "no client"]
        Just client
            | "-l" `elem` flags -> do
                mlast <- readTVarIO client.lastSession
                sessions <- readTVarIO st.sessions
                case mlast >>= (`Map.lookup` sessions) of
                    Nothing -> pure [RErr "no last session"]
                    Just sess -> switchClientTo st client sess >> pure []
            | otherwise ->
                withTargetSession st mclient (lookup "-t" opts) $ \sess ->
                    switchClientTo st client sess >> pure []

cmdKillServer :: CommandImpl
cmdKillServer st mclient _ = do
    -- Flag first: pane readers race us into closePane once the ptys go,
    -- and the shutdown path must know this drain is a kill, not the last
    -- window closing (which drops the store instead).
    atomically $ writeTVar st.preserveStore True
    saveNow st  -- capture the tree before we tear it down
    sessions <- readTVarIO st.sessions
    forM_ (Map.keys sessions) $ \sid -> broadcast st sid Exited
    forM_ mclient $ \client -> send client Exited
    panes <- atomically $ do
        sess <- readTVar st.sessions
        fmap concat . forM (Map.elems sess) $ \s -> do
            ws <- readTVar s.windows
            fmap concat . forM (Map.elems ws) $ windowPanes
    forM_ panes $ \p -> Hat.Pty.closePty p.pty
    atomically $ do
        writeTVar st.sessions Map.empty
        writeTVar st.everAttached True
    pure []

cmdDisplayMessage :: CommandImpl
cmdDisplayMessage st mclient args = do
    let (_, flags, pos) = parseArgs "t" args
        raw = T.unwords pos
    msess <- targetSession st mclient Nothing
    text <- case msess of
        Nothing -> pure raw
        Just sess -> do
            env <- sessionFormatEnv st sess
            expandFormat st env raw
    if "-p" `elem` flags
        then pure [ROutput text]
        else do
            forM_ mclient $ \client -> showToast st client text
            pure []

cmdRunShell :: CommandImpl
cmdRunShell st mclient args = do
    let (_, _, pos) = parseArgs "t" args
        cmdText = T.unwords pos
    void . forkIO $ do
        (code, out, errOut) <- readCreateProcessWithExitCode
            (shell (T.unpack cmdText)) { close_fds = True } ""
        let firstLine = T.strip . T.takeWhile (/= '\n') . T.pack
        case code of
            ExitSuccess ->
                forM_ mclient $ \client ->
                    unless (null out) $ showToast st client (firstLine out)
            ExitFailure n ->
                forM_ mclient $ \client ->
                    showToast st client $
                        "run-shell exited " <> tshow n <> ": "
                        <> firstLine (out <> errOut)
    pure []

cmdIfShell :: CommandImpl
cmdIfShell st mclient args = do
    let (_, _, pos) = parseArgs "t" args
    case pos of
        (cond0 : thenCmd : rest) -> do
            -- tmux expands #{...} in the condition before running it, so
            -- @if-shell '[ "#{@pane-theme}" = dark ]' …@ works.
            msess <- targetSession st mclient Nothing
            cond <- case msess of
                Just sess -> do
                    env <- sessionFormatEnv st sess
                    expandFormat st env cond0
                Nothing -> pure cond0
            (code, _, _) <- readCreateProcessWithExitCode
                (shell (T.unpack cond)) { close_fds = True } ""
            let chosen = case (code, rest) of
                    (ExitSuccess, _) -> Just thenCmd
                    (ExitFailure _, [elseCmd]) -> Just elseCmd
                    _ -> Nothing
            case chosen of
                Nothing -> pure []
                Just cmdText -> runCommandText st mclient cmdText
        _ -> pure [RErr "usage: if-shell condition command [command]"]

-- Control clients ------------------------------------------------------------

controlLoop :: ServerState -> Client -> IO ()
controlLoop st client = do
    m <- recvMessage client.sock
    case m of
        Just (Right (Command cmds)) -> do
            replies <- runCommands st (Just client) cmds
            forM_ replies $ \case
                ROutput out -> send client (Message out)
                RErr e -> send client (ServerError e)
            send client CommandDone
            controlLoop st client
        Just (Right Detach) -> pure ()
        Nothing -> pure ()
        _ -> controlLoop st client

-- Helpers ------------------------------------------------------------

rawClient :: ClientId -> Int
rawClient (ClientId n) = n

rawPane :: PaneId -> Int
rawPane (PaneId n) = n

rawSession :: SessionId -> Int
rawSession (SessionId n) = n

tshow :: Show a => a -> Text
tshow = T.pack . show
