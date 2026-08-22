-- | A pane's whole life cycle: spawning its process and the reader thread
-- that pumps its output, the pipe subsystem behind @pipe-pane@, the
-- attention markers (bell/activity/focus), and the detach/reap/close
-- teardown a kill or a natural exit runs — including the re-parenting that
-- keeps a mobile pane's teardown honest.
module Hat.Server.Pane
    ( PaneStart (..)
    , SpawnOrigin (..)
    , DetachResult (..)
    , SessionFate (..)
    , OutputTap (..)
    , StdinFeed (..)
    , restoreEnv
    , defaultRestoreCommands
    , restoreWhitelist
    , restoreRun
    , resumeArgv
    , commandName
    , shellLine
    , shellStart
    , spawnPane
    , newWindowWithPane
    , startPaneReader
    , propKindLabel
    , notifyColorScheme
    , oscColorReply
    , forwardToPipe
    , startPipe
    , clearPipe
    , pumpPipeOutput
    , stopPipe
    , isCurrentWindow
    , attentionSeen
    , windowSeen
    , markActivity
    , windowActivity
    , markBell
    , noteOuterFocus
    , paneExitWaitMicros
    , hangupPane
    , detachPaneCurrent
    , closePane
    , detachPane
    , reapPane
    , detachPanes
    , killPaneLocs
    , killPaneLocsWith
    , LifecycleNotify (..)
    , WindowFate (..)
    , pickActivityTarget
    , removePaneFromTree
    , wrapPaneInWindow
    , paneCommandName
    , globalSpawnEnv
    , createSession
    , sessionSpawnEnv
    ) where

import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Exception (IOException, catch, finally, try)
import Control.Monad (forM, forM_, forever, unless, void, when)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Char (isAlphaNum)
import System.Environment (getEnvironment)
import System.IO (Handle, hFlush)
import System.Posix.Process (getProcessID)
import System.Process
    (CreateProcess (..), StdStream (..), shell, withCreateProcess)
import System.Timeout (timeout)

import Hat.Geometry
import Hat.Log
import Hat.Model
import Hat.Model.Options
import Hat.Server.ClientIO (broadcast)
import Hat.Server.ColorScheme (ColorScheme (..), schemeReport)
import Hat.Server.Environ (environFromPairs, environMerge, environPairs)
import Hat.Server.Keys
import Hat.Server.Layout
import Hat.Server.Hooks
    ( NotifyTarget (..), PayloadItem (..), noTarget, notify, notifyPane
    , sessionTarget )
import Hat.Server.Locate (locatePane)
import Hat.Server.Mru (popOnClose, scrub)
import Hat.Server.Resize (applySessionSize)
import Hat.Term.Emulator qualified as Emu
import Hat.Term.Pty qualified
import Hat.Transport.Wire

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
    , "top", "htop", "atop", "btop"
    , "claude" ]

restoreWhitelist :: ServerState -> IO [Text]
restoreWhitelist st = do
    opts <- readTVarIO st.options
    pure $ case Map.lookup "@restore-commands" opts.user of
        Just v | not (T.null (T.strip v)) -> T.words v
        _                                 -> defaultRestoreCommands

-- | Re-run the captured argv only when its program is whitelisted; otherwise
-- the pane comes back as a plain shell. The argv is passed through unsplit.
restoreRun :: [Text] -> SpawnOrigin -> Maybe [Text] -> PaneStart
restoreRun whitelist origin mcmd = case mcmd of
    Just argv@(prog : _) | commandName prog `elem` whitelist ->
        let resumed = resumeArgv (commandName prog) argv
        in case origin of
            -- Started from the pane's shell: type it into a fresh shell, which
            -- stays as the live parent (its rc runs; Ctrl-z drops back to it).
            ShellSpawned -> ShellInject resumed
            -- Launched directly: exec bare, so @vim "Foo Bar.txt"@ isn't re-split.
            Direct       -> ExecArgv resumed
    _ -> FreshShell

-- | The argv a whitelisted program is re-exec'd with. Most programs get
-- their captured argv back verbatim, but claude's saved arguments name a
-- past invocation, not the running conversation — only @claude --continue@
-- resumes it, so the arguments are rewritten to that regardless of what
-- was saved. The captured program path (argv[0]) is kept, so the exact
-- binary re-execs.
resumeArgv :: Text -> [Text] -> [Text]
resumeArgv "claude" (prog : _) = [prog, "--continue"]
resumeArgv _        argv       = argv

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
    paneHistVar <- newTVarIO []
    bellVar <- newTVarIO False
    activityVar <- newTVarIO False
    zoomVar <- newTVarIO Nothing
    autoRenameVar <- newTVarIO . (.automaticRename) =<< readTVarIO st.options
    layoutNameVar <- newTVarIO Nothing
    optionsVar    <- newTVarIO emptyDelta
    silenceVar    <- newTVarIO False
    activityAtVar <- newTVarIO =<< getPOSIXTime
    let win = Window
            { id = WindowId wid
            , name = nameVar
            , layout = layoutVar
            , layoutName = layoutNameVar
            , panes = panesVar
            , activeId = activeVar
            , paneHist = paneHistVar
            , bellFlag = bellVar
            , activity = activityVar
            , silenceFlag = silenceVar
            , activityAt = activityAtVar
            , zoomed = zoomVar
            , autoRename = autoRenameVar
            , options = optionsVar
            }
    pure (win, pane)
  where
    baseName = Prelude.reverse . takeWhile (/= '/') . Prelude.reverse

-- | How a freshly-spawned pane chooses its process.
data PaneStart
    = FreshShell         -- ^ the session's login shell, no command
    | ShellCommand Text  -- ^ a user-supplied command line, run via @sh -c@
                         --   (@new-window@\/@split-window@ with an argument)
    | ExecArgv [Text]    -- ^ exec this argv directly, no shell. See
                         --   'restoreRun'.
    | ShellInject [Text] -- ^ spawn a fresh interactive shell and type this
                         --   argv into it as a command line. See 'restoreRun'.
    deriving (Eq, Show)

-- | Where a captured program was running: as a child of the pane's
-- interactive shell (the user typed it), or as the pane's own top-level
-- process (a pane launched directly on a program). See 'restoreRun'.
data SpawnOrigin = ShellSpawned | Direct
    deriving (Eq, Show)

-- | The shell-command spawn semantics for the user-facing @new-window@ and
-- @split-window@: an argument is a command line for the shell to interpret.
shellStart :: Maybe Text -> PaneStart
shellStart = maybe FreshShell ShellCommand

-- | Render an argv as one shell command line: each argument quoted so a single
-- round of shell word-splitting reproduces it exactly (a filename with spaces
-- stays one argument). See 'startPaneReader', which types the result.
shellLine :: [Text] -> Text
shellLine = T.unwords . map shellQuote
  where
    shellQuote arg
        | not (T.null arg) && T.all safe arg = arg
        | otherwise = "'" <> T.replace "'" "'\\''" arg <> "'"
    safe c = isAlphaNum c || c `elem` ("_-./:=@%+," :: String)

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
            FreshShell             -> (shellCmd, [])
            ShellCommand run       -> ("/bin/sh", ["-c", T.unpack run])
            ExecArgv (p:rest)      -> (T.unpack p, map T.unpack rest)
            ExecArgv []            -> (shellCmd, [])
            -- A fresh shell like any other pane; the argv rides as pending
            -- input the reader types once the shell prints (see startPaneReader).
            ShellInject _          -> (shellCmd, [])
        pending = case mrun of
            ShellInject argv@(_ : _) -> Just (shellLine argv)
            _                        -> Nothing
    pty <- Hat.Term.Pty.spawn Hat.Term.Pty.Spawn
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
    readerVar <- newTVarIO Nothing
    optionsVar <- newTVarIO emptyDelta
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
        , options = optionsVar
        , pipe = pipeVar
        , readerTid = readerVar
        , pendingInput = pending
        }

-- | The reader thread owns a pane's lifetime: it pumps pty output into
-- the emulator until end-of-file, and 'closePane' runs in a @finally@ so
-- the pane's resources and model entry are released however the loop ends
-- — clean EOF, a hang-up from a kill command, or an exception.
--
-- The @sid@\/@win@ are the pane's window AT SPAWN. They drive only the
-- best-effort attention markers (bell\/activity) and desktop-notification
-- routing, where a stale window after a re-parent is a harmless misroute — not
-- worth a tree scan on every screen change. Teardown must NOT use them: a
-- re-parented pane's 'closePane' finds its CURRENT window (see
-- 'detachPaneCurrent'), or it would strand the pane in the wrong window.
startPaneReader :: ServerState -> SessionId -> Window -> Pane -> IO ()
startPaneReader st sid win pane = do
    -- Count the pane live before forking, so a kill that lands before the
    -- reader is scheduled still finds it counted and 'waitIdle' waits for
    -- its reap. Decremented once the reader (and its 'reapPane') is done.
    atomically $ modifyTVar' st.livePanes (+ 1)
    void . forkIO $ do
        tid <- myThreadId
        atomically $ writeTVar pane.readerTid (Just tid)
        readLoop pane.pendingInput
            `finally` paneEof st pane
            `finally` atomically (modifyTVar' st.livePanes (subtract 1))
  where
    -- @pending@ is a one-shot: a restored program's command line, typed into
    -- the pane the moment its shell first prints (so readline is up to read
    -- it), then dropped. Enter is a bare CR, as a real keyboard sends.
    readLoop pending = do
        bs <- Hat.Term.Pty.readPty pane.pty
        unless (B8.null bs) $ do
            pending' <- case pending of
                Just line -> Nothing <$
                    Hat.Term.Pty.writePty pane.pty (TE.encodeUtf8 (line <> "\r"))
                Nothing   -> pure Nothing
            forwardToPipe pane bs
            events <- Emu.feed pane.emulator bs
            forM_ events $ \case
                Emu.Output out -> Hat.Term.Pty.writePty pane.pty out
                -- The app asked the current light/dark scheme (CSI ? 996 n);
                -- answer from the OS scheme the server already tracks.
                Emu.ColorSchemeQuery -> do
                    mscheme <- readTVarIO st.colorScheme
                    forM_ mscheme (notifyColorScheme pane)
                -- The app asked its terminal's fg/bg color (OSC 10/11 ;?),
                -- the classic light-versus-dark probe; hat doesn't paint a
                -- pane background, so answer with the canonical color for
                -- the OS scheme it tracks (white-on-black when dark).
                Emu.OscColorQuery target term -> do
                    mscheme <- readTVarIO st.colorScheme
                    forM_ mscheme $ \scheme ->
                        Hat.Term.Pty.writePty pane.pty (oscColorReply target term scheme)
                -- The app raised a desktop notification (OSC 9/777); hat has
                -- no notification UI of its own, so forward it verbatim to
                -- the session's attached terminals to raise with the OS.
                Emu.DesktopNotification raw -> broadcast st sid (Notify raw)
                -- The pane's own OSC title only feeds #{pane_title} (the
                -- emulator stores it); the client's desktop title is
                -- composed in 'refreshTitles'.
                Emu.TitleChanged t ->
                    notifyPane st "pane-title-changed" pane
                        [ ("new_title", PText t) ]
                Emu.Bell -> do
                    fires <- atomically $ do
                        r <- bellAlerts st win
                        bumpDirty st
                        pure r
                    broadcast st sid RingBell
                    notifyPane st "pane-bell" pane []
                    forM_ fires $ \(osid, sname, wname) ->
                        notify st "alert-bell"
                            (NotifyTarget (Just osid) (Just win.id) Nothing)
                            [ ("session", PSessionRef osid sname)
                            , ("window", PWindowRef win.id wname) ]
                Emu.ScreenChanged -> windowActivity st sid win
                -- the emulator reported a terminal property hat does not act on;
                -- surface it as a warning rather than silently dropping it.
                Emu.UnknownProp kind prop ->
                    logEvent st.logger UnknownTermProp
                        { pane = rawPane pane.id
                        , propKind = propKindLabel kind
                        , prop = prop
                        }
                -- A tmux passthrough payload hat neither answers nor forwards
                -- (OSC 52 clipboard, OSC 12 cursor color, OSC 4 palette, …);
                -- log it rather than discard it silently (allow-passthrough
                -- off would drop it with no trace).
                Emu.UnhandledPassthrough raw ->
                    logEvent st.logger UnhandledPassthrough
                        { pane = rawPane pane.id
                        , payload = T.pack (show raw)
                        }
            readLoop pending'

-- | Name a terminal prop's value kind for the 'UnknownTermProp' log.
propKindLabel :: Emu.PropKind -> Text
propKindLabel = \case
    Emu.PropBool -> "bool"
    Emu.PropInt  -> "int"
    Emu.PropStr  -> "str"

-- | Whether @win@ is the session's current (foreground) window.
isCurrentWindow :: Session -> Window -> STM Bool
isCurrentWindow sess win = do
    cur <- readTVar sess.currentIx
    ws <- readTVar sess.windows
    pure $ maybe False (\w -> w.id == win.id) (Map.lookup cur ws)

-- | Whether a window counts as being watched, so its attention marker is
-- suppressed. A window is "seen" only when it is the session's current
-- window AND some attached client viewing it has its outer terminal focused
-- (?1004). If every viewer's outer terminal is unfocused — the user has
-- switched to another OS window — even the current window is unwatched and
-- must still flag activity/bell.
attentionSeen :: Bool -> Bool -> Bool
attentionSeen isCurrent anyViewerFocused = isCurrent && anyViewerFocused

-- | Resolve 'attentionSeen' against live state: whether @win@ is the
-- session's current window and any attached client has outer focus.
windowSeen :: ServerState -> Session -> Window -> STM Bool
windowSeen st sess win = do
    current <- isCurrentWindow sess win
    cs <- sessionClients st sess.id
    anyFocused <- or <$> mapM (readTVar . (.outerFocused)) cs
    pure (attentionSeen current anyFocused)

-- | Register activity on a window: restart its silence timer, raise the
-- activity flag, and fire alert-activity when the flag newly rose on a
-- non-current window. Output and window creation both land here.
windowActivity :: ServerState -> SessionId -> Window -> IO ()
windowActivity st sid win = do
    now <- getPOSIXTime
    mfire <- atomically $ do
        writeTVar win.activityAt now
        r <- activityAlert st sid win
        bumpDirty st
        pure r
    forM_ mfire $ \(sname, wname) ->
        notify st "alert-activity"
            (NotifyTarget (Just sid) (Just win.id) Nothing)
            [ ("session", PSessionRef sid sname)
            , ("window", PWindowRef win.id wname) ]

-- | 'markBell' plus the alert decision, once per session holding the
-- window: the sessions whose @alert-bell@ should fire (monitor-bell on and
-- bell-action admits the window there). Bells are not deduplicated — every
-- bell reports.
bellAlerts :: ServerState -> Window -> STM [(SessionId, Text, Text)]
bellAlerts st win = do
    sessions <- Map.toAscList <$> readTVar st.sessions
    fmap concat . forM sessions $ \(osid, sess) -> do
        ws <- readTVar sess.windows
        if not (any (\w -> w.id == win.id) (Map.elems ws))
            then pure []
            else do
                wopts <- resolveForWindow st sess win
                if not wopts.monitorBell then pure [] else do
                    seen <- windowSeen st sess win
                    unless seen $ writeTVar win.bellFlag True
                    isCur <- isCurrentWindow sess win
                    if alertAllows wopts.bellAction isCur
                        then do
                            sname <- readTVar sess.name
                            wname <- readTVar win.name
                            pure [(osid, sname, wname)]
                        else pure []

-- | 'markActivity' plus the alert decision: names when @alert-activity@
-- should fire — only on the flag's off-to-on transition, and never for the
-- current window (tmux's default @activity-action other@).
activityAlert :: ServerState -> SessionId -> Window -> STM (Maybe (Text, Text))
activityAlert st sid win = do
    msess <- Map.lookup sid <$> readTVar st.sessions
    case msess of
        Nothing -> pure Nothing
        Just sess -> do
            wopts <- resolveForWindow st sess win
            if not wopts.monitorActivity then pure Nothing else do
                was <- readTVar win.activity
                seen <- windowSeen st sess win
                unless seen $ writeTVar win.activity True
                nowSet <- readTVar win.activity
                isCur <- isCurrentWindow sess win
                if not was && nowSet && not isCur
                    then do
                        sname <- readTVar sess.name
                        wname <- readTVar win.name
                        pure (Just (sname, wname))
                    else pure Nothing

-- | Flag a window as having activity, when @monitor-activity@ is on. A
-- window being watched ('windowSeen') is exempt — no attention marker
-- belongs on what the user is looking at (mirrors 'markBell', which exempts
-- a watched window the same way).
markActivity :: ServerState -> SessionId -> Window -> STM ()
markActivity st sid win = do
    msess <- Map.lookup sid <$> readTVar st.sessions
    forM_ msess $ \sess -> do
        opts <- resolveForWindow st sess win
        when opts.monitorActivity $ do
            seen <- windowSeen st sess win
            unless seen $ writeTVar win.activity True

-- | Raise a window's bell marker. A window being watched ('windowSeen') is
-- exempt — you are already looking at it — so only a bell you are not
-- watching leaves a visible marker; the audible bell rings regardless (see
-- the 'Emu.Bell' handler).
markBell :: ServerState -> SessionId -> Window -> STM ()
markBell st sid win = do
    msess <- Map.lookup sid <$> readTVar st.sessions
    forM_ msess $ \sess -> do
        seen <- windowSeen st sess win
        unless seen $ writeTVar win.bellFlag True

-- | Track a client's outer-terminal focus from its ?1004 reports. FocusOut
-- means the user looked away, so activity/bell on the window it is viewing
-- will start flagging (see 'windowSeen'). FocusIn means they are looking
-- again: clear the marker on the window the client now views. Non-focus
-- keys are ignored.
noteOuterFocus :: ServerState -> Client -> Key -> IO ()
noteOuterFocus st client k
    | k.name == "FocusOut" = atomically $ writeTVar client.outerFocused False
    | k.name == "FocusIn"  = atomically $ do
        writeTVar client.outerFocused True
        sid <- readTVar client.session
        msess <- Map.lookup sid <$> readTVar st.sessions
        forM_ msess $ \sess -> do
            mwin <- currentWindow sess
            forM_ mwin $ \win -> do
                writeTVar win.activity False
                writeTVar win.bellFlag False
        bumpDirty st
    | otherwise = pure ()

-- The grace a hung-up child gets before SIGKILL: long enough for a
-- well-behaved child to die on SIGHUP, short enough that a shutdown never
-- stalls on a stubborn one. See 'closePane'.
paneExitWaitMicros :: Int
paneExitWaitMicros = 400_000

-- | Tear a pane down from an external command (kill-pane/window/session,
-- kill-server) without deadlocking. 'closePty' would block closing the
-- master Handle while the reader still holds it in a read that a
-- SIGHUP-ignoring child never ends; instead SIGHUP the child, then
-- interrupt the reader so its blocked read returns and its @finally@ runs
-- 'closePane' (which releases the master Handle and the rest). Before the
-- reader has recorded its id (a brief spawn race) fall back to 'closePty'.
hangupPane :: Pane -> IO ()
hangupPane pane = do
    Hat.Term.Pty.signalHangup pane.pty
    mtid <- readTVarIO pane.readerTid
    case mtid of
        Just tid -> killThread tid
        Nothing  -> Hat.Term.Pty.closePty pane.pty

-- | The reader saw EOF: with @remain-on-exit@ on, the pane stays in the
-- tree showing its last screen (reaped, @pane-died@ fired) until a kill;
-- otherwise it is torn down ('closePane').
paneEof :: ServerState -> Pane -> IO ()
paneEof st pane = do
    keep <- do
        mloc <- atomically (locatePane st pane.id)
        case mloc of
            Nothing -> pure False
            Just (sid, win) -> do
                msess <- Map.lookup sid <$> readTVarIO st.sessions
                case msess of
                    Nothing -> pure False
                    Just sess -> (.remainOnExit)
                        <$> atomically (resolveForPane st sess win pane)
    if keep
        then do
            reapPane st pane
            atomically (writeTVar pane.readerTid Nothing)
            notifyPane st "pane-died" pane []
        else closePane st pane

-- | The model half of the reader thread's teardown: detach the pane from the
-- window it lives in NOW. A pane is mobile — break-pane, join-pane, and
-- swap-pane re-parent a live pane after its reader started — so teardown must
-- find the pane's current window ('locatePane') rather than the one it was
-- spawned in, or it detaches from the wrong window and strands the pane. Yields
-- the session detached from (for the reflow and the emptied-session notice), or
-- 'Nothing' when a killing command already removed the pane from the tree.
detachPaneCurrent :: ServerState -> Pane -> STM (Maybe SessionId, DetachResult)
detachPaneCurrent st pane = do
    mloc <- locatePane st pane.id
    case mloc of
        Just (sid, win) -> (,) (Just sid) <$> detachPane st sid win pane
        Nothing         -> pure (Nothing, AlreadyDetached)

-- | Tear a pane down from the reader thread's @finally@: the model detach
-- (idempotent — a killing command may already have done it, see
-- 'detachPane') followed by the OS-side reap, which runs exactly once here
-- so no teardown path can forget a resource. (The emulator frees itself via
-- its finalizer.)
closePane :: ServerState -> Pane -> IO ()
closePane st pane = do
    mctx <- atomically $ do
        mloc <- locatePane st pane.id
        traverse
            (\(sid, win) -> do
                wname <- readTVar win.name
                msess <- Map.lookup sid <$> readTVar st.sessions
                sname <- maybe (pure "") (\sess -> readTVar sess.name) msess
                pure (sid, sname, win, wname))
            mloc
    (msid, r) <- atomically (detachPaneCurrent st pane)
    forM_ msid $ \sid -> when (r /= AlreadyDetached) $ applySessionSize st sid
    -- End-of-life hooks fire only when the reader's own teardown detached
    -- the pane — a killing command detached it first, and kills fire their
    -- own notifications ('killPaneLocs'). Order: pane, window, session.
    case (r, mctx) of
        (Detached wf sf, Just (sid, sname, win, wname)) -> do
            notify st "pane-exited"
                (NotifyTarget (Just sid) (Just win.id) (Just pane.id))
                [ ("pane", PPaneRef pane.id)
                , ("window", PWindowRef win.id wname) ]
            when (wf == WindowRemoved) $ do
                notify st "window-unlinked"
                    (sessionTarget sid)
                    [ ("session", PSessionRef sid sname)
                    , ("window", PWindowRef win.id wname) ]
                notify st "window-closed" (sessionTarget sid)
                    [ ("window", PWindowRef win.id wname) ]
            when (sf == SessionEmptied) $ notify st "session-closed"
                noTarget
                [ ("session", PSessionRef sid sname) ]
        (Detached _ _, Nothing) -> notify st "pane-exited"
            (NotifyTarget Nothing Nothing (Just pane.id))
            [ ("pane", PPaneRef pane.id) ]
        (AlreadyDetached, _) -> pure ()
    reapPane st pane
    forM_ msid $ \sid ->
        when (r == Detached WindowRemoved SessionEmptied) $
            broadcast st sid Exited

-- | The model half of a pane's teardown, in one atomic transaction: drop
-- the pane from its window's map and layout, reactivate a surviving pane,
-- and collapse an emptied window (and session) upward. Guarded by
-- @pane.dead@, so of the two racers that may both attempt it — the killing
-- command ('cmdKillPane', for a reflow synchronous with the command) and
-- the reader thread's @finally@ — exactly one performs it under ANY
-- interleaving; the loser sees 'AlreadyDetached' and must not touch the
-- model (a second removal would find the leaf gone and wrongly collapse
-- the window). Deliberately free of any OS-side effect: the child's
-- lifetime is 'reapPane''s business, so the screen never waits on a
-- process.
detachPane :: ServerState -> SessionId -> Window -> Pane -> STM DetachResult
detachPane st sid win pane = do
    isDead <- readTVar pane.dead
    if isDead then pure AlreadyDetached else do
        writeTVar pane.dead True
        modifyTVar' win.panes (Map.delete pane.id)
        lay <- readTVar win.layout
        mz <- readTVar win.zoomed
        when (mz == Just pane.id) $ writeTVar win.zoomed Nothing
        case removeLeaf pane.id lay of
            Just lay' -> do
                writeTVar win.layout lay'
                let survivors = layoutPanes lay'
                active <- readTVar win.activeId
                hist <- readTVar win.paneHist
                if active == pane.id
                    then forM_ (popOnClose (`elem` survivors) (listToMaybe survivors) hist) $
                            \(next, hist') -> do
                                writeTVar win.activeId next
                                writeTVar win.paneHist hist'
                    else writeTVar win.paneHist (scrub (/= pane.id) hist)
                bumpDirty st
                pure (Detached WindowSurvives SessionSurvives)
            Nothing -> do
                msess <- Map.lookup sid <$> readTVar st.sessions
                case msess of
                    Nothing -> pure (Detached WindowRemoved SessionSurvives)
                    Just sess -> do
                        ws <- readTVar sess.windows
                        let ws' = Map.filter (\w -> w.id /= win.id) ws
                        writeTVar sess.windows ws'
                        if Map.null ws'
                            then do
                                modifyTVar' st.sessions (Map.delete sid)
                                pure (Detached WindowRemoved SessionEmptied)
                            else do
                                cur <- readTVar sess.currentIx
                                hist <- readTVar sess.windowHist
                                let survivors = Map.keysSet ws'
                                if Set.member cur survivors
                                    then writeTVar sess.windowHist
                                            (scrub (`Set.member` survivors) hist)
                                    else forM_ (popOnClose (`Set.member` survivors)
                                                    (Set.lookupMin survivors) hist) $
                                            \(ix, hist') -> do
                                                writeTVar sess.currentIx ix
                                                writeTVar sess.windowHist hist'
                                bumpDirty st
                                pure (Detached WindowRemoved SessionSurvives)

-- | What 'detachPane' did. See 'detachPane'.
data DetachResult = AlreadyDetached | Detached WindowFate SessionFate
    deriving (Eq, Show)

-- | Whether a detach removed the pane's whole window. See 'detachPane'.
data WindowFate = WindowSurvives | WindowRemoved
    deriving (Eq, Show)

-- | Whether a detach emptied the pane's whole session. See 'detachPane'.
data SessionFate = SessionSurvives | SessionEmptied
    deriving (Eq, Show)

-- | The OS half of a pane's teardown: stop any pipe-pane, wait for the
-- child — bounded, since a child that ignores SIGHUP would never exit on
-- its own; on timeout escalate to SIGKILL and wait again (the reaper
-- always fills the exit slot, so the second wait is guaranteed to
-- complete) — then release the pty. Runs once, in the reader thread's
-- @finally@ (see 'closePane').
reapPane :: ServerState -> Pane -> IO ()
reapPane st pane = do
    stopPipe pane
    reaped <- timeout paneExitWaitMicros (Hat.Term.Pty.waitExit pane.pty)
    when (reaped == Nothing) $ do
        Hat.Term.Pty.signalKill pane.pty
        void $ Hat.Term.Pty.waitExit pane.pty
    Hat.Term.Pty.closePty pane.pty
    logEvent st.logger PaneExited { pane = rawPane pane.id }

-- | Detach a whole set of located panes in one transaction, so a window —
-- and a session emptied with it — collapses atomically. Returns the
-- sessions the detach emptied (their clients need an @Exited@). The model
-- primitive behind kill-window\/-session\/-server; see 'killPaneLocs' for
-- the surrounding OS teardown and 'detachPane' for the per-pane guard.
detachPanes
    :: ServerState -> [(SessionId, Window, Pane)]
    -> STM [((SessionId, Window), DetachResult)]
detachPanes st = mapM detach
  where
    detach (sid, win, pane) =
        (,) (sid, win) <$> detachPane st sid win pane

-- | Kill a set of located panes: detach them from the model in one
-- transaction (the reflow is synchronous with the command and never waits
-- on signal delivery, child exit, or the reader's reap), redraw the
-- sessions that survive, tell the clients of any emptied session, and hang
-- the children up for their reader threads to reap ('closePane'). The one
-- IO primitive behind kill-pane\/-window\/-session.
killPaneLocs :: ServerState -> [(SessionId, Window, Pane)] -> IO ()
killPaneLocs st = killPaneLocsWith NotifyLifecycle st

-- | Whether a kill announces the windows and sessions it destroys.
-- @kill-session@ orders its own notifications (session first), so it kills
-- quietly. See 'killPaneLocsWith'.
data LifecycleNotify = NotifyLifecycle | QuietLifecycle
    deriving (Eq, Show)

killPaneLocsWith
    :: LifecycleNotify -> ServerState -> [(SessionId, Window, Pane)] -> IO ()
killPaneLocsWith mode st locs = do
    (results, names, preActive) <- atomically $ do
        names <- forM locs $ \(sid, win, _) -> do
            msess <- Map.lookup sid <$> readTVar st.sessions
            sname <- maybe (pure "") (\sess -> readTVar sess.name) msess
            wname <- readTVar win.name
            pure ((sid, win.id), (sname, wname))
        pre <- forM locs $ \(_, win, _) ->
            (,) win.id <$> readTVar win.activeId
        rs <- detachPanes st locs
        pure (rs, Map.fromList names, Map.fromList pre)
    forM_ (List.nub [sid | (sid, _, _) <- locs]) (applySessionSize st)
    let removedWindows = List.nubBy (\(s1, w1) (s2, w2) ->
                s1 == s2 && w1.id == w2.id)
            [ (sid, win) | ((sid, win), Detached WindowRemoved _) <- results ]
        emptied = List.nub
            [ sid | ((sid, _), Detached _ SessionEmptied) <- results ]
    when (mode == NotifyLifecycle) $ do
        forM_ removedWindows $ \(sid, win) -> do
            let (sname, wname) =
                    Map.findWithDefault ("", "") (sid, win.id) names
            notify st "window-unlinked" (sessionTarget sid)
                [ ("session", PSessionRef sid sname)
                , ("window", PWindowRef win.id wname) ]
            notify st "window-closed" (sessionTarget sid)
                [ ("window", PWindowRef win.id wname) ]
        forM_ emptied $ \sid -> do
            let sname = maybe "" fst (List.lookup sid
                    [ (s, nm) | ((s, _), nm) <- Map.toList names ])
            notify st "session-closed" noTarget
                [ ("session", PSessionRef sid sname) ]
    -- A surviving window whose active pane was killed reports the switch.
    when (mode == NotifyLifecycle) $ do
        let survivors = List.nubBy (\(_, w1) (_, w2) -> w1.id == w2.id)
                [ (sid, win)
                | ((sid, win), Detached WindowSurvives _) <- results ]
        forM_ survivors $ \(sid, win) -> do
            newActive <- readTVarIO win.activeId
            forM_ (Map.lookup win.id preActive) $ \old ->
                when (old /= newActive) $ do
                    let wname = maybe "" snd
                            (Map.lookup (sid, win.id) names)
                    notify st "window-pane-changed"
                        (NotifyTarget (Just sid) (Just win.id)
                            (Just newActive))
                        [ ("window", PWindowRef win.id wname)
                        , ("pane", PPaneRef newActive)
                        , ("old_pane", PPaneRef old)
                        , ("new_pane", PPaneRef newActive) ]
    forM_ emptied $ \sid -> broadcast st sid Exited
    forM_ locs $ \(_, _, pane) -> hangupPane pane

-- | Where @<leader> a@ should jump. An activity-marked window takes
-- priority: pick the first one in the same cyclic scan @next-window -a@
-- uses (indices after the current, then wrapping around to those before
-- it), so repeated presses walk the flagged windows in order. With no
-- activity anywhere, fall back to @last-window@ (the last-active index).
pickActivityTarget
    :: [Int]        -- ^ window indices in ascending order
    -> Int          -- ^ the current window index
    -> Set.Set Int  -- ^ indices carrying an activity flag
    -> Maybe Int    -- ^ the last-active window index, if any
    -> Maybe Int
pickActivityTarget ixs cur flagged mlast =
    case filter (`Set.member` flagged) scan of
        (ix : _) -> Just ix
        []       -> mlast
  where
    curPos = fromMaybe (-1) (List.elemIndex cur ixs)
    (before, after) = splitAt (curPos + 1) ixs
    scan = after <> before

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
    paneHistVar <- newTVarIO []
    bellVar <- newTVarIO False
    activityVar <- newTVarIO False
    zoomVar <- newTVarIO Nothing
    autoRenameVar <- newTVarIO . (.automaticRename) =<< readTVarIO st.options
    layoutNameVar <- newTVarIO Nothing
    optionsVar <- newTVarIO emptyDelta
    silenceVar <- newTVarIO False
    activityAtVar <- newTVarIO =<< getPOSIXTime
    pure Window
        { id = WindowId wid
        , name = nameVar
        , layout = layoutVar
        , layoutName = layoutNameVar
        , panes = panesVar
        , activeId = activeVar
        , paneHist = paneHistVar
        , bellFlag = bellVar
        , activity = activityVar
        , silenceFlag = silenceVar
        , activityAt = activityAtVar
        , zoomed = zoomVar
        , autoRename = autoRenameVar
        , options = optionsVar
        }

-- | @-O@: whether pane output is tapped into the process's stdin.
data OutputTap = OutputTapped | OutputUntapped
    deriving (Eq)

-- | @-I@: whether the process's stdout is fed back into the pane.
data StdinFeed = StdinFed | StdinUnfed
    deriving (Eq)

-- Spawn a @pipe-pane@ under a supervisor thread that owns the subprocess and
-- its handles as scoped sub-resources: @withCreateProcess@ terminates and reaps
-- the child (and closes its pipes) on every exit, and the stdout pump runs in
-- the supervisor itself, so a cancel unwinds both. 'stopPipe' thus releases
-- everything by a single cancel, never a hand-called kill/close/terminate an
-- exception could skip (mirrors 'startPaneReader's scoped ownership).
startPipe :: Pane -> String -> OutputTap -> StdinFeed -> IO ()
startPipe pane cmd outputTap stdinFeed = do
    ready <- newEmptyMVar
    super <- async $ withCreateProcess (shell cmd)
        { std_in  = if tapOn then CreatePipe else Inherit
        , std_out = if feedOn then CreatePipe else Inherit
        } $ \mIn mOut _ _ -> do
            self <- takeMVar ready
            atomically $ writeTVar pane.pipe $ Just PipeHandle
                { super = self, toStdin = if tapOn then mIn else Nothing }
            (case mOut of
                Just hout | feedOn -> pumpPipeOutput pane hout
                _                  -> forever (threadDelay maxBound))
                `finally` clearPipe pane self
    putMVar ready super
  where
    tapOn = outputTap == OutputTapped
    feedOn = stdinFeed == StdinFed

-- Drop a supervisor's own 'pane.pipe' entry as it exits (a child that quit on
-- its own, or a 'stopPipe' cancel), but only while the entry is still this
-- supervisor: a newer 'startPipe' that already replaced it must not be cleared.
clearPipe :: Pane -> Async () -> IO ()
clearPipe pane self = atomically $ modifyTVar' pane.pipe $ \case
    Just ph | ph.super == self -> Nothing
    other                      -> other

-- Read the process's stdout and write it into the pane's pty (@-I@). A closed
-- stdout (the child exited) ends the pump; an async cancel from 'stopPipe'
-- propagates so the supervisor's brackets can tear it down.
pumpPipeOutput :: Pane -> Handle -> IO ()
pumpPipeOutput pane hout = loop `catch` \(_ :: IOException) -> pure ()
  where
    loop = do
        chunk <- B8.hGetSome hout 4096
        unless (B8.null chunk) $ do
            Hat.Term.Pty.writePty pane.pty chunk
            loop

-- Feed a chunk of pane output to the pipe subprocess (@-O@). A write to a
-- handle 'stopPipe' has closed underneath us raises 'IOException' and is
-- dropped; anything else (including an async cancel) propagates.
forwardToPipe :: Pane -> B.ByteString -> IO ()
forwardToPipe pane bs = do
    mp <- readTVarIO pane.pipe
    forM_ mp $ \ph -> forM_ ph.toStdin $ \hdl ->
        (B8.hPut hdl bs >> hFlush hdl)
            `catch` \(_ :: IOException) -> pure ()

-- Stop any pipe on the pane by cancelling its supervisor, whose brackets reap
-- the child and stop the pump. 'cancel' waits for the teardown, so a following
-- 'startPipe' never races the old child.
stopPipe :: Pane -> IO ()
stopPipe pane = do
    mp <- atomically $ do
        m <- readTVar pane.pipe
        writeTVar pane.pipe Nothing
        pure m
    forM_ mp $ \ph -> cancel ph.super

-- | Write a single DEC-mode-2031 color-scheme report to a pane's pty.
notifyColorScheme :: Pane -> ColorScheme -> IO ()
notifyColorScheme pane scheme =
    Hat.Term.Pty.writePty pane.pty (schemeReport scheme)

-- | The reply to an OSC 10/11 color query, echoing the query's terminator
-- (xterm answers BEL with BEL, ST with ST).
oscColorReply :: Emu.OscColorTarget -> Emu.OscTerm -> ColorScheme -> B.ByteString
oscColorReply target term scheme =
    "\ESC]" <> code <> ";rgb:" <> color <> terminator
  where
    code = case target of
        Emu.Foreground -> "10"
        Emu.Background -> "11"
    color = case (scheme, target) of
        (SchemeDark, Emu.Background)  -> black
        (SchemeDark, Emu.Foreground)  -> white
        (SchemeLight, Emu.Background) -> white
        (SchemeLight, Emu.Foreground) -> black
    black = "0000/0000/0000"
    white = "ffff/ffff/ffff"
    terminator = case term of
        Emu.TermBel -> "\a"
        Emu.TermSt  -> "\ESC\\"

-- | A pane's foreground program (from @/proc@) as a display name:
-- normalized by 'commandName', so a NixOS-wrapped @vim@ shows as @vim@
-- everywhere (window titles, @#{pane_current_command}@, the persisted
-- tree). Falls back to @sh@.
paneCommandName :: Pane -> IO Text
paneCommandName pane = do
    mfg <- Hat.Term.Pty.foregroundCommand pane.pty
    raw <- case mfg of
        Just cmd -> pure cmd
        Nothing -> do
            r <- try (TIO.readFile ("/proc/" <> show (Hat.Term.Pty.pid pane.pty) <> "/comm"))
            pure $ case r of
                Right s -> let t = T.strip s in if T.null t then "sh" else t
                Left (_ :: IOException) -> "sh"
    pure (commandName raw)

-- | 'sessionSpawnEnv' for spawn paths that have seed pairs but no 'Session'
-- to read yet (session creation, restore).
globalSpawnEnv :: ServerState -> [(Text, Text)] -> IO [(Text, Text)]
globalSpawnEnv st pairs = do
    g <- readTVarIO st.globalEnviron
    pure (environPairs (environMerge g (environFromPairs pairs)))

createSession
    :: ServerState -> Maybe Text -> Maybe Text -> [(Text, Text)]
    -> FilePath -> Size -> IO Session
createSession st mname mrun environ dir sz = do
    sid <- atomically (freshId st.nextSession)
    opts <- readTVarIO st.options
    spawnEnv <- globalSpawnEnv st environ
    let shellCmd = maybe "/bin/sh" T.unpack (List.lookup "SHELL" spawnEnv)
    (win, pane) <- newWindowWithPane st (SessionId sid) shellCmd mrun
        dir spawnEnv (sz)
    nameVar <- newTVarIO (fromMaybe (tshow sid) mname)
    windowsVar <- newTVarIO (Map.singleton opts.baseIndex win)
    currentVar <- newTVarIO opts.baseIndex
    windowHistVar <- newTVarIO []
    sizeVar <- newTVarIO sz
    environVar <- newTVarIO (environFromPairs environ)
    cwdVar <- newTVarIO dir
    optionsVar <- newTVarIO emptyDelta
    let sess = Session
            { id = SessionId sid
            , name = nameVar
            , windows = windowsVar
            , currentIx = currentVar
            , windowHist = windowHistVar
            , lastSize = sizeVar
            , environ = environVar
            , startCwd = cwdVar
            , options = optionsVar
            }
    atomically $ modifyTVar' st.sessions (Map.insert sess.id sess)
    startPaneReader st sess.id win pane
    sname <- readTVarIO nameVar
    notify st "session-created"
        (NotifyTarget (Just sess.id) (Just win.id) (Just pane.id))
        [("session", PSessionRef sess.id sname)]
    pure sess

-- | The env a new pane inherits: the global environment overlaid by the
-- session's, hidden and cleared entries dropped (tmux's environ_for_session).
sessionSpawnEnv :: ServerState -> Session -> IO [(Text, Text)]
sessionSpawnEnv st sess = atomically $ do
    g <- readTVar st.globalEnviron
    s <- readTVar sess.environ
    pure (environPairs (environMerge g s))
