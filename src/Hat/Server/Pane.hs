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
    , withDefaultCommand
    , suitableShell
    , paneShell
    , paneProgram
    , spawnPane
    , programEnv
    , emptyPane
    , launchOf
    , respawnPane
    , dismissDeadPane
    , deadNoticeBytes
    , respawnStart
    , newWindowWithPane
    , startPaneReader
    , pauseReaders
    , resumeReaders
    , syncPanes
    , syncAllPanes
    , propKindLabel
    , notifyColorScheme
    , oscColorReply
    , forwardToPipe
    , startPipe
    , clearPipe
    , pumpPipeOutput
    , stopPipe
    , isCurrentWindow
    , enterClockMode
    , clockModeKey
    , attentionSeen
    , windowSeen
    , markActivity
    , windowActivity
    , markBell
    , noteOuterFocus
    , paneExitWaitMicros
    , hangupPane
    , typeIntoPane
    , syncTargets
    , typeKeyIntoPane
    , keyWrites
    , syncedWrites
    , typeWrites
    , coalesceWrites
    , keyBytesFor
    , typeIntoPaneSynced
    , hostPolicyOf
    , reencodeKey
    , keyEncoding
    , setScreenTitle
    , restorePaneState
    , claimPane
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
    , releaseModal
    , switchActive
    , rehomePanes
    , windowHolders
    , wrapPaneInWindow
    , paneCommandName
    , globalSpawnEnv
    , createSession
    , createGroupedSession
    , sessionSpawnEnv
    ) where

import Control.Concurrent
    (forkIO, forkIOWithUnmask, killThread, myThreadId, threadDelay, throwTo)
import Control.Concurrent.Async (Async, async, cancel, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Exception (Exception, IOException, catch, finally, mask_)
import Control.Monad (filterM, forM, forM_, forever, unless, void, when)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Data.Word (Word64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Clock (getMonotonicTimeNSec)
import Data.Char (isAlphaNum)
import System.Environment (getEnvironment, getProgName)
import System.IO (Handle, hFlush)
import System.Posix.Files (fileAccess)
import System.Posix.Process (getProcessID)
import System.Exit (ExitCode (..))
import System.Posix.Process qualified as Posix
import System.Posix.Unistd qualified as Posix
import System.Process
    (CreateProcess (..), StdStream (..), shell, withCreateProcess)
import System.Timeout (timeout)

import Hat.Geometry
import Hat.Log
import Hat.Model
import Hat.Model.Options
import Hat.Server.Alerts (AlertKind (..), AlertWhere (..), alertApplies, deliverAlert)
import Hat.Server.ClientIO (broadcastAs, sendAs)
import Hat.Server.Outbound (Delivery (..))
import Hat.Server.Clipboard (appClipboard, paneViewers)
import Hat.Server.InputRequests (Asked (..))
import Hat.Server.InputRequestsIO (askTerminal, forgetPaneRequests, paneHolders, paneReply)
import Hat.Server.ClientSession (sendClientsAway)
import Hat.Server.ControlQueue (awaitControlRoom, forwardToControl)
import Hat.Server.ColorScheme (ColorScheme (..), schemeReport)
import Hat.Server.Environ (environFromPairs, environMerge, environPairs)
import Hat.Server.Keys
import Hat.Server.Floating (ModalReturn (..), Restack (..), loseModal, modalPane, noFloats, restack)
import Hat.Server.Layout
import Hat.Server.Hooks
    ( NotifyTarget (..), PayloadItem (..), noTarget, notify, notifyGroupChange
    , notifyGroupDrops, notifyPane, notifyWindow, sessionTarget )
import Hat.Server.FormatEnv (expandFormat, paneEnvById, refreshAutoNames)
import Hat.Server.Locate (locatePane)
import Hat.Server.PaneColours
    (oscColourReply, queryCursorColour, queryDefaultColour, queryPaletteColour)
import Hat.Server.Style (applyStyle)
import Hat.Server.Winlink (unlinkEverywhere)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator.Types (paintLineBytes)
import Hat.Server.Mru (popOnClose, recordVisit, scrub)
import Hat.Server.Resize (applySessionSize)
import Hat.Term.Emulator qualified as Emu
import Hat.Term.Pty qualified
import Hat.Term.HostProtocol (cleanName)
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

newWindowWithPane
    :: ServerState -> SessionId -> FilePath -> PaneStart -> PaneKind -> FilePath
    -> [(Text, Text)] -> Size -> IO (Window, Pane)
newWindowWithPane st sid shellCmd how kind dir environ sz = do
    (wid, pid) <- atomically $
        (,) <$> freshId st.nextWindow <*> freshId st.nextPane
    pane <- case kind of
        ProgramPane -> spawnPane st (PaneId pid) sid shellCmd how dir environ sz
        EmptyPane -> emptyPane st (PaneId pid) how dir sz
    nameVar <- newTVarIO $ case how of
        ShellCommand cmd -> T.takeWhile (/= ' ') cmd
        ExecArgv (p : _) -> T.pack (baseName (T.unpack p))
        _ -> T.pack (baseName shellCmd)
    layoutVar <- newTVarIO (singlePane pane.id)
    panesVar <- newTVarIO (Map.singleton pane.id pane)
    activeVar <- newTVarIO pane.id
    paneHistVar <- newTVarIO []
    bellVar <- newTVarIO False
    activityVar <- newTVarIO False
    zoomVar <- newTVarIO Nothing
    autoRenameVar <- newTVarIO . (.automaticRename) =<< readTVarIO st.options
    layoutNameVar <- newTVarIO Nothing
    optionsVar    <- newOwnOptionsIO
    silenceVar    <- newTVarIO False
    activityAtVar <- newTVarIO =<< getPOSIXTime
    arrangeVar    <- newTVarIO Nothing
    floatsVar     <- newTVarIO noFloats
    menuVar       <- newTVarIO Nothing
    menuLastVar   <- newTVarIO (0, 0)
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
            , arrangeCache = arrangeVar
            , floats = floatsVar
            , menu = menuVar
            , menuLast = menuLastVar
            }
    pure (win, pane)

-- | A path's last component, or the whole path when that is empty (how tmux
-- names a shell).
baseName :: FilePath -> String
baseName path = case Prelude.reverse (takeWhile (/= '/') (Prelude.reverse path)) of
    "" -> path
    base -> base

-- | Where a captured program was running: as a child of the pane's
-- interactive shell (the user typed it), or as the pane's own top-level
-- process (a pane launched directly on a program). See 'restoreRun'.
data SpawnOrigin = ShellSpawned | Direct
    deriving (Eq, Show)

-- | What a new pane given the command @argv@ starts (tmux's spawn: one
-- word is a shell command line, more are exec'd as they are), else the
-- session's @default-command@ @dc@ ('withDefaultCommand').
shellStart :: Text -> [Text] -> PaneStart
shellStart dc = withDefaultCommand dc . \case
    [] -> FreshShell
    [c] -> ShellCommand c
    argv -> ExecArgv argv

-- | tmux's spawn: a new pane given no command runs the session's
-- @default-command@ @dc@ when one is set, else a fresh shell.
withDefaultCommand :: Text -> PaneStart -> PaneStart
withDefaultCommand dc = \case
    FreshShell | not (T.null dc) -> ShellCommand dc
    start -> start

-- | tmux's @checkshell@: an absolute path to an executable that is not hat
-- itself.
suitableShell :: FilePath -> IO Bool
suitableShell path = case path of
    '/' : _ -> do
        self <- getProgName
        if baseName path == self then pure False
            else fileAccess path False False True
                `catch` \(_ :: IOException) -> pure False
    _ -> pure False

-- | The shell a session's new panes run: its @default-shell@ when that is
-- a 'suitableShell', else @/bin/sh@.
paneShell :: Options -> IO FilePath
paneShell o = do
    let wanted = T.unpack (optionText "default-shell" o)
    ok <- suitableShell wanted
    pure (if ok then wanted else "/bin/sh")

-- | The program a pane execs and its arguments: the shell alone with no
-- command, @shell -c@ for one command string, an argv as is. Unlike tmux,
-- the shell runs as a plain interactive shell, not a login shell: a login
-- shell re-reads the system profile, resetting the prompt and environment
-- the pane inherited.
paneProgram :: FilePath -> PaneStart -> (FilePath, [String])
paneProgram sh = \case
    ShellCommand run -> (sh, ["-c", T.unpack run])
    ExecArgv (p : rest) -> (T.unpack p, map T.unpack rest)
    ExecArgv [] -> (sh, [])
    FreshShell -> (sh, [])
    ShellInject _ -> (sh, [])

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
    opts <- readTVarIO st.options
    paneEnv <- programEnv st sid (Just pid) shellCmd environ
    let -- A 'ShellInject' pane is a fresh shell like any other; its argv
        -- rides as pending input the reader types once the shell prints (see
        -- startPaneReader).
        (cmd, args) = paneProgram shellCmd mrun
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
    logEvent st.logger PaneSpawned
        { pane = rawPane pid, cmd = T.pack cmd }
    newPane pid pty emu sz dir pending mrun ProgramPane

-- | The environment a program hat spawns runs in: @environ@ less what hat
-- sets itself — the terminal, the server and session (@TMUX@), the pane
-- (@TMUX_PANE@) when there is one, and the shell.
programEnv
    :: ServerState -> SessionId -> Maybe PaneId -> FilePath -> [(Text, Text)]
    -> IO [(String, String)]
programEnv st sid mpid shellCmd environ = do
    serverPid <- getProcessID
    opts <- readTVarIO st.options
    let cleanEnv =
            [ (T.unpack k, T.unpack v)
            | (k, v) <- environ
            , k `notElem` ["TERM", "TMUX", "TMUX_PANE", "HAT", "HAT_PANE", "SHELL"]
            ]
        hatVar = st.sockPath <> "," <> show serverPid <> ","
            <> show (rawSession sid)
    pure $ cleanEnv <>
        [ ("TERM", T.unpack opts.defaultTerminal)
        , ("TMUX", hatVar)
        , ("HAT", hatVar) ]
        <> concat [ [ ("TMUX_PANE", "%" <> show (rawPane pid))
                    , ("HAT_PANE", "%" <> show (rawPane pid)) ] | Just pid <- [mpid] ]
        <> [ ("SHELL", shellCmd) ]

-- | What a spawning command's arguments ask for (tmux's @SPAWN_EMPTY@ and
-- argv): @-E@, or a lone empty argument, makes an empty pane; one argument
-- is a shell command line and several an argv run directly. An empty pane
-- refuses a command.
launchOf :: Bool -> [Text] -> Either Text (PaneStart, PaneKind)
launchOf emptyFlag pos
    | emptyFlag || pos == [""] = case pos of
        [] -> Right (FreshShell, EmptyPane)
        [""] -> Right (FreshShell, EmptyPane)
        _ -> Left "command cannot be given for empty pane"
    | otherwise = Right $ case pos of
        [] -> (FreshShell, ProgramPane)
        [c] -> (ShellCommand c, ProgramPane)
        argv -> (ExecArgv argv, ProgramPane)

-- | A pane running no program (tmux's empty pane): it shows only what is
-- written to its screen — with the cursor off and a line feed returning the
-- carriage, as tmux sets it up — and keeps @start@ and @dir@ for a later
-- @respawn-pane@ to run.
emptyPane :: ServerState -> PaneId -> PaneStart -> FilePath -> Size -> IO Pane
emptyPane st pid start dir sz = do
    opts <- readTVarIO st.options
    emu <- Emu.newEmulator sz opts.historyLimit
    _ <- Emu.feed emu "\ESC[?25l\ESC[20h"
    pty <- Hat.Term.Pty.unattachedPty
    newPane pid pty emu sz dir Nothing start EmptyPane

-- | A fresh pane record around its pty and emulator.
newPane
    :: PaneId -> Hat.Term.Pty.PtyHandle -> Emu.Emulator -> Size -> FilePath
    -> Maybe Text -> PaneStart -> PaneKind -> IO Pane
newPane pid pty emu sz dir pending start kind = do
    sizeVar <- newTVarIO sz
    deadVar <- newTVarIO False
    modeVar <- newTVarIO Nothing
    pipeVar <- newTVarIO Nothing
    readerVar <- newTVarIO Nothing
    homeVar <- newTVarIO Nothing
    optionsVar <- newOwnOptionsIO
    exitedVar <- newTVarIO Nothing
    inputOffVar <- newTVarIO False
    syncVar <- newTVarIO noOutputSync
    -- A new pane is titled with the host's name until its program sets one.
    host <- Posix.nodeName <$> Posix.getSystemID
    _ <- Emu.setTitle emu (T.pack host)
    alternateVar <- newTVarIO False
    clockVar <- newTVarIO NoClock
    viewVar <- newTVarIO Nothing
    replyVar <- newMVar ()
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
        , home = homeVar
        , pendingInput = pending
        , start = start
        , exited = exitedVar
        , kind = kind
        , inputOff = inputOffVar
        , onAlternate = alternateVar
        , outputSync = syncVar
        , clock = clockVar
        , viewBacking = viewVar
        , replyLock = replyVar
        }

-- | Run a new process in an existing pane (@respawn-pane@): the pane keeps
-- its id, slot, size and options; a fresh emulator, process and reader take
-- over, so the screen and any copy mode start clean (tmux's
-- @screen_reinit@). The old record is retired — marked so its reader's
-- teardown only reaps — and swapped out atomically, then hung up if its
-- process still runs. 'Nothing' when the pane left the tree meanwhile (a
-- kill or its own exit won the race); the new process is then hung up.
respawnPane
    :: ServerState -> SessionId -> Window -> Pane -> FilePath -> PaneStart
    -> PaneKind -> FilePath -> [(Text, Text)] -> IO (Maybe Pane)
respawnPane st sid win old shellCmd how kind dir environ = do
    sz <- readTVarIO old.size
    new <- case kind of
        ProgramPane -> spawnPane st old.id sid shellCmd how dir environ sz
        EmptyPane -> emptyPane st old.id how dir sz
    swapped <- atomically $ do
        claimed <- claimPane old
        if not claimed then pure Nothing else do
            forgetPaneRequests st old
            writeOwn new.options =<< readOwn old.options
            writeTVar new.inputOff =<< readTVar old.inputOff
            modifyTVar' win.panes (Map.insert new.id new)
            bumpDirty st
            Just . isNothing <$> readTVar old.exited
    case swapped of
        Nothing -> Nothing <$ hangupPane new
        Just wasLive -> do
            when wasLive (hangupPane old)
            startPaneReader st sid win new
            pure (Just new)

-- | What @respawn-pane@ without a command re-runs: the pane's own launch. A
-- restored program typed into a fresh shell respawns as that shell.
respawnStart :: PaneStart -> PaneStart
respawnStart = \case
    ShellInject _ -> FreshShell
    other         -> other

-- | Signals a pane's reader that its shell child has exited. See
-- 'startPaneReader'.
data ChildExited = ChildExited
    deriving Show

instance Exception ChildExited

-- | The reader thread owns a pane's lifetime: it pumps pty output into
-- the emulator until end-of-file, and 'closePane' runs in a @finally@ so
-- the pane's resources and model entry are released however the loop ends
-- — clean EOF, a hang-up from a kill command, or an exception.
--
-- The @spawnSid@\/@spawnWin@ only seed 'pane.home'; the loop re-reads it per
-- chunk, so output routing follows the pane across re-parents — see
-- Note [Pane home].
startPaneReader :: ServerState -> SessionId -> Window -> Pane -> IO ()
startPaneReader st spawnSid spawnWin pane = do
    atomically $ writeTVar pane.home (Just (spawnSid, spawnWin))
    -- A dead pane adopted across a reload, or an empty one, has no process
    -- to read.
    dead <- readTVarIO pane.exited
    unless (isJust dead || pane.kind == EmptyPane) (runPaneReader st pane)

runPaneReader :: ServerState -> Pane -> IO ()
runPaneReader st pane = do
    -- Count the pane live before forking, so a kill that lands before the
    -- reader is scheduled still finds it counted and 'waitIdle' waits for
    -- its reap. Decremented once the reader (and its 'reapPane') is done.
    atomically $ modifyTVar' st.livePanes (+ 1)
    -- Masked for the thread's whole life except between chunks: once the
    -- tid is published, a 'hangupPane' kill must always reach the
    -- cleanup, and an async exception (a kill, 'ChildExited') never lands
    -- between consuming a chunk and feeding it.
    void $ mask_ $ forkIOWithUnmask $ \unmask -> do
        tid <- myThreadId
        atomically $ writeTVar pane.readerTid (Just tid)
        pendingRef <- newIORef pane.pendingInput
        rbuf <- Hat.Term.Pty.newReadBuf
        -- Track pane death by the shell's exit, not only pty EOF: a lingering
        -- fd-holder can keep the master from ever EOFing. 'ChildExited' (never
        -- the 'ThreadKilled' of an explicit kill) switches the reader to a
        -- non-blocking drain of the reaped child's final bytes, then ends.
        -- 'withAsync' scopes the watcher to the read phase: once the reads end
        -- (by EOF or by draining after 'ChildExited'), it is cancelled before
        -- the 'finally' cleanup, so a late async 'ChildExited' can never land in
        -- 'paneEof' and interrupt the 'Exited' broadcast it runs.
        (withAsync (Hat.Term.Pty.waitExit pane.pty >> throwTo tid ChildExited) $
            \_ -> readLoop unmask
                    (atomically (awaitControlRoom st pane)
                        >> Hat.Term.Pty.awaitOutputOr pane.pty syncRequested)
                    (Hat.Term.Pty.readPtyBuf pane.pty rbuf)
                    (Hat.Term.Pty.readAvailBuf pane.pty rbuf) pendingRef
                `catch` \ChildExited -> do
                    writeIORef pendingRef Nothing
                    let drainRd = Hat.Term.Pty.readAvailBuf pane.pty rbuf
                    readLoop unmask (pure Nothing) drainRd drainRd pendingRef)
            `finally` paneEof st pane
            `finally` atomically (modifyTVar' st.livePanes (subtract 1))
  where
    -- Consume one chunk and feed it, counted in flight from the read gate
    -- through the feed. See 'pauseReaders'.
    feedChunk rd = do
        atomically $ do
            readTVar st.readGate >>= check . (== ReadersFlowing)
            modifyTVar' st.readsInFlight (+ 1)
        (do bs <- rd
            mopts <- paneOptionsNow st pane
            forM_ mopts (Emu.setHostPolicy pane.emulator . hostPolicyOf)
            events <- if B8.null bs then pure [] else Emu.feed pane.emulator bs
            noteAlternate st pane
            pure (bs, events, mopts))
            `finally` atomically (modifyTVar' st.readsInFlight (subtract 1))
    -- @pending@ is a one-shot: a restored program's command line, typed into
    -- the pane the moment its shell first prints (so readline is up to read
    -- it), then dropped. Enter is a bare CR, as a real keyboard sends.
    -- @await@ waits for output without consuming it, or for a sync request
    -- ('syncPanes'); @rd@ is the read step: blocking during the pane's life,
    -- a non-blocking drain once the shell has exited (an empty result ends
    -- the loop either way). @drainRd@ never blocks: it serves a sync.
    readLoop
        :: (forall a. IO a -> IO a) -> IO (Maybe Word64) -> IO B.ByteString
        -> IO B.ByteString -> IORef (Maybe Text) -> IO ()
    readLoop unmask await rd drainRd pending = do
        woke <- unmask await
        case woke of
            Nothing -> do
                (bs, events, mopts) <- feedChunk rd
                unless (B8.null bs) $ do
                    unmask (consume pending bs events mopts)
                    readLoop unmask await rd drainRd pending
            Just req -> do
                drain unmask drainRd pending syncDrainChunks
                atomically $ modifyTVar' pane.outputSync $ \o ->
                    o { answered = max o.answered req }
                readLoop unmask await rd drainRd pending
    syncRequested = do
        o <- readTVar pane.outputSync
        check (o.requested > o.answered)
        pure o.requested
    -- Everything the pty holds now, bounded so a pane that never stops
    -- writing cannot hold a sync forever.
    drain
        :: (forall a. IO a -> IO a) -> IO B.ByteString -> IORef (Maybe Text)
        -> Int -> IO ()
    drain unmask drainRd pending n = when (n > 0) $ do
        (bs, events, mopts) <- feedChunk drainRd
        unless (B8.null bs) $ do
            unmask (consume pending bs events mopts)
            drain unmask drainRd pending (n - 1)
    consume pending bs events mopts = do
        let allowed name = maybe False (optionFlag name) mopts
        do
            readIORef pending >>= mapM_ (\line -> do
                Hat.Term.Pty.writePty pane.pty (TE.encodeUtf8 (line <> "\r"))
                writeIORef pending Nothing)
            forwardToPipe pane bs
            now <- (`div` 1000000) <$> getMonotonicTimeNSec
            atomically (forwardToControl st pane now bs)
            mhome <- readTVarIO pane.home  -- See Note [Pane home]
            forM_ events $ \case
                Emu.Output out -> paneReply st pane out
                -- The app asked the current light/dark scheme (CSI ? 996 n);
                -- answer from the OS scheme the server already tracks.
                Emu.ColorSchemeQuery -> do
                    mscheme <- readTVarIO st.colorScheme
                    forM_ mscheme (notifyColorScheme pane)
                -- The app asked its terminal's fg/bg color (OSC 10/11 ;?),
                -- the classic light-versus-dark probe; hat doesn't paint a
                -- pane background, so answer with the canonical color for
                -- the OS scheme it tracks (white-on-black when dark).
                -- A pane whose style gives it its own colour answers with
                -- that instead.
                Emu.OscColorQuery target term -> do
                    own <- queryDefaultColour st pane target
                    case own of
                        Just rgb -> paneReply st pane $ oscColourReply
                            (case target of
                                Emu.Foreground -> "10"
                                Emu.Background -> "11") rgb term
                        Nothing -> do
                            mscheme <- readTVarIO st.colorScheme
                            forM_ mscheme $ \scheme ->
                                paneReply st pane (oscColorReply target term scheme)
                -- An entry the pane leaves unset is the terminal's to say.
                Emu.OscPaletteQuery n term -> do
                    mrgb <- queryPaletteColour st pane n
                    case mrgb of
                        Just rgb -> paneReply st pane
                            (oscColourReply ("4;" <> T.pack (show n)) rgb term)
                        Nothing -> askTerminal st pane (AskPalette n) term
                Emu.OscCursorColourQuery term -> do
                    mrgb <- queryCursorColour st pane
                    forM_ mrgb $ \rgb -> paneReply st pane
                        (oscColourReply "12" rgb term)
                -- The app raised a desktop notification (OSC 9/777); hat has
                -- no notification UI of its own, so forward it verbatim to
                -- the session's attached terminals to raise with the OS.
                -- See Note [Falling behind]
                Emu.DesktopNotification raw ->
                    forM_ mhome $ \(sid, _) -> broadcastAs Expendable st sid (Notify raw)
                -- The pane's own title only feeds #{pane_title}; the
                -- client's desktop title is composed in 'refreshTitles'.
                Emu.TitleRequest raw -> when (allowed "allow-set-title") $
                    forM_ (cleanName raw) $ \t -> do
                        changed <- Emu.setTitle pane.emulator t
                        when changed $ notifyPane st "pane-title-changed" pane
                            [ ("new_title", PText t) ]
                Emu.RenameRequest raw -> when (allowed "allow-rename") $
                    forM_ (cleanName raw) (renameFromApp st pane)
                Emu.ClipboardRequest param term -> forM_ mopts $ \o ->
                    appClipboard st o pane param term
                Emu.Bell -> forM_ mhome $ \(_, win) -> do
                    fires <- atomically $ do
                        r <- bellAlerts st win
                        bumpDirty st
                        pure r
                    notifyPane st "pane-bell" pane []
                    forM_ fires (fireAlert st "alert-bell" BellAlert win)
                -- An unhomed pane (spawn race) repaints unconditionally:
                -- over-painting is safe, a missed repaint freezes the pane.
                Emu.ScreenChanged -> case mhome of
                    Just (sid, win) -> windowActivity st sid win
                    Nothing -> atomically (bumpDirty st)
                -- the emulator reported a terminal property hat does not act on;
                -- surface it as a warning rather than silently dropping it.
                Emu.UnknownProp kind prop ->
                    logEvent st.logger UnknownTermProp
                        { pane = rawPane pane.id
                        , propKind = propKindLabel kind
                        , prop = prop
                        }
                -- A tmux passthrough payload hat does not answer itself goes
                -- on to the outer terminals @allow-passthrough@ names; with it
                -- off, it is logged rather than dropped without a trace.
                Emu.UnhandledPassthrough raw ->
                    case maybe "off" (optionText "allow-passthrough") mopts of
                        "off" -> logEvent st.logger UnhandledPassthrough
                            { pane = rawPane pane.id
                            , payload = T.pack (show raw)
                            }
                        reach -> do
                            cs <- atomically $ if reach == "all"
                                then paneHolders st pane
                                else paneViewers st pane
                            -- See Note [Falling behind]
                            forM_ cs $ \c -> sendAs Expendable c (Notify raw)

-- | The pane options that shape how its output is taken.
hostPolicyOf :: Options -> Emu.HostPolicy
hostPolicyOf o = Emu.defaultHostPolicy
    { Emu.allowAltScreen = optionFlag "alternate-screen" o
    , Emu.scrollOnClear = optionFlag "scroll-on-clear" o
    }

-- | An app renamed its window (@ESC k@, under @allow-rename@; tmux's
-- @input_exit_rename@): a name pins the window's name; an empty one hands
-- the window back to @automatic-rename@, or blanks the name when that is
-- off.
renameFromApp :: ServerState -> Pane -> Text -> IO ()
renameFromApp st pane name = do
    mhome <- readTVarIO pane.home  -- See Note [Pane home]
    forM_ mhome $ \(sid, win) -> do
        auto <- (.automaticRename) <$> readTVarIO st.options
        if T.null name && auto
            then do
                atomically (writeTVar win.autoRename True)
                refreshAutoNames st
            else do
                atomically $ do
                    writeTVar win.name name
                    writeTVar win.autoRename False
                    bumpDirty st
                notifyWindow st "window-renamed" (Just sid) win []

-- | Read chunks a reader drains to serve one sync; see 'readLoop'.
syncDrainChunks :: Int
syncDrainChunks = 16

-- | Wait until every pane's reader has fed its emulator all the output the
-- pane had written by the time of the call, so a command run next sees it
-- (as tmux's single loop guarantees). A reader with nothing unread and no
-- chunk in flight is already caught up; one that may not read now — parked
-- for a reload, or held back by a control client's flow control — is not
-- waited for, and a stalled one is given up on after a second.
syncPanes :: ServerState -> [Pane] -> IO ()
syncPanes st panes = do
    posted <- fmap catMaybes . forM panes $ \pane -> do
        unread <- Hat.Term.Pty.hasOutput pane.pty
        atomically $ do
            inFlight <- (> 0) <$> readTVar st.readsInFlight
            reading <- isJust <$> readTVar pane.readerTid
            flowing <- (== ReadersFlowing) <$> readTVar st.readGate
            room <- (True <$ awaitControlRoom st pane) `orElse` pure False
            if not ((unread || inFlight) && reading && flowing && room)
                then pure Nothing
                else do
                    o <- readTVar pane.outputSync
                    let req = o.requested + 1
                    writeTVar pane.outputSync o { requested = req }
                    pure (Just (pane, req))
    expired <- registerDelay 1_000_000
    atomically $ (readTVar expired >>= check) `orElse`
        forM_ posted (\(pane, req) -> do
            o <- readTVar pane.outputSync
            gone <- isNothing <$> readTVar pane.readerTid
            check (o.answered >= req || gone))

-- | 'syncPanes' over every pane in the tree.
syncAllPanes :: ServerState -> IO ()
syncAllPanes st = do
    panes <- atomically $ do
        sessions <- Map.elems <$> readTVar st.sessions
        wins <- concat <$> mapM (fmap Map.elems . readTVar . (.windows)) sessions
        concat <$> mapM windowPanes wins
    syncPanes st (List.nubBy (\a b -> a.id == b.id) panes)

-- | Quiesce the pane readers for a reload capture: close the read gate,
-- then wait out every chunk already in flight, so on return every pane byte
-- is either in its emulator (captured) or still in the kernel pty buffer
-- (survives the exec).
pauseReaders :: ServerState -> IO ()
pauseReaders st = do
    atomically $ writeTVar st.readGate ReadersPaused
    atomically $ readTVar st.readsInFlight >>= check . (== 0)

-- | Reopen the read gate so parked readers resume; the failed-reload
-- fallback.
resumeReaders :: ServerState -> IO ()
resumeReaders st = atomically $ writeTVar st.readGate ReadersFlowing

-- | Name a terminal prop's value kind for the 'UnknownTermProp' log.
propKindLabel :: Emu.PropKind -> Text
propKindLabel = \case
    Emu.PropBool -> "bool"
    Emu.PropInt  -> "int"
    Emu.PropStr  -> "str"

-- | Whether @win@ is the session's current (foreground) window.
-- | @clock-mode@: clock mode becomes the pane's top mode, redrawn each
-- second while the pane keeps it.
enterClockMode :: ServerState -> Pane -> IO ()
enterClockMode st pane = do
    was <- atomically $ do
        c <- readTVar pane.clock
        writeTVar pane.clock ClockOnTop
        bumpDirty st
        pure c
    unless (was == ClockOnTop) $ notifyPane st "pane-mode-changed" pane []
    when (was == NoClock) $ void $ forkIO tick
  where
    tick = do
        now <- getPOSIXTime
        let toNext = 1 - (realToFrac now - fromIntegral (floor now :: Integer)) :: Double
        threadDelay (max 1000 (round (toNext * 1_000_000)))
        on <- atomically $ do
            c <- readTVar pane.clock
            when (c /= NoClock) (bumpDirty st)
            pure (c /= NoClock)
        when on tick

-- | A key reaching a pane whose top mode is clock mode ends it, as tmux's
-- @window_clock_key@ does; 'True' when the key was spent so.
clockModeKey :: ServerState -> Pane -> IO Bool
clockModeKey st pane = do
    ended <- atomically $ do
        top <- paneClockOnTop pane
        when top $ do
            writeTVar pane.clock NoClock
            bumpDirty st
        pure top
    when ended $ notifyPane st "pane-mode-changed" pane []
    pure ended

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

-- | Whether some attached client has @win@ on screen: it is the current
-- window of the client's session, or the client has a chooser open (whose
-- live preview can show any window).
windowDisplayed :: ServerState -> Window -> STM Bool
windowDisplayed st win = do
    sessions <- readTVar st.sessions
    cs <- Map.elems <$> readTVar st.clients
    let displays c = readTVar c.picker >>= \case
            Just _ -> pure True
            Nothing -> do
                sid <- readTVar c.session
                maybe (pure False) (\sess -> isCurrentWindow sess win)
                    (Map.lookup sid sessions)
    or <$> mapM displays [c | c <- cs, c.role == Attached]

-- | Register activity on a window: restart its silence timer, raise the
-- activity flag, and alert as @activity-action@ says. Output and window
-- creation both land here. A repaint is scheduled only when a client could
-- see the difference — the window is displayed, or its status-line flag
-- rose.
windowActivity :: ServerState -> SessionId -> Window -> IO ()
windowActivity st sid win = do
    now <- getPOSIXTime
    mfire <- atomically $ do
        writeTVar win.activityAt now
        was <- readTVar win.activity
        r <- activityAlert st sid win
        flagRose <- (&& not was) <$> readTVar win.activity
        displayed <- windowDisplayed st win
        when (displayed || flagRose) (bumpDirty st)
        pure r
    forM_ mfire (fireAlert st "alert-activity" ActivityAlert win)

-- | A session's alert: its hook, then its clients told
-- ('deliverAlert').
fireAlert :: ServerState -> Text -> AlertKind -> Window -> SessionAlert -> IO ()
fireAlert st hook kind win a = do
    notify st hook
        (NotifyTarget (Just a.session.id) (Just win.id) Nothing)
        [ ("session", PSessionRef a.session.id a.sessionName)
        , ("window", PWindowRef win.id a.windowName) ]
    deliverAlert st a.session a.options kind a.index

-- | An alert that applies in one session holding the window.
data SessionAlert = SessionAlert
    { session :: Session
    , options :: Options  -- ^ the window's, in that session
    , index :: Int        -- ^ the window's there
    , sessionName :: Text
    , windowName :: Text
    }

-- | The alert for a window in a session, when its action option admits it.
sessionAlert :: AlertKind -> Session -> Options -> Window -> STM (Maybe SessionAlert)
sessionAlert kind sess opts win = do
    ws <- readTVar sess.windows
    cur <- readTVar sess.currentIx
    case [ ix | (ix, w) <- Map.toList ws, w.id == win.id ] of
        ix : _ | alertApplies kind opts (if ix == cur then InCurrentWindow else InWindow ix) -> do
            sname <- readTVar sess.name
            wname <- readTVar win.name
            pure (Just (SessionAlert sess opts ix sname wname))
        _ -> pure Nothing

-- | 'markBell' plus the alert decision, once per session holding the
-- window: the sessions whose @alert-bell@ should fire (monitor-bell on and
-- bell-action admits the window there). Bells are not deduplicated — every
-- bell reports.
bellAlerts :: ServerState -> Window -> STM [SessionAlert]
bellAlerts st win = do
    sessions <- Map.elems <$> readTVar st.sessions
    fmap concat . forM sessions $ \sess -> do
        ws <- readTVar sess.windows
        if not (any (\w -> w.id == win.id) (Map.elems ws))
            then pure []
            else do
                wopts <- resolveForWindow st sess win
                if not wopts.monitorBell then pure [] else do
                    seen <- windowSeen st sess win
                    unless seen $ writeTVar win.bellFlag True
                    maybe [] pure <$> sessionAlert BellAlert sess wopts win

-- | 'markActivity' plus the alert decision: once flagged, a window raises
-- no more activity alerts until the flag clears; otherwise the alert fires
-- as @activity-action@ admits it, on the current window too.
activityAlert :: ServerState -> SessionId -> Window -> STM (Maybe SessionAlert)
activityAlert st sid win = do
    msess <- Map.lookup sid <$> readTVar st.sessions
    case msess of
        Nothing -> pure Nothing
        Just sess -> do
            wopts <- resolveForWindow st sess win
            was <- readTVar win.activity
            if not wopts.monitorActivity || was then pure Nothing else do
                seen <- windowSeen st sess win
                unless seen $ writeTVar win.activity True
                sessionAlert ActivityAlert sess wopts win

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

-- | Set the title on a pane's screen.
setScreenTitle :: Pane -> Text -> IO ()
setScreenTitle pane t =
    void (Emu.setTitle pane.emulator t)

-- | Give a rebuilt pane the input state and title it was captured with.
restorePaneState :: Pane -> Bool -> Maybe Text -> IO ()
restorePaneState pane off mtitle = do
    atomically (writeTVar pane.inputOff off)
    forM_ mtitle (setScreenTitle pane)

-- | Write keyboard input — typed, sent or pasted — to a pane's program,
-- unless @select-pane -d@ turned its input off.
typeIntoPane :: Pane -> B.ByteString -> IO ()
typeIntoPane pane bytes = do
    off <- readTVarIO pane.inputOff
    unless off (Hat.Term.Pty.writePty pane.pty bytes)

-- | The panes input typed into @pane@ is copied to under
-- @synchronize-panes@ (tmux's @window_pane_copy_key@): when @pane@ has the
-- option on, every other visible, live pane of its window that has it on
-- and is not in a mode.
syncTargets :: ServerState -> Pane -> STM [Pane]
syncTargets st pane = do
    mhome <- readTVar pane.home  -- See Note [Pane home]
    sessions <- readTVar st.sessions
    case mhome of
        Just (sid, win) | Just sess <- Map.lookup sid sessions -> do
            let synced p = (.synchronizePanes) <$> resolveForPane st sess win p
            on <- synced pane
            if not on then pure [] else do
                zoom <- readTVar win.zoomed
                active <- readTVar win.activeId
                others <- filter (\p -> p.id /= pane.id) <$> windowPanes win
                flip filterM others $ \p -> do
                    inMode <- isJust <$> readTVar p.mode
                    gone <- isJust <$> readTVar p.exited
                    let visible = isNothing zoom || p.id == active
                    if inMode || gone || not visible then pure False else synced p
        _ -> pure []

-- | A key pressed in a pane (tmux's @window_pane_key@): typed as the pane's
-- terminal expects it, and copied to the panes synchronized with it. A focus
-- report concerns only the pane it was meant for.
typeKeyIntoPane :: ServerState -> Pane -> Key -> IO ()
typeKeyIntoPane st pane key = typeWrites =<< keyWrites st pane key

-- | What 'typeKeyIntoPane' writes, and to which panes.
keyWrites :: ServerState -> Pane -> Key -> IO [(Pane, B.ByteString)]
keyWrites st pane key = do
    o <- readTVarIO st.options
    let bspace = o.backspace
        writeTo p = fmap ((,) p) <$> paneKeyBytes (keyEncoding o) bspace p key
    targets <- if key.name `elem` ["FocusIn", "FocusOut"]
        then pure []
        else atomically (syncTargets st pane)
    catMaybes <$> mapM writeTo (pane : targets)

-- | Type writes into their panes, one write per pane carrying all of its
-- bytes, as tmux's buffered pane output reaches the program.
typeWrites :: [(Pane, B.ByteString)] -> IO ()
typeWrites ws = forM_ (coalesceWrites (.id) ws) (uncurry typeIntoPane)

-- | Writes joined per destination, in the order each was first written to.
coalesceWrites :: Ord k => (a -> k) -> [(a, B.ByteString)] -> [(a, B.ByteString)]
coalesceWrites key ws =
    [ (dest, B.concat [ bs | (d, bs) <- ws, key d == key dest ])
    | (dest, _) <- List.nubBy (\a b -> key (fst a) == key (fst b)) ws ]

-- | The bytes a key sends to a pane (tmux's @input_key@): Backspace sends
-- what the @backspace@ option names, a @User@/N/ key sends nothing, and
-- every other key is encoded for the pane's terminal ('reencodeKey').
paneKeyBytes :: KeyEncoding -> Text -> Pane -> Key -> IO (Maybe B.ByteString)
paneKeyBytes kenc bspace p = keyBytesFor kenc bspace p.emulator

-- | 'paneKeyBytes' for any emulator's program.
keyBytesFor :: KeyEncoding -> Text -> Emu.Emulator -> Key -> IO (Maybe B.ByteString)
keyBytesFor kenc bspace emu key = case key.name of
    "BSpace" -> pure (backspaceBytes bspace)
    "M-BSpace" -> pure (("\ESC" <>) <$> backspaceBytes bspace)
    n | isUserKey n -> pure Nothing
    _ -> Just . (.raw) <$> reencodeKey kenc (Just emu) key

-- | How keys are encoded for panes under these (server) options.
keyEncoding :: Options -> KeyEncoding
keyEncoding o = keyEncodingOf o.extendedKeys o.extendedKeysFormat

-- | Bytes typed into a pane as they are, copied to the panes synchronized
-- with it (tmux's @window_pane_paste@).
typeIntoPaneSynced :: ServerState -> Pane -> B.ByteString -> IO ()
typeIntoPaneSynced st pane bytes = typeWrites =<< syncedWrites st pane bytes

-- | What 'typeIntoPaneSynced' writes, and to which panes.
syncedWrites :: ServerState -> Pane -> B.ByteString -> IO [(Pane, B.ByteString)]
syncedWrites st pane bytes = do
    targets <- atomically (syncTargets st pane)
    pure [ (p, bytes) | p <- pane : targets ]

-- | Forward a recognized key as the pane's advertised terminal expects, not
-- as the outer terminal's incidental bytes. The arrows are DECCKM-dependent
-- (@\ESC[A@ vs @\ESCOA@), and a modified character key depends on the key
-- protocol the pane's app turned on, so the pane's emulator encodes both;
-- every other named key forwards its tmux-256color terminfo bytes from
-- 'namedKeys'. Unrecognized keys keep their raw bytes.
reencodeKey :: KeyEncoding -> Maybe Emu.Emulator -> Key -> IO Key
reencodeKey kenc memu key = case arrowOf key.name of
    Just ck | Just emu <- memu -> do
        enc <- Emu.encodeKey emu ck
        pure key { raw = enc }
    _ | Just app <- Map.lookup key.name keypadAppBytes, Just emu <- memu -> do
        on <- Emu.keypadMode emu
        pure $ if on then key { raw = app } else canonical
      | Just (code, mods) <- extendedKeyCode key.name
      , Just emu <- memu -> modified emu code mods
      -- Mode 2 spells Shift-Tab as an extended S-Tab, as tmux does.
      | key.name == "BTab", Just emu <- memu -> do
        mode <- paneMode emu
        pure $ if mode == 2 then extendedOr key 9 2 mode else canonical
      | otherwise -> pure canonical
  where
    canonical = maybe key (\canon -> key { raw = canon })
        (Map.lookup key.name namedKeyBytes)
    -- A kitty-protocol app gets its own spelling; otherwise the pane's
    -- extended-key mode decides (see 'extendedKeyBytes').
    modified emu code mods = do
        kitty <- Emu.encodeKeyPress emu Emu.KeyPress { code = code, mods = mods }
        case kitty of
            Just bs -> pure key { raw = bs }
            Nothing -> extendedOr key code mods <$> paneMode emu
    paneMode emu = paneKeyMode kenc.extended . (.extendedKeys) <$> Emu.keyModes emu
    extendedOr k code mods mode =
        maybe k (\bs -> k { raw = bs }) (extendedKeyBytes kenc.format mode code mods)
    arrowOf n = case n of
        "Up"    -> Just Emu.CursorUp
        "Down"  -> Just Emu.CursorDown
        "Left"  -> Just Emu.CursorLeft
        "Right" -> Just Emu.CursorRight
        _       -> Nothing

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

-- | Retire a pane, making the caller the one to hang it up; 'False' when it
-- was already retired.
claimPane :: Pane -> STM Bool
claimPane pane = do
    retired <- readTVar pane.dead
    unless retired (writeTVar pane.dead True)
    pure (not retired)

-- | The reader saw EOF. A pane already retired from the tree — killed, or
-- replaced by a respawn — is only reaped. Otherwise @remain-on-exit@
-- decides: the pane is torn down ('closePane'), or it stays in the tree as a
-- dead pane showing its last screen until a kill or a respawn
-- ('markPaneDead').
paneEof :: ServerState -> Pane -> IO ()
paneEof st pane = do
    retired <- readTVarIO pane.dead
    remain <- if retired then pure RemainOff else paneRemainOnExit st pane
    if remain == RemainOff then closePane st pane else do
        status <- reapPane st pane
        if remain == RemainFailed && status == Just (Posix.Exited ExitSuccess)
            then teardownPane st pane (pure ())
            else markPaneDead st pane status

-- | The pane's resolved @remain-on-exit@, read where the pane lives now.
paneRemainOnExit :: ServerState -> Pane -> IO RemainOnExit
paneRemainOnExit st pane = atomically $ do
    mloc <- locatePane st pane.id
    case mloc of
        Nothing -> pure RemainOff
        Just (sid, win) -> do
            msess <- Map.lookup sid <$> readTVar st.sessions
            case msess of
                Nothing -> pure RemainOff
                Just sess -> remainOnExitOf <$> resolveForPane st sess win pane

remainOnExitOf :: Options -> RemainOnExit
remainOnExitOf opts = case optionText "remain-on-exit" opts of
    "on" -> RemainOn
    "failed" -> RemainFailed
    "key" -> RemainKey
    _ -> RemainOff

-- | Keep an exited (and already reaped) pane in the tree as a dead pane:
-- record how its program ended, draw @remain-on-exit-format@ on a fresh
-- bottom line with the cursor hidden, and fire @pane-died@.
markPaneDead :: ServerState -> Pane -> Maybe Posix.ProcessStatus -> IO ()
markPaneDead st pane status = do
    now <- getPOSIXTime
    let (code, sig) = case status of
            Just (Posix.Exited ExitSuccess)     -> (Just 0, Nothing)
            Just (Posix.Exited (ExitFailure n)) -> (Just n, Nothing)
            Just (Posix.Terminated s _)         -> (Nothing, Just (fromIntegral s))
            _                                   -> (Nothing, Nothing)
    atomically $ do
        writeTVar pane.readerTid Nothing
        writeTVar pane.exited
            (Just PaneExit { status = code, signal = sig, diedAt = now })
    notifyPane st "pane-died" pane []
    menv <- paneEnvById st pane.id
    forM_ menv $ \env -> do
        fmt <- paneRemainFormat st pane
        unless (T.null fmt) $ do
            notice <- expandFormat st env fmt
            sz <- readTVarIO pane.size
            void (Emu.feed pane.emulator (deadNoticeBytes sz notice))
    atomically (bumpDirty st)

-- | A key typed into a dead pane under @remain-on-exit key@ closes it.
-- Whether the key was consumed so.
dismissDeadPane :: ServerState -> Pane -> IO Bool
dismissDeadPane st pane = do
    dead <- isJust <$> readTVarIO pane.exited
    remain <- if dead then paneRemainOnExit st pane else pure RemainOff
    if remain /= RemainKey then pure False else do
        mloc <- atomically (locatePane st pane.id)
        forM_ mloc $ \(sid, win) -> killPaneLocs st [(sid, win, pane)]
        pure True

-- | The pane's resolved @remain-on-exit-format@.
paneRemainFormat :: ServerState -> Pane -> IO Text
paneRemainFormat st pane = atomically $ do
    mloc <- locatePane st pane.id
    case mloc of
        Nothing -> pure ""
        Just (sid, win) -> do
            msess <- Map.lookup sid <$> readTVar st.sessions
            case msess of
                Nothing -> pure ""
                Just sess -> optionText "remain-on-exit-format"
                    <$> resolveForPane st sess win pane

-- | The bytes that draw a dead pane's notice the way tmux does: scroll the
-- whole screen up one line, write the expanded format (its @#[…]@ styles
-- applied, cut to the pane width) on the bottom row, and hide the cursor.
deadNoticeBytes :: Size -> Text -> B.ByteString
deadNoticeBytes sz notice =
    "\ESC[r\ESC[" <> B8.pack (show sz.rows) <> ";1H\n\ESC[0m"
        <> paintLineBytes (V.fromList
            (take (fromIntegral sz.cols) (styledRun Cell.defaultStyle notice)))
        <> "\ESC[0m\ESC[?25l"
  where
    styledRun sty t = case T.breakOn "#[" t of
        (pre, rest) ->
            [ Cell.glyphCell c sty | c <- T.unpack pre ]
            <> case T.breakOn "]" (T.drop 2 rest) of
                _ | T.null rest -> []
                (spec, close)
                    | T.null close -> [ Cell.glyphCell c sty | c <- T.unpack rest ]
                    | otherwise -> styledRun (applyStyle sty spec) (T.drop 1 close)

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
        Just (sid, win) -> (,) (Just sid) <$> detachPane st win pane
        Nothing         -> pure (Nothing, AlreadyDetached)

-- | Tear a pane down from the reader thread's @finally@: the model detach
-- (idempotent — a killing command may already have done it, see
-- 'detachPane') followed by the OS-side reap, which runs exactly once here
-- so no teardown path can forget a resource. (The emulator frees itself via
-- its finalizer.)
closePane :: ServerState -> Pane -> IO ()
closePane st pane = teardownPane st pane (void (reapPane st pane))

-- | 'closePane' with the OS-side reap supplied: run between the end-of-life
-- notifications and the emptied session's @Exited@, or a no-op when the
-- caller already reaped.
teardownPane :: ServerState -> Pane -> IO () -> IO ()
teardownPane st pane reap = do
    (mctx, names, before) <- atomically $ do
        mloc <- locatePane st pane.id
        names <- sessionNames st
        ctx <- traverse
            (\(sid, win) -> do
                wname <- readTVar win.name
                pure (sid, maybe "" fst (Map.lookup sid names), win, wname))
            mloc
        (,,) ctx names <$> readTVar st.sessions
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
            when (wf == WindowSurvives) $
                notifyWindow st "window-layout-changed" (Just sid) win []
            when (wf == WindowRemoved) $ do
                notify st "window-unlinked"
                    (sessionTarget sid)
                    [ ("session", PSessionRef sid sname)
                    , ("window", PWindowRef win.id wname) ]
                notify st "window-closed" (sessionTarget sid)
                    [ ("window", PWindowRef win.id wname) ]
            announceEmptied st names (emptiedBy sf)
        (Detached _ _, Nothing) -> notify st "pane-exited"
            (NotifyTarget Nothing Nothing (Just pane.id))
            [ ("pane", PPaneRef pane.id) ]
        (AlreadyDetached, _) -> pure ()
    reap
    case r of
        Detached _ sf -> sendClientsAway st
            (mapMaybe (`Map.lookup` before) (emptiedBy sf))
        AlreadyDetached -> pure ()

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
detachPane :: ServerState -> Window -> Pane -> STM DetachResult
detachPane st win pane = do
    claimed <- claimPane pane
    if not claimed then pure AlreadyDetached else do
        modifyTVar' win.panes (Map.delete pane.id)
        releaseModal win pane.id
        lay <- readTVar win.layout
        mz <- readTVar win.zoomed
        when (mz == Just pane.id) $ writeTVar win.zoomed Nothing
        case removePane pane.id lay of
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
                emptied <- unlinkEverywhere st win
                bumpDirty st
                pure $ Detached WindowRemoved $ case emptied of
                    [] -> SessionSurvives
                    _ -> SessionsEmptied (map (.id) emptied)

-- | The modal half of @window_lost_pane@, for a pane already gone from its
-- window's pane map: it drops out of the floating state, and the modal
-- pane's going hands the window back to the pane active before it, zoomed
-- again if the window was.
releaseModal :: Window -> PaneId -> STM ()
releaseModal win pid = do
    (fs', mret) <- loseModal pid <$> readTVar win.floats
    writeTVar win.floats fs'
    ps <- readTVar win.panes
    forM_ mret $ \ret -> forM_ ret.returnTo $ \back -> when (Map.member back ps) $ do
        writeTVar win.activeId back
        modifyTVar' win.paneHist (filter (/= back))
        when ret.rezoom $ writeTVar win.zoomed (Just back)

-- | Make a pane its window's active one (@window_set_active_pane@), unless
-- a modal pane holds the window; a floating pane comes to the front of
-- the stacking (@window_redraw_active_switch@). Whether it changed.
switchActive :: Window -> PaneId -> STM Bool
switchActive win pid = do
    active <- readTVar win.activeId
    fs <- readTVar win.floats
    lay <- readTVar win.layout
    let held = maybe False (/= pid) (modalPane fs)
    if active == pid || held then pure False else do
        when (isFloating pid lay) $
            writeTVar win.floats (restack ToFront pid fs lay)
        modifyTVar' win.paneHist (recordVisit active pid)
        writeTVar win.activeId pid
        pure True

-- | What 'detachPane' did. See 'detachPane'.
data DetachResult = AlreadyDetached | Detached WindowFate SessionFate
    deriving (Eq, Show)

-- | Whether a detach removed the pane's whole window. See 'detachPane'.
data WindowFate = WindowSurvives | WindowRemoved
    deriving (Eq, Show)

-- | The sessions a detach destroyed by emptying them: the window's last
-- link went with its last pane. See 'detachPane'.
data SessionFate = SessionSurvives | SessionsEmptied [SessionId]
    deriving (Eq, Show)

emptiedBy :: SessionFate -> [SessionId]
emptiedBy = \case
    SessionSurvives -> []
    SessionsEmptied sids -> sids

-- | Every live session's name and group, for announcing sessions about to
-- vanish.
sessionNames :: ServerState -> STM (Map.Map SessionId (Text, Maybe Text))
sessionNames st = traverse (\s -> (,) <$> readTVar s.name <*> readTVar s.group)
    =<< readTVar st.sessions

-- | Announce sessions emptied by a detach: session-closed, then their
-- departure from their groups.
announceEmptied :: ServerState -> Map.Map SessionId (Text, Maybe Text) -> [SessionId] -> IO ()
announceEmptied st names gone = do
    let info sid = Map.findWithDefault ("", Nothing) sid names
    forM_ gone $ \sid -> notify st "session-closed" noTarget
        [ ("session", PSessionRef sid (fst (info sid))) ]
    notifyGroupDrops st [ (sid, nm, g) | sid <- gone, let (nm, g) = info sid ]

-- | The OS half of a pane's teardown: stop any pipe-pane, wait for the
-- child — bounded, since a child that ignores SIGHUP would never exit on
-- its own; on timeout escalate to SIGKILL and wait again (the reaper
-- always fills the exit slot, so the second wait is guaranteed to
-- complete) — then release the pty. Runs once, in the reader thread's
-- @finally@ (see 'closePane').
reapPane :: ServerState -> Pane -> IO (Maybe Posix.ProcessStatus)
reapPane st pane = do
    stopPipe pane
    reaped <- timeout paneExitWaitMicros (Hat.Term.Pty.waitExit pane.pty)
    status <- case reaped of
        Just s -> pure s
        Nothing -> do
            Hat.Term.Pty.signalKill pane.pty
            Hat.Term.Pty.waitExit pane.pty
    Hat.Term.Pty.closePty pane.pty
    logEvent st.logger PaneExited { pane = rawPane pane.id }
    pure status

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
        (,) (sid, win) <$> detachPane st win pane

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
    (results, names, snames, preActive, before) <- atomically $ do
        before <- readTVar st.sessions
        snames <- sessionNames st
        names <- forM locs $ \(sid, win, _) -> do
            wname <- readTVar win.name
            pure ((sid, win.id), (maybe "" fst (Map.lookup sid snames), wname))
        pre <- forM locs $ \(_, win, _) ->
            (,) win.id <$> readTVar win.activeId
        rs <- detachPanes st locs
        pure (rs, Map.fromList names, snames, Map.fromList pre, before)
    forM_ (List.nub [sid | (sid, _, _) <- locs]) (applySessionSize st)
    let removedWindows = List.nubBy (\(s1, w1) (s2, w2) ->
                s1 == s2 && w1.id == w2.id)
            [ (sid, win) | ((sid, win), Detached WindowRemoved _) <- results ]
        emptied = List.nub
            [ sid | (_, Detached _ sf) <- results, sid <- emptiedBy sf ]
    when (mode == NotifyLifecycle) $ do
        forM_ removedWindows $ \(sid, win) -> do
            let (sname, wname) =
                    Map.findWithDefault ("", "") (sid, win.id) names
            notify st "window-unlinked" (sessionTarget sid)
                [ ("session", PSessionRef sid sname)
                , ("window", PWindowRef win.id wname) ]
            notify st "window-closed" (sessionTarget sid)
                [ ("window", PWindowRef win.id wname) ]
        announceEmptied st snames emptied
    -- A surviving window whose active pane was killed reports the switch.
    when (mode == NotifyLifecycle) $ do
        let survivors = List.nubBy (\(_, w1) (_, w2) -> w1.id == w2.id)
                [ (sid, win)
                | ((sid, win), Detached WindowSurvives _) <- results ]
        forM_ survivors $ \(sid, win) -> do
            notifyWindow st "window-layout-changed" (Just sid) win []
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
    sendClientsAway st (mapMaybe (`Map.lookup` before) emptied)
    forM_ [ pane | ((_, _, pane), (_, Detached _ _)) <- zip locs results ]
        hangupPane

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

-- | Detach a pane from whichever window holds it (tmux's
-- @window_lost_pane@ + @layout_close_pane@): its mark is cleared, a lost
-- active pane gives way to the last pane, else its previous sibling, else
-- its next, and an emptied window is unlinked everywhere. The pane's pty
-- keeps running — this only re-parents it, backing @break-pane@\/
-- @join-pane@. Yields the sessions the unlink emptied, already removed.
removePaneFromTree :: ServerState -> PaneId -> STM [Session]
removePaneFromTree st pid = do
    sessions <- readTVar st.sessions
    wins <- concat <$> forM (Map.elems sessions) (fmap Map.elems . readTVar . (.windows))
    holders <- filterM (fmap (Map.member pid) . readTVar . (.panes)) wins
    case holders of
        [] -> pure []
        win : _ -> do
            marked <- readTVar st.markedPane
            when (marked == Just pid) $ writeTVar st.markedPane Nothing
            ps <- readTVar win.panes
            lay <- readTVar win.layout
            let order = paneOrder lay ps
                (before, after) = break (== pid) order
                neighbour = listToMaybe (reverse before) <|> listToMaybe (drop 1 after)
            writeTVar win.panes (Map.delete pid ps)
            releaseModal win pid
            mz <- readTVar win.zoomed
            when (mz == Just pid) $ writeTVar win.zoomed Nothing
            bumpDirty st
            case removePane pid lay of
                Just lay' -> do
                    writeTVar win.layout lay'
                    active <- readTVar win.activeId
                    hist <- filter (/= pid) <$> readTVar win.paneHist
                    if active /= pid
                        then writeTVar win.paneHist hist
                        else forM_ (popOnClose (`elem` layoutPanes lay') neighbour hist) $
                            \(next, hist') -> do
                                writeTVar win.activeId next
                                writeTVar win.paneHist hist'
                    pure []
                Nothing -> unlinkEverywhere st win

-- | Point every pane of @win@ at @(sid, win)@, after the window changed
-- session. See Note [Pane home].
rehomePanes :: SessionId -> Window -> STM ()
rehomePanes sid win =
    readTVar win.panes >>= mapM_ (\p -> writeTVar p.home (Just (sid, win)))

-- | Every session currently linking @win@.
windowHolders :: ServerState -> Window -> STM [Session]
windowHolders st win = do
    sessions <- Map.elems <$> readTVar st.sessions
    filterM
        (fmap (any (\w -> w.id == win.id) . Map.elems) . readTVar . (.windows))
        sessions

-- | Build a fresh single-pane window around an already-running pane.
wrapPaneInWindow :: ServerState -> Pane -> IO Window
wrapPaneInWindow st pane = do
    wid <- atomically (freshId st.nextWindow)
    name <- paneCommandName pane
    nameVar <- newTVarIO name
    layoutVar <- newTVarIO (singlePane pane.id)
    panesVar <- newTVarIO (Map.singleton pane.id pane)
    activeVar <- newTVarIO pane.id
    paneHistVar <- newTVarIO []
    bellVar <- newTVarIO False
    activityVar <- newTVarIO False
    zoomVar <- newTVarIO Nothing
    autoRenameVar <- newTVarIO . (.automaticRename) =<< readTVarIO st.options
    layoutNameVar <- newTVarIO Nothing
    optionsVar <- newOwnOptionsIO
    silenceVar <- newTVarIO False
    activityAtVar <- newTVarIO =<< getPOSIXTime
    arrangeVar <- newTVarIO Nothing
    floatsVar <- newTVarIO noFloats
    menuVar <- newTVarIO Nothing
    menuLastVar <- newTVarIO (0, 0)
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
        , arrangeCache = arrangeVar
        , floats = floatsVar
        , menu = menuVar
        , menuLast = menuLastVar
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

-- | 'sessionSpawnEnv' for spawn paths that have seed pairs but no 'Session'
-- to read yet (session creation, restore).
globalSpawnEnv :: ServerState -> [(Text, Text)] -> IO [(Text, Text)]
globalSpawnEnv st pairs = do
    g <- readTVarIO st.globalEnviron
    pure (environPairs (environMerge g (environFromPairs pairs)))

-- | A fresh session id, and the name a session created without @-s@ takes
-- from it — the id itself, after @prefix-@ for a session group's member —
-- skipping ids whose name is taken (tmux's @session_create@).
freshSessionName :: ServerState -> Maybe Text -> Maybe Text -> STM (Int, Text)
freshSessionName st prefix mname = do
    sid <- freshId st.nextSession
    case mname of
        Just nm -> pure (sid, nm)
        Nothing -> do
            let nm = maybe "" (<> "-") prefix <> tshow sid
            taken <- mapM (\s -> readTVar s.name) . Map.elems =<< readTVar st.sessions
            if nm `elem` taken then freshSessionName st prefix mname else pure (sid, nm)

createSession
    :: ServerState -> Maybe Text -> [Text] -> [(Text, Text)]
    -> FilePath -> Size -> IO Session
createSession st mname argv environ dir sz = do
    (sid, sname0) <- atomically (freshSessionName st Nothing mname)
    opts <- atomically (resolveGlobal st)
    spawnEnv <- globalSpawnEnv st environ
    shellCmd <- paneShell opts
    (win, pane) <- newWindowWithPane st (SessionId sid) shellCmd
        (shellStart (optionText "default-command" opts) argv) ProgramPane dir spawnEnv sz
    now <- getPOSIXTime
    nameVar <- newTVarIO sname0
    windowsVar <- newTVarIO (Map.singleton opts.baseIndex win)
    currentVar <- newTVarIO opts.baseIndex
    windowHistVar <- newTVarIO []
    sizeVar <- newTVarIO sz
    environVar <- newTVarIO (environFromPairs environ)
    cwdVar <- newTVarIO dir
    optionsVar <- newOwnOptionsIO
    groupVar <- newTVarIO Nothing
    activityVar <- newTVarIO =<< atomically (tickActivity st)
    holdVar <- newTVarIO NotHeld
    let sess = Session
            { id = SessionId sid
            , createdAt = now
            , restores = 0
            , name = nameVar
            , windows = windowsVar
            , currentIx = currentVar
            , windowHist = windowHistVar
            , lastSize = sizeVar
            , environ = environVar
            , startCwd = cwdVar
            , options = optionsVar
            , group = groupVar
            , activity = activityVar
            , destroyHold = holdVar
            }
    atomically $ modifyTVar' st.sessions (Map.insert sess.id sess)
    startPaneReader st sess.id win pane
    sname <- readTVarIO nameVar
    notify st "session-created"
        (NotifyTarget (Just sess.id) (Just win.id) (Just pane.id))
        [("session", PSessionRef sess.id sname)]
    pure sess

-- | Create a session in @leader@'s session group, sharing its window map
-- (tmux's @new-session -t@): no window is spawned, and the new session
-- starts on the lowest-numbered window. The group takes @leader@'s name
-- when @leader@ is not grouped yet.
createGroupedSession
    :: ServerState -> Session -> Maybe Text -> [(Text, Text)] -> FilePath
    -> IO Session
createGroupedSession st leader mname environ dir = do
    now <- getPOSIXTime
    (sess, founded, gname) <- atomically $ do
        lname <- readTVar leader.name
        lgroup <- readTVar leader.group
        let gname = fromMaybe lname lgroup
        writeTVar leader.group (Just gname)
        (sid, sname) <- freshSessionName st (Just gname) mname
        ws <- readTVar leader.windows
        sz <- readTVar leader.lastSize
        nameVar <- newTVar sname
        currentVar <- newTVar (maybe 0 fst (Map.lookupMin ws))
        windowHistVar <- newTVar []
        sizeVar <- newTVar sz
        environVar <- newTVar (environFromPairs environ)
        cwdVar <- newTVar dir
        optionsVar <- newOwnOptions
        groupVar <- newTVar (Just gname)
        activityVar <- newTVar =<< tickActivity st
        holdVar <- newTVar NotHeld
        let sess = Session
                { id = SessionId sid
                , createdAt = now
                , restores = 0
                , name = nameVar
                , windows = leader.windows
                , currentIx = currentVar
                , windowHist = windowHistVar
                , lastSize = sizeVar
                , environ = environVar
                , startCwd = cwdVar
                , options = optionsVar
                , group = groupVar
                , activity = activityVar
                , destroyHold = holdVar
                }
        modifyTVar' st.sessions (Map.insert sess.id sess)
        bumpDirty st
        pure (sess, lgroup == Nothing, gname)
    size <- atomically $ do
        ss <- Map.elems <$> readTVar st.sessions
        length <$> filterM (fmap (== Just gname) . readTVar . (.group)) ss
    when founded $ do
        lname <- readTVarIO leader.name
        notifyGroupChange st "session-added-to-group" (sessionTarget leader.id)
            leader.id lname gname 1
    sname <- readTVarIO sess.name
    notifyGroupChange st "session-added-to-group" (sessionTarget sess.id)
        sess.id sname gname size
    -- Joining a group links each of its windows into the new session.
    shared <- Map.elems <$> readTVarIO sess.windows
    forM_ shared $ \win -> notifyWindow st "window-linked" (Just sess.id) win
        [ ("session", PSessionRef sess.id sname) ]
    mwin <- atomically (currentWindow sess)
    notify st "session-created"
        (NotifyTarget (Just sess.id) ((.id) <$> mwin) Nothing)
        [("session", PSessionRef sess.id sname)]
    pure sess

-- | The env a new pane inherits: the global environment overlaid by the
-- session's, hidden and cleared entries dropped (tmux's environ_for_session).
sessionSpawnEnv :: ServerState -> Session -> IO [(Text, Text)]
sessionSpawnEnv st sess = atomically $ do
    g <- readTVar st.globalEnviron
    s <- readTVar sess.environ
    pure (environPairs (environMerge g s))

-- | Track whether a pane's program is on the alternate screen, waking the
-- renderer and the reconcile loop when it moves: the pane's scrollbar
-- comes and goes with it ('Hat.Server.Resize.windowArrange').
noteAlternate :: ServerState -> Pane -> IO ()
noteAlternate st pane = do
    alt <- Emu.onAlternateScreen pane.emulator
    atomically $ do
        was <- readTVar pane.onAlternate
        when (was /= alt) $ do
            writeTVar pane.onAlternate alt
            bumpDirty st
