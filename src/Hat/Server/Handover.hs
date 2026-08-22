-- | The reload handover's two halves: capture the live tree with the pty
-- handles and screens it must carry across the re-exec, and rebuild that
-- tree in the new image by adopting them.
module Hat.Server.Handover
    ( ScrollbackCarry (..)
    , captureReload
    , readReload
    , rebuildReload
    , rebuildReloadSession
    , captureReloadScreen
    , replayPane
    , captureSize
    , reloadSchemePush
    ) where

import Control.Concurrent.STM
import Control.Exception
    (IOException, catch, try)
import Control.Monad (filterM, forM, forM_, unless)
import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory
    (renameFile)
import System.Posix.Types (Fd (..))

import Hat.Geometry
import Hat.Log
import Hat.Server.Environ (environFromPairs)
import Hat.Model
import Hat.Model.Options
import Hat.Server.Reload
    (Handover (..), ReloadCleanup (..), ReloadModes (..), ReloadPane (..)
    , ReloadScreen (..), ReloadSession (..), ReloadState (..), ReloadWindow (..)
    , decodeHandover)
import qualified Hat.Term.Pty
import Hat.Server.ColorScheme
    ( ColorScheme, schemeReport )
import Hat.Server.WindowStruct (WindowStruct (..), windowStruct)
import Hat.Server.Layout
import Hat.Server.LayoutString (layoutFromString, layoutSize)
import Hat.Server.Pane
import qualified Hat.Term.Cell as Cell
import qualified Hat.Term.Emulator as Emu

-- | The color-scheme report a reload must re-push into an adopted pane so a
-- surviving app reacts to the OS scheme immediately, not only on its next
-- change. Only a pane whose app held the ?2031 subscription gets one, and only
-- when the server already knows the scheme; otherwise 'Nothing' (nothing to
-- push). The subscription itself is re-armed separately, by 'replayPane'. See
-- 'adoptPane'.
reloadSchemePush :: ReloadModes -> Maybe ColorScheme -> Maybe B.ByteString
reloadSchemePush rm mscheme
    | rm.colorReport = schemeReport <$> mscheme
    | otherwise      = Nothing

-- Reload: capture the live tree with its inherited handles, and rebuild it
-- in the re-exec'd image by adopting them ------------------------------------

-- | Whether a reload's handover carries the panes' scrollback; see
-- 'captureReloadScreen'.
data ScrollbackCarry = KeepScrollback | DropScrollback
    deriving (Eq, Show)

-- | Capture the running tree into the two halves of a handover: the evolving
-- 'ReloadState' (the same structure 'captureSnapshot' records, plus each
-- pane's live pty master fd and child pid), and the version-independent
-- 'ReloadCleanup' core (the listening socket fd and the flat list of every
-- pane's (master fd, child pid), so a version-mismatched reload can hang the
-- inherited processes up cleanly rather than orphan them).
captureReload :: ScrollbackCarry -> ServerState -> IO (ReloadCleanup, ReloadState)
captureReload carry st = do
    (sess, laName, lsName, mfd) <- atomically $ do
        sessMap <- readTVar st.sessions
        laId    <- readTVar st.lastActiveSession
        laName  <- traverse (readTVar . (.name)) (laId >>= (`Map.lookup` sessMap))
        lsId    <- readTVar st.lastSession
        lsName  <- traverse (readTVar . (.name)) (lsId >>= (`Map.lookup` sessMap))
        mfd     <- readTVar st.listenFd
        pure (Map.elems sessMap, laName, lsName, mfd)
    rsessions <- mapM (captureReloadSession carry) sess
    let tree = ReloadState rsessions laName lsName
        liveHandles =
            [ (p.masterFd, p.childPid)
            | s <- rsessions, w <- s.windows, p <- w.panes ]
        cleanup = ReloadCleanup
            { listenFd = fromMaybe (-1) mfd, live = liveHandles }
    pure (cleanup, tree)

captureReloadSession :: ScrollbackCarry -> Session -> IO ReloadSession
captureReloadSession carry s = do
    (nm, cwd, curIx, winHist, wstructs) <- atomically $ do
        nm    <- readTVar s.name
        cwd   <- readTVar s.startCwd
        curIx <- readTVar s.currentIx
        winHist <- readTVar s.windowHist
        eff   <- readTVar s.lastSize
        ws    <- Map.toAscList <$> readTVar s.windows
        wstructs <- mapM (windowStruct eff) ws
        pure (nm, cwd, curIx, winHist, wstructs)
    rwins <- mapM (captureReloadWindow carry) wstructs
    pure (ReloadSession nm (T.pack cwd) curIx winHist rwins)

captureReloadWindow :: ScrollbackCarry -> WindowStruct -> IO ReloadWindow
captureReloadWindow carry ws = do
    rpanes <- forM ws.wsPanes $ \pane -> do
        dir <- paneCurrentPath pane
        let Fd fd = Hat.Term.Pty.masterFd pane.pty
        ms <- Emu.modes pane.emulator
        sc <- captureReloadScreen carry pane.emulator
        pure (ReloadPane (T.pack dir) (fromIntegral fd)
                (fromIntegral (Hat.Term.Pty.pid pane.pty)) (reloadModesOf ms) sc)
    pure (ReloadWindow ws.wsIx ws.wsName ws.wsLayout ws.wsActive
            ws.wsLastActive ws.wsAutoRename rpanes)

-- | Freeze a pane's emulator into the reload payload: its live grid and cursor,
-- its alt-screen flag, and its scrollback (oldest line first). 'adoptPane'
-- replays this back into the fresh emulator after a reload. 'DropScrollback'
-- skips the scrollback entirely, so the reload doubles as a memory cleanup.
captureReloadScreen :: ScrollbackCarry -> Emu.Emulator -> IO ReloadScreen
captureReloadScreen carry emu = do
    scr <- Emu.snapshot emu
    m   <- Emu.modes emu
    pen <- Emu.currentPen emu
    sb  <- case carry of
        DropScrollback -> pure []
        KeepScrollback -> do
            len <- Emu.scrollbackLength emu
            catMaybes <$> mapM (Emu.scrollbackLine emu) [0 .. len - 1]
    pure ReloadScreen
        { altScreen     = m.altScreen
        , cursorRow     = scr.cursor.row
        , cursorCol     = scr.cursor.col
        , cursorVisible = scr.cursorVisible
        , rows          = map V.toList (V.toList scr.cells)
        , scrollback    = map V.toList sb
        , pen           = pen
        }

-- | The app-set mode subscriptions to carry across a reload; the inverse
-- rebuild happens in 'adoptPane'.
reloadModesOf :: Emu.Modes -> ReloadModes
reloadModesOf m = ReloadModes
    { colorReport = m.colorReport
    , focusReport = m.focusReport
    , mouse = case m.mouse of
        Emu.MouseOff   -> 0
        Emu.MouseClick -> 1
        Emu.MouseDrag  -> 2
        Emu.MouseMove  -> 3
    }

-- Read and consume the handover file the outgoing image wrote. The frozen
-- envelope yields the cleanup core even for an incompatible version; 'Nothing'
-- only when even that is unreadable (a corrupt or foreign file), where there
-- are no inherited fds to reclaim.
readReload :: Logger -> FilePath -> IO (Maybe Handover)
readReload lg hp = do
    r <- try (B.readFile hp)
    -- Keep the consumed blob as .last rather than deleting it: a resume that
    -- crashes the process (e.g. a native abort in the emulator) leaves the exact
    -- bytes that reproduce it on disk for offline replay.
    renameFile hp (hp <> ".last") `catch` \(_ :: IOException) -> pure ()
    case r of
        Left (e :: IOException) -> do
            logEvent lg ServerCrash
                { err = "reload: unreadable handover: " <> T.pack (show e) }
            pure Nothing
        Right bs -> case decodeHandover bs of
            Left derr -> do
                logEvent lg ServerCrash { err = "reload: bad handover: " <> derr }
                pure Nothing
            Right h -> pure (Just h)

-- | Rebuild the tree from a reload handover, adopting each pane's inherited
-- pty and child rather than spawning; each pane's captured screen is replayed
-- into its fresh emulator ('adoptPane').
rebuildReload :: ServerState -> ReloadState -> IO ()
rebuildReload st rs = do
    forM_ rs.sessions (rebuildReloadSession st)
    forM_ rs.currentSession $ \nm ->
        resolveSessionByName st nm $ \s ->
            atomically (writeTVar st.lastActiveSession (Just s.id))
    forM_ rs.lastSession $ \nm ->
        resolveSessionByName st nm $ \s ->
            atomically (writeTVar st.lastSession (Just s.id))

resolveSessionByName :: ServerState -> Text -> (Session -> IO ()) -> IO ()
resolveSessionByName st nm act = do
    sessMap <- readTVarIO st.sessions
    hits <- filterM (fmap (== nm) . readTVarIO . (.name)) (Map.elems sessMap)
    forM_ (listToMaybe hits) act

rebuildReloadSession :: ServerState -> ReloadSession -> IO ()
rebuildReloadSession st rsess = do
    let wins = filter (not . null . (.panes)) rsess.windows
    unless (null wins) $ do
        sid <- SessionId <$> atomically (freshId st.nextSession)
        env <- restoreEnv
        -- The captured window area (every window was captured at the same
        -- effective size), so the reconcile tick that runs before any client
        -- attaches finds the adopted panes already at their layout size.
        let sz = fromMaybe Size { rows = 24, cols = 80 }
                (listToMaybe (mapMaybe (layoutSize . (.layout)) wins))
        built <- forM wins $ \rwin -> do
            (win, panes) <- rebuildReloadWindow st sz rwin
            pure (rwin.ix, win, panes)
        let winMap = Map.fromList [(wix, win) | (wix, win, _) <- built]
            curIx | Map.member rsess.currentIx winMap = rsess.currentIx
                  | otherwise = maybe rsess.currentIx fst (Map.lookupMin winMap)
            winHist = List.nub
                [l | l <- rsess.windowHist, l /= curIx, Map.member l winMap]
        nameVar    <- newTVarIO rsess.name
        windowsVar <- newTVarIO winMap
        currentVar <- newTVarIO curIx
        windowHistVar <- newTVarIO winHist
        sizeVar    <- newTVarIO sz
        environVar <- newTVarIO (environFromPairs env)
        cwdVar     <- newTVarIO (T.unpack rsess.startCwd)
        optionsVar <- newTVarIO emptyDelta
        let sess = Session
                { id = sid, name = nameVar, windows = windowsVar
                , currentIx = currentVar, windowHist = windowHistVar
                , lastSize = sizeVar, environ = environVar
                , startCwd = cwdVar, options = optionsVar }
        atomically $ modifyTVar' st.sessions (Map.insert sid sess)
        forM_ built $ \(_, win, panes) ->
            forM_ panes (startPaneReader st sid win)

rebuildReloadWindow :: ServerState -> Size -> ReloadWindow -> IO (Window, [Pane])
rebuildReloadWindow st sz rwin = do
    wid <- WindowId <$> atomically (freshId st.nextWindow)
    histLimit <- (.historyLimit) <$> readTVarIO st.options
    panes <- forM rwin.panes (adoptPane st sz histLimit)
    let pids = map (.id) panes
        paneMap = Map.fromList [(p.id, p) | p <- panes]
        lay = fromMaybe (namedLayout EvenHorizontal (1 % 2) pids)
                        (layoutFromString rwin.layout pids)
        activePid = pids !! max 0 (min (length pids - 1) rwin.active)
        paneHistPids = List.nub
            [ pids !! o | o <- rwin.paneHist
            , o >= 0, o < length pids, pids !! o /= activePid ]
    nameVar       <- newTVarIO rwin.name
    layoutVar     <- newTVarIO lay
    layoutNameVar <- newTVarIO Nothing
    panesVar      <- newTVarIO paneMap
    activeVar     <- newTVarIO activePid
    paneHistVar   <- newTVarIO paneHistPids
    bellVar       <- newTVarIO False
    activityVar   <- newTVarIO False
    zoomVar       <- newTVarIO Nothing
    autoRenameVar <- newTVarIO rwin.autoRename
    optionsVar    <- newTVarIO emptyDelta
    silenceVar    <- newTVarIO False
    activityAtVar <- newTVarIO =<< getPOSIXTime
    let win = Window
            { id = wid, name = nameVar, layout = layoutVar
            , layoutName = layoutNameVar
            , panes = panesVar, activeId = activeVar
            , paneHist = paneHistVar, bellFlag = bellVar
            , activity = activityVar, zoomed = zoomVar
            , silenceFlag = silenceVar, activityAt = activityAtVar
            , autoRename = autoRenameVar, options = optionsVar }
    pure (win, panes)

-- | Build a pane around an inherited pty ('Hat.Term.Pty.adopt') and a blank
-- emulator, for the reload path — the analogue of 'spawnPane' that re-adopts
-- a running child instead of forking a new one.
adoptPane :: ServerState -> Size -> Int -> ReloadPane -> IO Pane
adoptPane st sz histLimit rp = do
    pid <- PaneId <$> atomically (freshId st.nextPane)
    -- Trace each adopt phase so a resume that stalls names the pane and the
    -- step it stalled on (fd adopt vs. screen replay) instead of going silent.
    logEvent st.logger ReloadAdopt { pane = rawPane pid, phase = "start" }
    pty <- Hat.Term.Pty.adopt (Fd (fromIntegral rp.masterFd))
                              (fromIntegral rp.childPid)
    -- Adopt at the size the pane was CAPTURED at, not the session default:
    -- replaying a capture into a smaller grid wraps and clamps it into a
    -- state whose later reflow-resize aborts inside the emulator ("screen_resize
    -- failed to update cursor position", the 2026-07-28 field crash). The
    -- reconcile loop then resizes toward the layout as for any live pane.
    let esz = fromMaybe sz (captureSize rp.screen)
    emu <- Emu.newEmulator esz histLimit
    logEvent st.logger ReloadAdopt { pane = rawPane pid, phase = "replaying" }
    -- Seed the scrollback first, then paint the live grid on top of it, so the
    -- restored viewport sits above the reseeded history. replayBytes also re-arms
    -- the app's ?2031/?1004/mouse subscriptions and re-enters the alt screen when
    -- the program was in it, so a later exit reverts cleanly.
    let (replayBytes, replaySb) = replayPane esz rp
    Emu.seedScrollback emu replaySb
    _ <- Emu.feed emu replayBytes
    logEvent st.logger ReloadAdopt { pane = rawPane pid, phase = "ready" }
    sizeVar   <- newTVarIO esz
    deadVar   <- newTVarIO False
    modeVar   <- newTVarIO Nothing
    pipeVar   <- newTVarIO Nothing
    readerVar <- newTVarIO Nothing
    optionsVar <- newTVarIO emptyDelta
    let pane = Pane
            { id = pid, pty = pty, emulator = emu, size = sizeVar
            , dead = deadVar, startCwd = T.unpack rp.cwd, mode = modeVar
            , options = optionsVar
            , pipe = pipeVar, readerTid = readerVar, pendingInput = Nothing }
    -- A surviving app that held the ?2031 subscription never re-emits it across
    -- the reload, so re-push the current scheme once — otherwise it renders the
    -- pre-reload scheme until the OS scheme next changes (bug f3).
    scheme <- readTVarIO st.colorScheme
    forM_ (reloadSchemePush rp.modes scheme) (Hat.Term.Pty.writePty pty)
    pure pane

-- | Rebuild the emulator mode subscriptions a reload carried. @altScreen@ is
-- left off here; 'adoptPane' sets it from the captured screen before replaying
-- it. Inverse of 'reloadModesOf'.
emuModesOf :: ReloadModes -> Emu.Modes
emuModesOf rm = Emu.Modes
    { altScreen = False
    , colorReport = rm.colorReport
    , focusReport = rm.focusReport
    , mouse = case rm.mouse of
        1 -> Emu.MouseClick
        2 -> Emu.MouseDrag
        3 -> Emu.MouseMove
        _ -> Emu.MouseOff
    }

-- | Rebuild the 'Emu.Screen' a reload captured, sized to the pane, for
-- 'Emu.restoreBytes'. See 'replayPane'.
screenOf :: Size -> ReloadScreen -> Emu.Screen
screenOf sz rs = Emu.Screen
    { size = sz
    , cells = V.fromList (map V.fromList rs.rows)
    , cursor = Pos { row = rs.cursorRow, col = rs.cursorCol }
    , cursorVisible = rs.cursorVisible
    }

-- | What 'adoptPane' feeds a reloaded pane's fresh emulator to reconstruct it:
-- the bytes that replay the mode subscriptions then repaint the captured
-- screen (re-entering the alt screen when the program was in it), paired with
-- the scrollback lines to reseed. Pure, so the capture→replay round trip is
-- testable without a pty.
replayPane :: Size -> ReloadPane -> (B.ByteString, [V.Vector Cell.Cell])
replayPane sz rp =
    ( Emu.modeReplayBytes (emuModesOf rp.modes)
        <> Emu.restoreBytes restoreModes rp.screen.pen (screenOf sz rp.screen)
    , map V.fromList rp.screen.scrollback )
  where
    restoreModes = (emuModesOf rp.modes) { Emu.altScreen = rp.screen.altScreen }

-- | The size a reload capture was taken at, reconstructed from its grid;
-- 'Nothing' for a blank capture (a migrated pre-screen blob), where there is
-- nothing to preserve and the caller's default applies. See 'adoptPane'.
captureSize :: ReloadScreen -> Maybe Size
captureSize sc = case sc.rows of
    [] -> Nothing
    rs -> Just Size
        { rows = clamp (length rs)
        , cols = clamp (maximum (map length rs)) }
  where
    -- Sane bounds armor a hand-edited or corrupt blob: a Word16-overflowing
    -- or zero dimension must not produce a degenerate emulator.
    clamp n = fromIntegral (max 1 (min 1000 n))
