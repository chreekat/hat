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
import Control.Monad (filterM, forM_)
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.Directory
    (renameFile)
import System.Posix.Types (Fd (..))

import Hat.Geometry
import Hat.Log
import Hat.Model
import Hat.Model.Options
import Hat.Server.Persist (PaneSnap (..), encodeSnapshotJson)
import Hat.Server.Reload
    (Handover (..), HotPane (..), HotSession (..), HotWindow (..)
    , ReloadCleanup (..), ReloadHot (..), ReloadModes (..), ReloadScreen (..)
    , ReloadTree (..), decodeHandover)
import Hat.Term.Pty qualified
import Hat.Server.ColorScheme
    ( ColorScheme, schemeReport )
import Hat.Server.Pane
import Hat.Server.Rebuild (rebuildSession)
import Hat.Server.Snapshot (captureTree)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu

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
-- 'ReloadHot' (the tree exactly as 'captureTree' records it, plus each pane's
-- hot state), and the version-independent 'ReloadCleanup' core (the listening
-- socket fd and the flat list of every pane's (master fd, child pid), so a
-- version-mismatched reload can hang the inherited processes up cleanly rather
-- than orphan them). Tree and hot state come from one walk, so they agree
-- pane-for-pane by construction.
captureReload :: ScrollbackCarry -> ServerState -> IO (ReloadCleanup, ReloadHot)
captureReload carry st = do
    (snap, panes) <- captureTree st
    (lsName, mfd) <- atomically $ do
        sessMap <- readTVar st.sessions
        lsId    <- readTVar st.lastSession
        lsName  <- traverse (readTVar . (.name)) (lsId >>= (`Map.lookup` sessMap))
        mfd     <- readTVar st.listenFd
        pure (lsName, mfd)
    hots <- mapM (captureHotPane carry) panes
    pure ( ReloadCleanup
             { listenFd = fromMaybe (-1) mfd
             , live = [(p.masterFd, p.childPid) | p <- hots] }
         , ReloadHot
             { tree = encodeSnapshotJson snap, hot = hots
             , lastSession = lsName } )

captureHotPane :: ScrollbackCarry -> Pane -> IO HotPane
captureHotPane carry pane = do
    let Fd fd = Hat.Term.Pty.masterFd pane.pty
    ms <- Emu.modes pane.emulator
    km <- Emu.keyModes pane.emulator
    sc <- captureReloadScreen carry pane.emulator
    pure HotPane
        { masterFd = fromIntegral fd
        , childPid = fromIntegral (Hat.Term.Pty.pid pane.pty)
        , modes = reloadModesOf ms km
        , screen = sc }

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

-- | The app-set subscriptions to carry across a reload; the inverse
-- rebuild happens in 'adoptPane'.
reloadModesOf :: Emu.Modes -> Emu.KeyModes -> ReloadModes
reloadModesOf m km = ReloadModes
    { colorReport = m.colorReport
    , focusReport = m.focusReport
    , mouse = case m.mouse of
        Emu.MouseOff   -> 0
        Emu.MouseClick -> 1
        Emu.MouseDrag  -> 2
        Emu.MouseMove  -> 3
    , modifyOtherKeys = km.modifyOtherKeys
    , kittyFlags = km.kittyFlags
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
rebuildReload :: ServerState -> ReloadTree -> IO ()
rebuildReload st rt = do
    forM_ rt.sessions (rebuildReloadSession st)
    forM_ rt.currentSession $ \nm ->
        resolveSessionByName st nm $ \s ->
            atomically (writeTVar st.lastActiveSession (Just s.id))
    forM_ rt.lastSession $ \nm ->
        resolveSessionByName st nm $ \s ->
            atomically (writeTVar st.lastSession (Just s.id))

resolveSessionByName :: ServerState -> Text -> (Session -> IO ()) -> IO ()
resolveSessionByName st nm act = do
    sessMap <- readTVarIO st.sessions
    hits <- filterM (fmap (== nm) . readTVarIO . (.name)) (Map.elems sessMap)
    forM_ (listToMaybe hits) act

rebuildReloadSession :: ServerState -> HotSession -> IO ()
rebuildReloadSession st rsess = do
    env <- restoreEnv
    histLimit <- (.historyLimit) <$> readTVarIO st.options
    rebuildSession st env (const (adoptPane st histLimit)) rsess

-- | Build a pane around an inherited pty ('Hat.Term.Pty.adopt') and a blank
-- emulator, for the reload path — the analogue of 'spawnPane' that re-adopts
-- a running child instead of forking a new one.
adoptPane :: ServerState -> Int -> Size -> (PaneSnap, HotPane) -> IO Pane
adoptPane st histLimit sz (psnap, rp) = do
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
            , dead = deadVar, startCwd = T.unpack psnap.cwd, mode = modeVar
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

-- | Rebuild the key protocols a reload carried, so the adopted pane keeps
-- encoding keys as the surviving program expects. Inverse of 'reloadModesOf'.
keyModesOf :: ReloadModes -> Emu.KeyModes
keyModesOf rm = Emu.KeyModes
    { modifyOtherKeys = rm.modifyOtherKeys, kittyFlags = rm.kittyFlags }

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
-- the bytes that replay the mode and key-protocol subscriptions then repaint the captured
-- screen (re-entering the alt screen when the program was in it), paired with
-- the scrollback lines to reseed. Pure, so the capture→replay round trip is
-- testable without a pty.
replayPane :: Size -> HotPane -> (B.ByteString, [V.Vector Cell.Cell])
replayPane sz rp =
    ( Emu.modeReplayBytes (emuModesOf rp.modes)
        <> Emu.keyModeReplayBytes (keyModesOf rp.modes)
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
