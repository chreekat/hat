module Hat.Server.RestoreSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO, writeTVar)
import Control.Exception (ErrorCall (..), bracket, throwIO)
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import System.Posix.IO (closeFd)
import System.Posix.Terminal (openPseudoTerminal)
import Test.Hspec

import Hat.Geometry (Rect (..), Size (..))
import Hat.Log (newLogger)
import Hat.Model
    (PaneId (..), ServerState (..), Session (..), StartupPhase (..),
     newServerState)
import Hat.Model.Options (OptionName (..), OptionValue (..), applyEntry)
import Hat.Transport.Wire (Autostart (..))
import Data.Maybe (fromMaybe)

import Hat.Server
    (DirenvAvailable (..), PaneStart (..), PersistDecision (..),
     Reply (..), ScrollbackCarry (..), SpawnOrigin (..),
     StartupGate (..), StorePin (..),
     captureReloadScreen, captureSize, cmdRestart, cmdRestartServer,
     defaultRestoreCommands, finallyReady, persistDecision, phaseAfterConfig,
     rebuildReloadSession, reloadSchemePush, replayPane, restoreRun,
     restoreShellExec, runCommands, snapshotHistoryLimit, startupGate,
     uniquifySessionNames)
import Hat.Server.ColorScheme (ColorScheme (..))
import Hat.Server.Layout (Layout (..))
import Hat.Server.LayoutString (emitLayout)
import Hat.Server.Persist (SessionSnap (..), Snapshot (..))
import Hat.Server.Reload
    (ReloadModes (..), ReloadPane (..), ReloadScreen (..), ReloadSession (..),
     ReloadWindow (..), emptyReloadScreen)
import qualified Hat.Term.Cell as Cell
import qualified Hat.Term.Emulator as Emu

-- A minimal non-empty tree: one named session with no windows is enough
-- for the mirror's write decision, which only inspects emptiness and
-- equality against the previous snapshot.
sampleSnapshot :: Text -> Snapshot
sampleSnapshot nm = Snapshot
    { sessions =
        [ SessionSnap
            { name = nm, startCwd = "/tmp"
            , currentIx = 0, lastIx = Nothing, windows = [] } ]
    , lastActiveSession = Nothing }

emptySnapshot :: Snapshot
emptySnapshot = Snapshot { sessions = [], lastActiveSession = Nothing }

errStrings :: [Reply] -> [Text]
errStrings replies = [e | RErr e <- replies]

-- A bare server-state shell, enough to exercise the startup gates.
testState :: IO ServerState
testState = do
    lg <- newLogger "/dev/null"
    newServerState Map.empty lg "/tmp/hat-restorespec.sock" Nothing

spec :: Spec
spec = do
    describe "finallyReady" $
        -- A crashed config load or restore must never strand the phase:
        -- every attach parks on it forever in ensureSession (the deadlock
        -- an interrupted server used to cause on the next start).
        it "lands the phase at Ready even when the startup action throws" $ do
            st <- testState
            atomically (writeTVar st.startupPhase Restoring)
            finallyReady st (throwIO (ErrorCall "restore blew up"))
                `shouldThrow` anyErrorCall
            readTVarIO st.startupPhase `shouldReturn` Ready

    -- The startup ordering contract (tmux's, minus the C machinery): config
    -- commands run on their own thread; the client that spawned the server
    -- waits for startup to land; everyone else — in particular a nested
    -- `hat run` spawned BY an if-shell condition in the config — is served
    -- immediately, or config load deadlocks on its own child.
    describe "startupGate" $ do
        let cmds = [["new-window"]]
        it "serves everyone once startup is Ready" $ do
            startupGate Ready Autostarted cmds `shouldBe` Proceed
            startupGate Ready Joined cmds `shouldBe` Proceed
        it "holds everyone while a restore is rebuilding the tree" $ do
            startupGate Restoring Autostarted cmds `shouldBe` Hold
            startupGate Restoring Joined cmds `shouldBe` Hold
        it "holds only the autostarting client during config load" $ do
            startupGate LoadingConfig Autostarted cmds `shouldBe` Hold
            startupGate LoadingConfig Joined cmds `shouldBe` Proceed
        it "never holds a restart-server batch (the guard must reject, not wait)" $ do
            let reload = [["restart-server", "/some/hat"]]
            startupGate Restoring Autostarted reload `shouldBe` Proceed
            startupGate LoadingConfig Autostarted reload `shouldBe` Proceed

        it "never holds a restart batch either (4f)" $ do
            let reload = [["restart", "/some/hat"]]
            startupGate Restoring Autostarted reload `shouldBe` Proceed
            startupGate LoadingConfig Autostarted reload `shouldBe` Proceed

    describe "phaseAfterConfig" $ do
        it "moves to Restoring when a tree remains to rebuild" $
            phaseAfterConfig True `shouldBe` Restoring
        it "lands straight at Ready when there is nothing to restore" $
            phaseAfterConfig False `shouldBe` Ready

    -- The restart-server live-screen preservation seam: capturing a pane's
    -- emulator and replaying it into a fresh one (as adoptPane does) must
    -- reproduce the visible grid, the alt-screen mode, and the scrollback.
    describe "reload screen capture and replay" $ do
        it "round-trips a pane's live screen, alt mode, and scrollback" $ do
            let sz = Size { rows = 5, cols = 20 }
            src <- Emu.newEmulator sz 1000
            forM_ [1 .. 8 :: Int] $ \i ->
                Emu.feed src (B8.pack ("history " ++ show i ++ "\r\n"))
            _ <- Emu.feed src "\ESC[?1049h\ESC[42mALT SCREEN\r\nsecond row"
            srcScr <- Emu.snapshot src
            srcLen <- Emu.scrollbackLength src
            srcSb  <- mapM (Emu.scrollbackLine src) [0 .. srcLen - 1]
            captured <- captureReloadScreen KeepScrollback src
            captured.altScreen `shouldBe` True
            let rp = ReloadPane
                    { cwd = "/tmp", masterFd = 0, childPid = 0
                    , modes = ReloadModes False False 0, screen = captured }
                (bytes, sb) = replayPane sz rp
            dst <- Emu.newEmulator sz 1000
            _ <- Emu.feed dst bytes
            Emu.seedScrollback dst sb
            dstScr <- Emu.snapshot dst
            dstMode <- Emu.modes dst
            dstLen <- Emu.scrollbackLength dst
            dstSb  <- mapM (Emu.scrollbackLine dst) [0 .. dstLen - 1]
            dstScr.cells `shouldBe` srcScr.cells
            dstMode.altScreen `shouldBe` True
            dstSb `shouldBe` srcSb

        -- Bug (2026-08-01): a reload repainted the grid but left the emulator's
        -- pen at the last painted cell's colour, so a restart-server left the
        -- still-alive shell's echoed keystrokes coloured (green at the prompt
        -- until a redraw reset it). The fix carries the live pen in the
        -- capture and restores it, so the program's next output is styled
        -- exactly as it was — whatever the last painted cell.
        let penAfterReload feedBytes = do
                let sz = Size { rows = 3, cols = 20 }
                src <- Emu.newEmulator sz 1000
                _ <- Emu.feed src feedBytes
                captured <- captureReloadScreen KeepScrollback src
                let rp = ReloadPane
                        { cwd = "/tmp", masterFd = 0, childPid = 0
                        , modes = ReloadModes False False 0, screen = captured }
                    (bytes, _) = replayPane sz rp
                dst <- Emu.newEmulator sz 1000
                _ <- Emu.feed dst bytes
                Emu.currentPen dst   -- the pen the next echoed keystroke takes
        it "restores a default live pen (coloured glyph, then the prompt's reset)" $
            -- the last VISIBLE cell is green, but the live pen is default
            penAfterReload "\ESC[32m$\ESC[0m " `shouldReturn` Cell.defaultStyle
        it "restores a non-default live pen (program mid-colour at the reload)" $
            -- green foreground still active, no reset: the pen stays green
            penAfterReload "\ESC[32mgreen"
                `shouldReturn` Cell.defaultStyle { Cell.fg = Cell.Indexed 2 }

        -- restart-server -C: restart as memory cleanup — the handover keeps
        -- the live grid but sheds every scrollback line.
        it "drops the scrollback from the capture when asked" $ do
            let sz = Size { rows = 5, cols = 20 }
            src <- Emu.newEmulator sz 1000
            forM_ [1 .. 8 :: Int] $ \i ->
                Emu.feed src (B8.pack ("history " ++ show i ++ "\r\n"))
            srcScr <- Emu.snapshot src
            captured <- captureReloadScreen DropScrollback src
            captured.scrollback `shouldBe` []
            captured.rows `shouldBe` map V.toList (V.toList srcScr.cells)

        -- Bug capture (field crash 2026-07-28): a pane captured LARGER than
        -- the rebuild default — cursor beyond the small grid, rows that would
        -- wrap — must be adopted at its captured size. Replayed into 24x80
        -- instead, the wrapped-and-clamped state made a later reconcile
        -- shrink abort the whole process inside libvterm ("screen_resize
        -- failed to update cursor position").
        it "adopts an oversized capture at its captured size and survives the shrink" $ do
            let wideRow = replicate 330 Cell.blankCell { Cell.text = "x" }
                sc = ReloadScreen
                    { altScreen = True, cursorRow = 39, cursorCol = 2
                    , cursorVisible = True
                    , rows = replicate 42 wideRow, scrollback = []
                    , pen = Cell.defaultStyle }
                rp = ReloadPane
                    { cwd = "/", masterFd = 0, childPid = 0
                    , modes = ReloadModes False False 0, screen = sc }
                esz = fromMaybe (Size 24 80) (captureSize sc)
            esz `shouldBe` Size 42 330
            e <- Emu.newEmulator esz 1000
            let (bytes, sb) = replayPane esz rp
            _ <- Emu.feed e bytes
            Emu.seedScrollback e sb
            Emu.resize e (Size 11 80)
            scr <- Emu.snapshot e
            scr.size `shouldBe` Size 11 80

    -- A reloaded session must come back at its captured window area, not a
    -- 24x80 placeholder (bug 4b).
    describe "reload session size" $
        it "rebuilds a reloaded session at its captured window area" $
            bracket openPseudoTerminal (\(m, s) -> closeFd m >> closeFd s) $
                \(m, _) -> do
            st <- testState
            let lay = emitLayout
                    Rect { startRow = 0, endRow = 30, startCol = 0, endCol = 100 }
                    (Leaf (PaneId 0))
                rp = ReloadPane
                    { cwd = "/tmp", masterFd = fromIntegral m, childPid = 999999
                    , modes = ReloadModes False False 0
                    , screen = emptyReloadScreen }
                rw = ReloadWindow
                    { ix = 0, name = "sh", layout = lay, active = 0
                    , lastActive = Nothing, autoRename = True, panes = [rp] }
                rsess = ReloadSession
                    { name = "0", startCwd = "/tmp", currentIx = 0
                    , lastIx = Nothing, windows = [rw] }
            rebuildReloadSession st rsess
            sessMap <- readTVarIO st.sessions
            szs <- mapM (readTVarIO . (.lastSize)) (Map.elems sessMap)
            szs `shouldBe` [Size { rows = 30, cols = 100 }]

    -- Bug f3: a reload re-arms the ?2031 subscription (replayPane) but the
    -- surviving app never re-emits it, so without a re-push it renders the old
    -- scheme until the OS scheme next CHANGES. adoptPane must push the current
    -- scheme once into a subscribed pane.
    describe "reload color-scheme re-push" $ do
        it "re-pushes the current scheme to a pane that held the ?2031 subscription" $ do
            reloadSchemePush (ReloadModes True False 0) (Just SchemeDark)
                `shouldBe` Just "\ESC[?997;1n"
            reloadSchemePush (ReloadModes True False 0) (Just SchemeLight)
                `shouldBe` Just "\ESC[?997;2n"

        it "pushes nothing to a pane that never subscribed" $
            reloadSchemePush (ReloadModes False False 0) (Just SchemeDark)
                `shouldBe` Nothing

        it "pushes nothing when the server does not yet know the scheme" $
            reloadSchemePush (ReloadModes True False 0) Nothing
                `shouldBe` Nothing

    -- Fail loud rather than reload on top of an in-flight startup: a second
    -- restart-server issued mid-restore would capture a half-rebuilt tree.
    -- All cases pass a nonexistent target so none ever re-execs.
    describe "restart-server reload-in-progress guard" $ do
        it "refuses to reload while a restore is in progress" $ do
            st <- testState
            atomically (writeTVar st.startupPhase Restoring)
            errs <- errStrings <$> cmdRestartServer st Nothing ["/no/such/hat"]
            errs `shouldSatisfy` any (T.isInfixOf "in progress")

        it "refuses to reload while the config is still loading" $ do
            st <- testState
            atomically (writeTVar st.startupPhase LoadingConfig)
            errs <- errStrings <$> cmdRestartServer st Nothing ["/no/such/hat"]
            errs `shouldSatisfy` any (T.isInfixOf "in progress")

        it "passes the guard when idle, reaching the normal target check" $ do
            st <- testState
            errs <- errStrings <$> cmdRestartServer st Nothing ["/no/such/hat"]
            errs `shouldSatisfy` any (T.isInfixOf "no such binary")

    -- `restart` (bug 4f) rides the whole restart-server path — guard, flags,
    -- target check — through the real command engine, so a bad reload is
    -- rejected before any client is told to restart.
    describe "restart" $ do
        it "dispatches from the command table into the reload path (4f)" $ do
            st <- testState
            errs <- errStrings <$> runCommands st Nothing [["restart", "/no/such/hat"]]
            errs `shouldSatisfy` any (T.isInfixOf "restart: no such binary")

        it "shares the reload-in-progress guard (4f)" $ do
            st <- testState
            atomically (writeTVar st.startupPhase Restoring)
            errs <- errStrings <$> cmdRestart st Nothing ["/no/such/hat"]
            errs `shouldSatisfy` any (T.isInfixOf "in progress")

    describe "restart-server flags" $ do
        it "accepts -C, reaching the normal target check" $ do
            st <- testState
            errs <- errStrings <$> cmdRestartServer st Nothing ["-C", "/no/such/hat"]
            errs `shouldSatisfy` any (T.isInfixOf "no such binary")

        -- Fail loud, never silently accept: a flag we don't implement errors.
        it "rejects an unknown flag" $ do
            st <- testState
            errs <- errStrings <$> cmdRestartServer st Nothing ["-X", "/no/such/hat"]
            errs `shouldSatisfy` any (T.isInfixOf "unknown flag")

    describe "restoreRun" $ do
        let whitelist = ["vim", "less"]

        -- b7/f: the whole argv comes back, and an argument with spaces stays
        -- one argument — exec'd directly, never re-split by a shell.
        it "re-execs a directly-launched whitelisted program with all its args" $
            restoreRun whitelist Direct (Just ["vim", "Foo Bar.txt"])
                `shouldBe` ExecArgv ["vim", "Foo Bar.txt"]

        it "drops to a fresh shell for a non-whitelisted program" $
            restoreRun whitelist Direct (Just ["bash", "-l"]) `shouldBe` FreshShell

        it "drops to a fresh shell when nothing was captured" $
            restoreRun whitelist Direct Nothing `shouldBe` FreshShell

        -- d: a whitelisted program that was originally started from inside the
        -- pane's interactive shell comes back through that shell (so the shell
        -- init — direnv, per-dir env, PATH — runs), not exec'd bare.
        it "runs a shell-spawned whitelisted program through the shell" $
            restoreRun whitelist ShellSpawned (Just ["vim", "Foo Bar.txt"])
                `shouldBe` ShellExecArgv ["vim", "Foo Bar.txt"]

        -- A shell-spawned but non-whitelisted program still just drops to a
        -- fresh shell — the whitelist gate comes first.
        it "drops a shell-spawned non-whitelisted program to a fresh shell" $
            restoreRun whitelist ShellSpawned (Just ["bash", "-l"])
                `shouldBe` FreshShell

        -- a2: direnv present → `direnv exec <cwd> <argv>`, so the per-directory
        -- env loads under any shell; argv stays unsplit.
        it "wraps a shell-spawned program as direnv exec when direnv is present" $
            restoreShellExec DirenvOnPath "/bin/bash" "/work/dir"
                ["vim", "Foo Bar.txt"]
                `shouldBe`
                    ("direnv", ["exec", "/work/dir", "vim", "Foo Bar.txt"])

        -- a2: no direnv → login-shell relaunch (-i sources rc), argv unsplit.
        it "falls back to a shell relaunch when direnv is absent" $
            restoreShellExec DirenvAbsent "/bin/bash" "/work/dir"
                ["vim", "Foo Bar.txt"]
                `shouldBe`
                    ( "/bin/bash"
                    , [ "-i", "-c", "exec \"$@\""
                      , "hat-restore", "vim", "Foo Bar.txt" ] )

        -- df: a restored claude pane must resume the same conversation, so
        -- whatever argv was saved (claude, claude -r foo, …), it comes back
        -- as @claude --continue@ — never the original arguments.
        it "restores claude as claude --continue regardless of saved args" $
            restoreRun defaultRestoreCommands Direct (Just ["claude", "-r", "foo"])
                `shouldBe` ExecArgv ["claude", "--continue"]

        it "keeps the saved claude binary path when rewriting to --continue" $
            restoreRun defaultRestoreCommands Direct
                (Just ["/nix/store/abc/bin/.claude-wrapped", "-r", "foo"])
                `shouldBe`
                    ExecArgv ["/nix/store/abc/bin/.claude-wrapped", "--continue"]

        -- The --continue rewrite carries through the shell wrapper too, so a
        -- shell-spawned claude both resumes and picks up direnv.
        it "rewrites a shell-spawned claude to --continue through the shell" $
            restoreRun defaultRestoreCommands ShellSpawned (Just ["claude", "-r", "foo"])
                `shouldBe` ShellExecArgv ["claude", "--continue"]

    describe "persistDecision" $ do
        let one = sampleSnapshot "one"
            two = sampleSnapshot "two"

        -- The store is pinned once kill-server captured the final tree: the
        -- mirror must never write again, even when the live tree has since
        -- changed (a stray fresh session on the dying server must not
        -- overwrite the saved 25-pane tree).
        it "never writes when the store is pinned, even on a change" $
            persistDecision Pinned (Just one) two `shouldBe` PinnedSkip

        it "never writes when pinned, even with no prior snapshot" $
            persistDecision Pinned Nothing one `shouldBe` PinnedSkip

        -- An empty tree is never mirrored: whether an empty store survives is
        -- decided at shutdown, not by the background loop.
        it "skips an empty snapshot" $
            persistDecision Unpinned Nothing emptySnapshot `shouldBe` EmptySkip

        it "skips when the snapshot is unchanged since the last write" $
            persistDecision Unpinned (Just one) one `shouldBe` UnchangedSkip

        it "writes a changed, non-empty snapshot when not pinned" $
            persistDecision Unpinned (Just one) two `shouldBe` WriteSnapshot

        it "writes the first non-empty snapshot when nothing was written yet" $
            persistDecision Unpinned Nothing one `shouldBe` WriteSnapshot

    -- bb: a restored snapshot's sessions must not steal a live session's
    -- name; a collision gets the first free numeric suffix.
    describe "uniquifySessionNames" $
        it "dodges live and just-restored names with -2, -3, … suffixes" $
            uniquifySessionNames ["main", "log"] ["main", "fresh", "main"]
                `shouldBe` ["main-2", "fresh", "main-3"]

    -- bb: @snapshot-limit drives the history depth; an unset or garbage
    -- value means the default, never a silently different behavior.
    describe "snapshotHistoryLimit" $ do
        it "reads @snapshot-limit from the options" $ do
            st <- testState
            atomically $ modifyTVar' st.options
                (applyEntry (OptUser "@snapshot-limit") (OVText "3"))
            snapshotHistoryLimit st `shouldReturn` 3

        it "defaults to 10, also for an unparsable value" $ do
            st <- testState
            snapshotHistoryLimit st `shouldReturn` 10
            atomically $ modifyTVar' st.options
                (applyEntry (OptUser "@snapshot-limit") (OVText "many"))
            snapshotHistoryLimit st `shouldReturn` 10
