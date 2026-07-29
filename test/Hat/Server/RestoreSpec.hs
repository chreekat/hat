module Hat.Server.RestoreSpec (spec) where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Exception (ErrorCall (..), throwIO)
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Test.Hspec

import Hat.Geometry (Size (..))
import Hat.Log (newLogger)
import Hat.Model (ServerState (..), newServerState)
import Data.Maybe (fromMaybe)

import System.Timeout (timeout)

import Hat.Server
    (DirenvAvailable (..), PaneStart (..), PersistDecision (..),
     Reply (..), SpawnOrigin (..), StorePin (..), awaitRestoreForCommand,
     captureReloadScreen, captureSize, cmdRestartServer, defaultRestoreCommands,
     finallyClearRestoring, persistDecision, replayPane, restoreRun,
     restoreShellExec)
import Hat.Server.Persist (SessionSnap (..), Snapshot (..))
import Hat.Server.Reload (ReloadModes (..), ReloadPane (..), ReloadScreen (..))
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

-- A bare server-state shell, enough to exercise the restoring gate.
testState :: IO ServerState
testState = do
    lg <- newLogger "/dev/null"
    newServerState Map.empty lg "/tmp/hat-restorespec.sock" Nothing

spec :: Spec
spec = do
    describe "finallyClearRestoring" $
        -- A crashed config load or restore must never leave the gate set:
        -- every attach parks on it forever in ensureSession (the deadlock
        -- an interrupted server used to cause on the next start).
        it "clears the restoring gate even when the startup action throws" $ do
            st <- testState
            atomically (writeTVar st.restoring True)
            finallyClearRestoring st (throwIO (ErrorCall "restore blew up"))
                `shouldThrow` anyErrorCall
            readTVarIO st.restoring `shouldReturn` False

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
            captured <- captureReloadScreen src
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
                    , rows = replicate 42 wideRow, scrollback = [] }
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

    -- Fail loud rather than reload on top of an in-flight restore: a second
    -- restart-server issued mid-restore would capture a half-rebuilt tree.
    -- Both cases pass a nonexistent target so neither ever re-execs.
    -- Bug f3a: a control command (e.g. list-panes) issued during a reload's
    -- restore must wait for the tree to be whole, not read a half-rebuilt one
    -- and report an empty/partial view. 'restart-server' is the exception: it
    -- must still reject an in-flight reload rather than wait for it.
    describe "control command restore gate" $ do
        it "blocks a normal command until the restore gate clears" $ do
            st <- testState
            atomically (writeTVar st.restoring True)
            -- Gate set: the command parks, so a bounded wait never completes.
            timeout 100000 (awaitRestoreForCommand st [["list-panes"]])
                `shouldReturn` Nothing
            atomically (writeTVar st.restoring False)
            -- Gate clear: it returns at once.
            timeout 2000000 (awaitRestoreForCommand st [["list-panes"]])
                `shouldReturn` Just ()

        it "never blocks restart-server, so it can reject an in-flight reload" $ do
            st <- testState
            atomically (writeTVar st.restoring True)
            timeout 2000000 (awaitRestoreForCommand st [["restart-server", "/x"]])
                `shouldReturn` Just ()

    describe "restart-server reload-in-progress guard" $ do
        it "refuses to reload while a restore is in progress" $ do
            st <- testState
            atomically (writeTVar st.restoring True)
            errs <- errStrings <$> cmdRestartServer st Nothing ["/no/such/hat"]
            errs `shouldSatisfy` any (T.isInfixOf "in progress")

        it "passes the guard when idle, reaching the normal target check" $ do
            st <- testState
            errs <- errStrings <$> cmdRestartServer st Nothing ["/no/such/hat"]
            errs `shouldSatisfy` any (T.isInfixOf "no such binary")

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
