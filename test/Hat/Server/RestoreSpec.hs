module Hat.Server.RestoreSpec (spec) where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Exception (ErrorCall (..), throwIO)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model (ServerState (..), newServerState)
import Hat.Server
    (PaneStart (..), PersistDecision (..), SpawnOrigin (..), StorePin (..),
     defaultRestoreCommands, finallyClearRestoring, persistDecision,
     restoreRun, shellExecArgs)
import Hat.Server.Persist (SessionSnap (..), Snapshot (..))

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

        -- d: the shell wrapper fires the shell's prompt hooks before exec,
        -- so direnv (bash installs it on PROMPT_COMMAND, which -c never
        -- reaches on its own) loads the pane's per-directory env before the
        -- program runs. The argv rides as positional params, unsplit.
        it "runs the shell's prompt hooks before exec so direnv loads" $
            shellExecArgs ["vim", "Foo Bar.txt"] `shouldBe`
                [ "-i", "-c", "eval \"$PROMPT_COMMAND\"; exec \"$@\""
                , "hat-restore", "vim", "Foo Bar.txt" ]

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
