module Hat.Server.RestoreSpec (spec) where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Exception (ErrorCall (..), throwIO)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model (ServerState (..), newServerState)
import Hat.Server (PaneStart (..), finallyClearRestoring, restoreRun)

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
        it "re-execs a whitelisted program with all of its arguments" $
            restoreRun whitelist (Just ["vim", "Foo Bar.txt"])
                `shouldBe` ExecArgv ["vim", "Foo Bar.txt"]

        it "drops to a fresh shell for a non-whitelisted program" $
            restoreRun whitelist (Just ["bash", "-l"]) `shouldBe` FreshShell

        it "drops to a fresh shell when nothing was captured" $
            restoreRun whitelist Nothing `shouldBe` FreshShell
