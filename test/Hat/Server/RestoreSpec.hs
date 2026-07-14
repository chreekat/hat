module Hat.Server.RestoreSpec (spec) where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Exception (ErrorCall (..), throwIO)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model (ServerState (..), newServerState)
import Hat.Server (finallyClearRestoring)

-- A bare server-state shell, enough to exercise the restoring gate.
testState :: IO ServerState
testState = do
    lg <- newLogger "/dev/null"
    newServerState Map.empty lg "/tmp/hat-restorespec.sock" Nothing

spec :: Spec
spec =
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
