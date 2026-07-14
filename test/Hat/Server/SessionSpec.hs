module Hat.Server.SessionSpec (spec) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Test.Hspec

import qualified Data.Set as Set

import Hat.Geometry (Size (..))
import Hat.Log (newLogger)
import Hat.Model
import Hat.Server (cmdAttachSession, chooseCurrentOnClose, pickAttachSession)

-- A bare session with a known default working directory, inserted into a
-- fresh server so 'cmdAttachSession' can target it.
seedSession :: FilePath -> IO (ServerState, Session)
seedSession start = do
    lg <- newLogger "/dev/null"
    st <- newServerState Map.empty lg "/tmp/hat-sessionspec.sock" Nothing
    sess <- Session (SessionId 0)
        <$> newTVarIO "work"
        <*> newTVarIO Map.empty
        <*> newTVarIO 0
        <*> newTVarIO Nothing
        <*> newTVarIO (Size { rows = 24, cols = 80 })
        <*> newTVarIO []
        <*> newTVarIO start
    atomically $ modifyTVar' st.sessions (Map.insert (SessionId 0) sess)
    pure (st, sess)

spec :: Spec
spec = do
    describe "attach-session -c" $
        -- The feature behind the author's @M-c@ binding: re-anchor where
        -- new windows start. Pure server state — set by the command, read
        -- by new-window — so a state check suffices, no client needed.
        it "re-anchors the session's default working directory" $ do
            (st, sess) <- seedSession "/start"
            _ <- cmdAttachSession st Nothing ["-c", "/re/anchored"]
            readTVarIO sess.startCwd `shouldReturn` "/re/anchored"

    describe "chooseCurrentOnClose" $ do
        -- Closing the current window should jump to the last-active window,
        -- mirroring tmux, not to the lowest-numbered survivor.
        let remaining = Set.fromList [0, 2, 3]

        it "keeps the current window when it survives the close" $
            -- current still present (some other window closed): no change.
            chooseCurrentOnClose remaining 2 (Just 0) `shouldBe` Nothing

        it "jumps to the last-active window when the current is gone" $
            -- current window 1 was closed; last-active was 3, still alive.
            chooseCurrentOnClose remaining 1 (Just 3) `shouldBe` Just 3

        it "falls back to the lowest survivor when there is no last-active" $
            chooseCurrentOnClose remaining 1 Nothing `shouldBe` Just 0

        it "falls back to the lowest survivor when the last-active is also gone" $
            -- last-active pointed at window 5, which no longer exists.
            chooseCurrentOnClose remaining 1 (Just 5) `shouldBe` Just 0

    describe "pickAttachSession" $ do
        -- A fresh client attaches to the last-active session (set from the
        -- snapshot on restore), not just the lowest-id one, so a reboot
        -- returns to the session that was focused.
        let sessions = Map.fromList [(1, "a"), (2, "b"), (3, "c")]
                :: Map.Map Int String

        it "attaches to the last-active session when it still exists" $
            pickAttachSession (Just 2) sessions `shouldBe` Just (2, "b")

        it "falls back to the lowest-id session when none is marked" $
            pickAttachSession Nothing sessions `shouldBe` Just (1, "a")

        it "falls back to the lowest-id session when the marked one is gone" $
            pickAttachSession (Just 9) sessions `shouldBe` Just (1, "a")

        it "has nothing to attach to when there are no sessions" $
            pickAttachSession (Just 2) (Map.empty :: Map.Map Int String)
                `shouldBe` Nothing
