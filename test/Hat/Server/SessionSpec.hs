module Hat.Server.SessionSpec (spec) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Test.Hspec

import qualified Data.Set as Set

import Hat.Geometry (Size (..))
import Hat.Log (newLogger)
import Hat.Model
import Hat.Model.Options
    (Options (..), OptionName (..), OptionValue (..)
    , emptyDelta, singletonDelta)
import Hat.Server
    (cmdAttachSession, chooseCurrentOnClose, pickActivityTarget, pickAttachSession)

-- A bare session with the given id inserted into an existing server.
addSession :: ServerState -> Int -> IO Session
addSession st n = do
    sess <- Session (SessionId n)
        <$> newTVarIO "s"
        <*> newTVarIO Map.empty
        <*> newTVarIO 0
        <*> newTVarIO Nothing
        <*> newTVarIO (Size { rows = 24, cols = 80 })
        <*> newTVarIO []
        <*> newTVarIO "/"
        <*> newTVarIO emptyDelta
    atomically $ modifyTVar' st.sessions (Map.insert (SessionId n) sess)
    pure sess

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
        <*> newTVarIO emptyDelta
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

    describe "scoped option resolution" $ do
        it "a session overlay shadows the global chain for that session only" $ do
            lg <- newLogger "/dev/null"
            st <- newServerState Map.empty lg "/tmp/hat-scopespec.sock" Nothing
            atomically $ writeTVar st.globalSessionOptions
                (singletonDelta OptPrefix (OVText "C-b"))
            sessA <- addSession st 0
            sessB <- addSession st 1
            atomically $ writeTVar sessA.options
                (singletonDelta OptPrefix (OVText "C-a"))
            a <- atomically (resolveForSession st sessA)
            b <- atomically (resolveForSession st sessB)
            a.prefix `shouldBe` "C-a"   -- session A's own set
            b.prefix `shouldBe` "C-b"   -- B falls through to the global

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

    describe "pickActivityTarget" $ do
        -- The @<leader> a@ jump: an activity-marked window wins, chosen in
        -- the same cyclic next-window order used by @next-window -a@; with no
        -- activity it degrades to @last-window@.
        let order = [0, 1, 2, 3]

        it "jumps to the next activity window after the current, cyclically" $
            -- current is 2; activity on 0 and 1; the cyclic scan wraps past
            -- 3 and lands on 0 (the first flagged one after the current).
            pickActivityTarget order 2 (Set.fromList [0, 1]) (Just 3)
                `shouldBe` Just 0

        it "prefers the nearest activity window ahead of the current" $
            -- current is 0; activity on 2 and 3; 2 comes first in the scan.
            pickActivityTarget order 0 (Set.fromList [2, 3]) (Just 1)
                `shouldBe` Just 2

        it "prioritizes activity over the last-active fallback" $
            -- last-active is 1, but window 3 carries activity, so it wins.
            pickActivityTarget order 0 (Set.fromList [3]) (Just 1)
                `shouldBe` Just 3

        it "falls back to last-window when no window has activity" $
            pickActivityTarget order 2 Set.empty (Just 1) `shouldBe` Just 1

        it "has nothing to do with neither activity nor a last window" $
            pickActivityTarget order 2 Set.empty Nothing `shouldBe` Nothing

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
