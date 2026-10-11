-- | The pane commands driven through the real command table against an
-- in-process server, with real ptys behind every pane.
module Hat.Server.PaneCommandSpec (spec) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (filterM, forM_, void)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model
import Hat.Server (Reply (..))
import Hat.Server.Dispatch (runArgv)
import Hat.Term.Pty qualified as Pty

-- | A fresh server for one test; on exit every pane still alive is killed
-- and its child reaped, so no test leaks a process.
withServer :: (ServerState -> IO a) -> IO a
withServer = bracket open close
  where
    open = do
        lg <- newLogger "/dev/null"
        newServerState Map.empty lg "/tmp/hat-panecommandspec.sock" Nothing
    close st = do
        panes <- allPanes st
        void (run st ["kill-server"])
        forM_ panes $ \p -> timeout 2_000_000 (Pty.waitExit p.pty)

allPanes :: ServerState -> IO [Pane]
allPanes st = atomically $ do
    sessions <- Map.elems <$> readTVar st.sessions
    wins <- concat <$> mapM (fmap Map.elems . readTVar . (.windows)) sessions
    concat <$> mapM (fmap Map.elems . readTVar . (.panes)) wins

run :: ServerState -> [Text] -> IO [Reply]
run st = runArgv st Nothing

-- | The window indices of the named session.
windowIndices :: ServerState -> Text -> IO [Int]
windowIndices st name = atomically $ do
    sessions <- Map.elems <$> readTVar st.sessions
    named <- filterM (\s -> (== name) <$> readTVar s.name) sessions
    concat <$> mapM (fmap Map.keys . readTVar . (.windows)) named

-- | A window's pane ids, in pane-index order.
paneIds :: ServerState -> Text -> IO [Text]
paneIds st tgt = do
    out <- run st ["list-panes", "-t", tgt, "-F", "#{pane_id}"]
    pure [ t | ROutput t <- out ]

spec :: Spec
spec = describe "kill-pane" $ do
    it "kills the pane a full -t target names, not the current one" $
        withServer $ \st -> do
            run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
            run st ["new-window", "-d", "-t", "S:9", "sleep 30"] `shouldReturn` []
            run st ["kill-pane", "-t", "S:9.0"] `shouldReturn` []
            windowIndices st "S" `shouldReturn` [0]
    it "-a kills every other pane, narrowed by -f" $
        withServer $ \st -> do
            run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
            run st ["split-window", "-d", "-t", "S:0", "sleep 30"] `shouldReturn` []
            run st ["split-window", "-d", "-t", "S:0", "sleep 30"] `shouldReturn` []
            [a, b, c] <- paneIds st "S:0"
            run st ["kill-pane", "-f", "1", "-t", a]
                `shouldReturn` [RErr "-f only valid with -a"]
            run st ["kill-pane", "-a", "-f", "#{==:#{pane_id}," <> c <> "}", "-t", a]
                `shouldReturn` []
            paneIds st "S:0" `shouldReturn` [a, b]
            run st ["kill-pane", "-a", "-t", b] `shouldReturn` []
            paneIds st "S:0" `shouldReturn` [b]
