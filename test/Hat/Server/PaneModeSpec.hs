-- | Pane modes (copy mode, and tmux's mode-tree screens) driven through the
-- real command table against an in-process server with real ptys.
module Hat.Server.PaneModeSpec (spec) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model
import Hat.Server (Reply (..))
import Hat.Server.Dispatch (installHooks, runArgv)
import Hat.Term.Pty qualified as Pty

-- | A fresh server for one test; on exit every pane still alive is killed
-- and its child reaped, so no test leaks a process.
withServer :: (ServerState -> IO a) -> IO a
withServer = bracket open close
  where
    open = do
        lg <- newLogger "/dev/null"
        st <- newServerState Map.empty lg "/tmp/hat-panemodespec.sock" Nothing
        installHooks st
        pure st
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

-- | Expand a format against a target pane.
display :: ServerState -> Text -> Text -> IO [Reply]
display st tgt fmt = run st ["display-message", "-p", "-t", tgt, fmt]

spec :: Spec
spec = do
    describe "pane_in_mode" $
        it "reports the target pane's own mode, not its session's active pane" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["new-window", "-t", "S:1", "cat"] `shouldReturn` []
                run st ["copy-mode", "-t", "S:0.0"] `shouldReturn` []
                display st "S:0.0" "#{pane_in_mode} #{pane_mode}"
                    `shouldReturn` [ROutput "1 copy-mode"]
                display st "S:1.0" "#{pane_in_mode}"
                    `shouldReturn` [ROutput "0"]
