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
import Hat.Server.Keymap (defaultKeymap)
import Hat.Server.PaneMode (dropModesForReload)
import Hat.Term.Pty qualified as Pty

-- | A fresh server for one test; on exit every pane still alive is killed
-- and its child reaped, so no test leaks a process.
withServer :: (ServerState -> IO a) -> IO a
withServer = bracket open close
  where
    open = do
        lg <- newLogger "/dev/null"
        st <- newServerState defaultKeymap lg "/tmp/hat-panemodespec.sock" Nothing
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

    describe "-k" $ do
        it "kills the pane when its copy mode exits" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["split-window", "-t", "S:0", "cat"] `shouldReturn` []
                [a, b] <- paneIds st "S:0"
                run st ["copy-mode", "-k", "-t", b] `shouldReturn` []
                run st ["send-keys", "-t", b, "-X", "cancel"] `shouldReturn` []
                paneIds st "S:0" `shouldReturn` [a]
        it "leaves the pane alone without -k" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["split-window", "-t", "S:0", "cat"] `shouldReturn` []
                [a, b] <- paneIds st "S:0"
                run st ["copy-mode", "-t", b] `shouldReturn` []
                run st ["send-keys", "-t", b, "-X", "cancel"] `shouldReturn` []
                display st b "#{pane_in_mode}" `shouldReturn` [ROutput "0"]
                paneIds st "S:0" `shouldReturn` [a, b]

    describe "send-keys" $
        it "runs a key through the copy-mode table of a pane in copy mode" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["set-option", "-g", "mode-keys", "vi"] `shouldReturn` []
                run st ["copy-mode", "-t", "S:0.0"] `shouldReturn` []
                run st ["send-keys", "-t", "S:0.0", "q"] `shouldReturn` []
                display st "S:0.0" "#{pane_in_mode}" `shouldReturn` [ROutput "0"]

    describe "tree modes" $ do
        it "choose-tree shows tree-mode until q" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["choose-tree", "-t", "S:0.0"] `shouldReturn` []
                display st "S:0.0" "#{pane_in_mode} #{pane_mode}"
                    `shouldReturn` [ROutput "1 tree-mode"]
                run st ["send-keys", "-t", "S:0.0", "q"] `shouldReturn` []
                display st "S:0.0" "#{pane_in_mode}" `shouldReturn` [ROutput "0"]
        it "choose-tree -k kills the pane when the mode exits" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["split-window", "-t", "S:0", "cat"] `shouldReturn` []
                [a, b] <- paneIds st "S:0"
                run st ["choose-tree", "-k", "-t", b] `shouldReturn` []
                run st ["send-keys", "-t", b, "q"] `shouldReturn` []
                paneIds st "S:0" `shouldReturn` [a]
        it "stacks over copy mode and returns to it" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["copy-mode", "-t", "S:0.0"] `shouldReturn` []
                run st ["choose-tree", "-t", "S:0.0"] `shouldReturn` []
                run st ["send-keys", "-t", "S:0.0", "Escape"] `shouldReturn` []
                display st "S:0.0" "#{pane_mode}" `shouldReturn` [ROutput "copy-mode"]
        it "rejects an unknown sort order" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["choose-tree", "-O", "sideways", "-t", "S:0.0"]
                    `shouldReturn` [RErr "invalid sort order"]
        it "choose-buffer has nothing to show without a buffer" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["choose-buffer", "-t", "S:0.0"] `shouldReturn` []
                display st "S:0.0" "#{pane_in_mode}" `shouldReturn` [ROutput "0"]
        it "choose-buffer d deletes the selected buffer" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["set-buffer", "-b", "one", "1"] `shouldReturn` []
                run st ["set-buffer", "-b", "two", "2"] `shouldReturn` []
                run st ["choose-buffer", "-O", "name", "-t", "S:0.0"] `shouldReturn` []
                run st ["send-keys", "-t", "S:0.0", "d"] `shouldReturn` []
                run st ["list-buffers", "-F", "#{buffer_name}"]
                    `shouldReturn` [ROutput "two"]
                display st "S:0.0" "#{pane_mode}" `shouldReturn` [ROutput "buffer-mode"]
        it "choose-client has nothing to show without a client" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["choose-client", "-t", "S:0.0"] `shouldReturn` []
                display st "S:0.0" "#{pane_in_mode}" `shouldReturn` [ROutput "0"]

    describe "dropModesForReload" $
        it "clears every mode and undoes a -Z zoom, keeping -k panes" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["split-window", "-t", "S:0", "cat"] `shouldReturn` []
                [a, b] <- paneIds st "S:0"
                run st ["choose-tree", "-k", "-Z", "-t", b] `shouldReturn` []
                display st b "#{window_zoomed_flag}" `shouldReturn` [ROutput "1"]
                dropModesForReload st
                display st b "#{pane_in_mode} #{window_zoomed_flag}"
                    `shouldReturn` [ROutput "0 0"]
                paneIds st "S:0" `shouldReturn` [a, b]

    describe "target clients" $ do
        it "switch-client -c names an unknown client loudly" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["switch-client", "-c", "client99", "-t", "S"]
                    `shouldReturn` [RErr "can't find client: client99"]
        it "detach-client -t names an unknown client loudly" $
            withServer $ \st ->
                run st ["detach-client", "-t", "client99"]
                    `shouldReturn` [RErr "can't find client: client99"]

-- | A window's pane ids, in pane-index order.
paneIds :: ServerState -> Text -> IO [Text]
paneIds st tgt = do
    out <- run st ["list-panes", "-t", tgt, "-F", "#{pane_id}"]
    pure [ t | ROutput t <- out ]
