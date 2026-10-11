-- | The pane commands driven through the real command table against an
-- in-process server, with real ptys behind every pane.
module Hat.Server.PaneCommandSpec (spec) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM_, unless, void)
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
        st <- newServerState Map.empty lg "/tmp/hat-panecommandspec.sock" Nothing
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

-- | Poll until a condition holds; bound it with 'timeout'.
untilM :: IO Bool -> IO ()
untilM cond = do
    ok <- cond
    unless ok (threadDelay 20_000 >> untilM cond)

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

-- | A window's panes as "left,top WxH" from their cells, in pane order.
geometry :: ServerState -> Text -> IO [Text]
geometry st tgt = do
    out <- run st ["list-panes", "-t", tgt, "-F",
        "#{pane_left},#{pane_top} #{e|+:#{e|-:#{pane_right},#{pane_left}},1}x\
        \#{e|+:#{e|-:#{pane_bottom},#{pane_top}},1}"]
    pure [ t | ROutput t <- out ]

spec :: Spec
spec = do
    describe "kill-pane" $ do
        it "kills the pane a full -t target names, not the current one" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["new-window", "-d", "-t", "S:9", "sleep 30"] `shouldReturn` []
                run st ["kill-pane", "-t", "S:9.0"] `shouldReturn` []
                windowIndices st "S" `shouldReturn` [0]
        it "with no -t in a hook, kills the hook's own pane" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["set-option", "-g", "remain-on-exit", "on"] `shouldReturn` []
                run st ["set-hook", "-g", "pane-died", "kill-pane"] `shouldReturn` []
                [p0] <- paneIds st "S:0"
                run st ["split-window", "-d", "-t", "S:0", "true"] `shouldReturn` []
                gone <- timeout 5_000_000 $ untilM ((== 1) . length <$> paneIds st "S:0")
                gone `shouldBe` Just ()
                paneIds st "S:0" `shouldReturn` [p0]
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

    describe "split-window" $ do
            it "sizes the new pane from -l, in cells or a percentage of the pane" $
                withServer $ \st -> do
                    run st ["new-session", "-d", "-s", "S", "-x", "80", "-y", "24", "sleep 30"]
                        `shouldReturn` []
                    [p0] <- paneIds st "S:0"
                    run st ["split-window", "-d", "-h", "-l", "20", "-t", p0, "sleep 30"]
                        `shouldReturn` []
                    run st ["split-window", "-d", "-v", "-l", "25%", "-t", p0, "sleep 30"]
                        `shouldReturn` []
                    geometry st "S:0" `shouldReturn` ["0,0 59x17", "0,18 59x6", "60,0 20x24"]
                    run st ["split-window", "-d", "-v", "-l", "x", "-t", p0]
                        `shouldReturn` [RErr "invalid tiled geometry invalid"]
            it "-E makes an empty pane, running nothing, and refuses a command" $
                withServer $ \st -> do
                    run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                    [out] <- run st ["split-window", "-d", "-E", "-P", "-F",
                        "#{pane_dead}:#{pane_pid}", "-t", "S:0"]
                    out `shouldBe` ROutput "0:"
                    run st ["split-window", "-d", "-E", "-t", "S:0", "true"]
                        `shouldReturn` [RErr "command cannot be given for empty pane"]

    describe "swap-pane" $
            it "takes the marked pane as its source when -s is not given" $
                withServer $ \st -> do
                    run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                    run st ["split-window", "-d", "-t", "S:0", "sleep 30"] `shouldReturn` []
                    run st ["split-window", "-d", "-t", "S:0", "sleep 30"] `shouldReturn` []
                    [a, b, c] <- paneIds st "S:0"
                    run st ["select-pane", "-m", "-t", c] `shouldReturn` []
                    run st ["swap-pane", "-d", "-t", a] `shouldReturn` []
                    paneIds st "S:0" `shouldReturn` [c, b, a]

    describe "select-pane" $
            it "-d and -e turn a pane's input off and on, -T sets its title" $
                withServer $ \st -> do
                    run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                    let fmt f = run st ["display-message", "-p", "-t", "S:0.0", f]
                    run st ["select-pane", "-d", "-t", "S:0.0"] `shouldReturn` []
                    fmt "#{pane_input_off}" `shouldReturn` [ROutput "1"]
                    run st ["select-pane", "-e", "-t", "S:0.0"] `shouldReturn` []
                    fmt "#{pane_input_off}" `shouldReturn` [ROutput "0"]
                    run st ["select-pane", "-T", "named", "-t", "S:0.0"] `shouldReturn` []
                    fmt "#{pane_title}" `shouldReturn` [ROutput "named"]

    describe "join-pane" $
            it "refuses to join a pane to itself" $
                withServer $ \st -> do
                    run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                    run st ["join-pane", "-s", "S:0.0", "-t", "S:0.0"]
                        `shouldReturn` [RErr "source and target panes must be different"]

    describe "break-pane" $
            it "moves the pane to a new window, printing where it went" $
                withServer $ \st -> do
                    run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                    run st ["split-window", "-d", "-t", "S:0", "sleep 30"] `shouldReturn` []
                    [_, b] <- paneIds st "S:0"
                    run st ["break-pane", "-d", "-P", "-F", "#{window_index}:#{pane_id}",
                            "-n", "out", "-s", b, "-t", "S:"]
                        `shouldReturn` [ROutput ("1:" <> b)]
                    windowIndices st "S" `shouldReturn` [0, 1]
