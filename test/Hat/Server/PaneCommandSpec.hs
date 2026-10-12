-- | The pane commands driven through the real command table against an
-- in-process server, with real ptys behind every pane.
module Hat.Server.PaneCommandSpec (spec) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM_, unless, void)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Posix.Process (getProcessID)
import System.Posix.Unistd (getSystemID, nodeName)
import System.Timeout (timeout)
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model
import Hat.Model.Options (Options (..))
import Hat.Server (Reply (..))
import Hat.Server.Dispatch (installHooks, runArgv)
import Hat.Server.Persist (decodeSnapshotJson, encodeSnapshotJson)
import Hat.Server.Snapshot (captureSnapshot, restoreSnapshot)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Pty qualified as Pty
import Data.Vector qualified as V

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

    describe "server formats" $
        it "#{pid} is the server's process id" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                pid <- getProcessID
                run st ["display-message", "-p", "#{pid}"]
                    `shouldReturn` [ROutput (T.pack (show pid))]

    describe "new-session" $
        it "execs a command of several words as its argv, which the start formats quote" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "Q", "--", "sh", "-c", "sleep 30", "it's", "a b"]
                    `shouldReturn` []
                run st ["display-message", "-p", "-t", "Q:0.0", "#{pane_start_command_list}"]
                    `shouldReturn` [ROutput "'sh' '-c' 'sleep 30' 'it'\\''s' 'a b'"]
                run st ["display-message", "-p", "-t", "Q:0.0", "#{pane_start_command}"]
                    `shouldReturn` [ROutput "sh -c \"sleep 30\" \"it's\" \"a b\""]

    describe "display-message" $ do
        it "with no target, expands in the most recently active session" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "A", "sleep 30"] `shouldReturn` []
                run st ["new-session", "-d", "-s", "B", "sleep 30"] `shouldReturn` []
                run st ["display-message", "-p", "#{session_name}"] `shouldReturn` [ROutput "B"]
                atomically $ do
                    ss <- Map.elems <$> readTVar st.sessions
                    named <- filterM (\s -> (== "A") <$> readTVar s.name) ss
                    forM_ named $ \s -> tickActivity st >>= writeTVar s.activity
                run st ["display-message", "-p", "#{session_name}"] `shouldReturn` [ROutput "A"]
        it "tells its after hook its -c and -d values apart from its message" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "A", "sleep 30"] `shouldReturn` []
                run st ["set-hook", "-g", "after-display-message", "set -gF @got '#{hook_flag_d}/#{hook_argument_0}'"]
                    `shouldReturn` []
                _ <- run st ["display-message", "-d", "5", "-p", "msg"]
                untilM' <- timeout 5_000_000 $ untilM $ do
                    o <- readTVarIO st.options
                    pure (Map.lookup "@got" o.user == Just "5/msg")
                untilM' `shouldBe` Just ()
        it "refuses a -c naming no client, and flags it does not implement" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "A", "sleep 30"] `shouldReturn` []
                run st ["display-message", "-c", "nope", "-p", "x"]
                    `shouldReturn` [RErr "can't find client: nope"]
                run st ["display-message", "-v", "-p", "x"]
                    `shouldReturn` [RErr "display-message -v: not supported"]
                run st ["display-message", "-l", "-p", "#{session_name}"]
                    `shouldReturn` [ROutput "#{session_name}"]

    describe "run-shell" $
        it "-t shows the output in the pane's view mode, -b too once it ends" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["run-shell", "-t", "S:0.0", "echo out"] `shouldReturn` []
                run st ["display-message", "-p", "-t", "S:0.0", "#{pane_mode}"]
                    `shouldReturn` [ROutput "view-mode"]
                run st ["run-shell", "-b", "-t", "S:0.0", "echo later"] `shouldReturn` []
                [p] <- allPanes st
                shown <- timeout 5_000_000 $ untilM $ do
                    mpm <- readTVarIO p.mode
                    pure $ case mpm of
                        Just pm | pm.frozen.fgSy > 1 ->
                            T.isPrefixOf "later" (T.pack (map Cell.baseChar (V.toList (pm.frozen.fgRows V.! 1))))
                        _ -> False
                shown `shouldBe` Just ()

    describe "new-window" $
        it "expands -n as a format in the target session" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["new-window", "-d", "-t", "S:9", "-n", "#{session_name}:.##(ok)", "sleep 30"]
                    `shouldReturn` []
                run st ["display-message", "-p", "-t", "S:9", "#{window_name}"]
                    `shouldReturn` [ROutput "S:.#(ok)"]

    describe "pipe-pane" $
        it "pipes the pane a full -t target names, not the current one" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["new-window", "-d", "-t", "S:9", "cat"] `shouldReturn` []
                run st ["pipe-pane", "-I", "-t", "S:9.0", "echo pasted-here"]
                    `shouldReturn` []
                let shown = do
                        out <- run st ["capture-pane", "-p", "-t", "S:9.0"]
                        pure (any (\r -> case r of ROutput t -> "pasted-here" `T.isInfixOf` t; _ -> False) out)
                landed <- timeout 5_000_000 (untilM shown)
                landed `shouldBe` Just ()

    describe "floating panes" $ do
        let fmt st tgt f = do
                out <- run st ["display-message", "-p", "-t", tgt, f]
                pure [ t | ROutput t <- out ]
            box = "#{e|+:#{e|-:#{pane_right},#{pane_left}},1}x\
                \#{e|+:#{e|-:#{pane_bottom},#{pane_top}},1}@#{pane_left},#{pane_top}"
            newFloat st extra = do
                out <- run st (["new-pane", "-dPF", "#{pane_id}"] <> extra <> ["sleep 30"])
                case out of
                    [ROutput t] -> pure t
                    other -> fail (show other)
            session st = run st ["new-session", "-d", "-s", "S", "-x", "80", "-y", "24", "sleep 30"]
                `shouldReturn` []
        it "new-pane places the box -x/-y/-X/-Y name, its border included" $
            withServer $ \st -> do
                session st
                f <- newFloat st ["-x", "20", "-y", "6", "-X", "8", "-Y", "3"]
                fmt st f (box <> " #{pane_floating_flag}") `shouldReturn` ["18x4@9,4 1"]
                run st ["new-pane", "-d", "-x", "0", "sleep 30"]
                    `shouldReturn` [RErr "invalid width"]
        it "without a position, cascades from the last floating pane" $
            withServer $ \st -> do
                session st
                a <- newFloat st []
                b <- newFloat st []
                fmt st a box `shouldReturn` ["40x6@4,2"]
                fmt st b box `shouldReturn` ["40x6@8,4"]
        it "stacks new panes in front; move-pane -P and -z restack them" $
            withServer $ \st -> do
                session st
                a <- newFloat st ["-x", "10", "-y", "4"]
                b <- newFloat st ["-x", "10", "-y", "4"]
                let order = fmt st "S:0" "#{P/z:#{pane_id}}"
                order `shouldReturn` [b <> a <> "%0"]
                run st ["move-pane", "-t", a, "-P", "front"] `shouldReturn` []
                order `shouldReturn` [a <> b <> "%0"]
                -- window_layout lists the floating panes front to back.
                fmap (map (("," <> T.drop 1 b <> ">") `T.isSuffixOf`))
                    (fmt st "S:0" "#{window_layout}") `shouldReturn` [True]
                run st ["move-pane", "-t", a, "-z", "5"] `shouldReturn` []
                order `shouldReturn` [b <> a <> "%0"]
                run st ["move-pane", "-t", "%0", "-P", "front"]
                    `shouldReturn` [RErr "pane is not floating"]
        it "move-pane -X/-Y and resize-pane -x/-y count the border" $
            withServer $ \st -> do
                session st
                f <- newFloat st ["-x", "20", "-y", "6", "-X", "8", "-Y", "3"]
                run st ["move-pane", "-t", f, "-X", "5", "-Y", "25%"] `shouldReturn` []
                run st ["resize-pane", "-t", f, "-x", "30", "-y", "10"] `shouldReturn` []
                fmt st f box `shouldReturn` ["28x8@6,7"]
                run st ["move-pane", "-t", f, "-P", "bottom-right"] `shouldReturn` []
                fmt st f box `shouldReturn` ["28x8@51,15"]
        it "break-pane -W floats a tiled pane; joining it to itself tiles it back" $
            withServer $ \st -> do
                session st
                run st ["split-window", "-d", "-h", "-t", "S:0", "sleep 30"] `shouldReturn` []
                [p0, p1] <- paneIds st "S:0"
                run st ["break-pane", "-W", "-s", p1, "-x", "12", "-y", "5", "-X", "1", "-Y", "1"]
                    `shouldReturn` []
                fmt st p1 (box <> " #{pane_floating_flag}") `shouldReturn` ["10x3@2,2 1"]
                fmt st p0 box `shouldReturn` ["80x24@0,0"]
                run st ["break-pane", "-W", "-s", p1] `shouldReturn` [RErr "pane is already floating"]
                run st ["join-pane", "-s", p1, "-t", p1] `shouldReturn` []
                -- The pane tiled back takes the larger half (layout_insert_tile).
                geometry st "S:0" `shouldReturn` ["0,0 39x24", "40,0 40x24"]
        it "a modal pane holds the window's focus until it closes" $
            withServer $ \st -> do
                session st
                run st ["split-window", "-d", "-t", "%0", "sleep 30"] `shouldReturn` []
                run st ["resize-pane", "-Z", "-t", "%0"] `shouldReturn` []
                [out] <- run st ["new-pane", "-OPF", "#{pane_id}", "-x", "20", "-y", "5", "sleep 30"]
                m <- case out of
                    ROutput t -> pure t
                    other -> fail (show other)
                fmt st "S:0" "#{pane_id} #{window_modal_pane} #{window_flags} #{window_zoomed_flag}"
                    `shouldReturn` [m <> " " <> m <> " *O 0"]
                run st ["new-pane", "-O", "sleep 30"]
                    `shouldReturn` [RErr "window already has a modal pane"]
                run st ["new-pane", "-O", "-L", "sleep 30"]
                    `shouldReturn` [RErr "modal pane must be floating"]
                run st ["break-pane", "-s", m] `shouldReturn` [RErr "pane is modal"]
                run st ["select-pane", "-t", "%0"] `shouldReturn` []
                fmt st "S:0" "#{pane_id}" `shouldReturn` [m]
                run st ["kill-pane", "-t", m] `shouldReturn` []
                fmt st "S:0" "#{pane_id} #{window_modal_pane} #{window_zoomed_flag}"
                    `shouldReturn` ["%0  1"]

        it "select-pane -m on a floating pane brings it to the front, active" $
            withServer $ \st -> do
                session st
                a <- newFloat st ["-x", "10", "-y", "4"]
                b <- newFloat st ["-x", "10", "-y", "4"]
                run st ["select-pane", "-t", "%0"] `shouldReturn` []
                run st ["select-pane", "-m", "-t", a] `shouldReturn` []
                fmt st "S:0" "#{pane_id} #{P/z:#{pane_id}}" `shouldReturn` [a <> " " <> a <> b <> "%0"]
                fmt st a "#{pane_marked}" `shouldReturn` ["1"]
        it "select-pane -m leaves a pane a zoom hides unmarked" $
            withServer $ \st -> do
                session st
                run st ["split-window", "-d", "-t", "%0", "sleep 30"] `shouldReturn` []
                [_, p1] <- paneIds st "S:0"
                run st ["resize-pane", "-Z", "-t", "%0"] `shouldReturn` []
                run st ["select-pane", "-m", "-t", p1] `shouldReturn` []
                fmt st p1 "#{pane_marked}" `shouldReturn` ["0"]
        it "floating panes, their stacking and the modal pane survive a restore" $
            withServer $ \st -> do
                session st
                a <- newFloat st ["-x", "10", "-y", "4", "-X", "-2", "-Y", "3"]
                _ <- newFloat st ["-x", "12", "-y", "5"]
                run st ["move-pane", "-t", a, "-P", "front"] `shouldReturn` []
                run st ["new-pane", "-O", "-x", "20", "-y", "5", "sleep 30"] `shouldReturn` []
                let probe s = fmap concat . mapM (\f -> fmap (map (<> ";"))
                        (fmap (\o -> [ t | ROutput t <- o ])
                            (run s ["list-panes", "-t", "S:0", "-F", f])))
                    shape = [ "#{pane_floating_flag}#{pane_modal_flag} " <> box <> " #{pane_z}" ]
                captured <- probe st shape
                snap <- captureSnapshot st
                withServer $ \st' -> do
                    restoreSnapshot st' (fromMaybe snap (decodeSnapshotJson (encodeSnapshotJson snap)))
                    probe st' shape `shouldReturn` captured
                    fmt st' "S:0" "#{pane_modal_flag}" `shouldReturn` ["1"]

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
        it "-k keeps the pane once its program exits, -W waits for that" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                [out] <- run st ["split-window", "-d", "-k", "-W", "-P", "-F",
                    "#{pane_id}", "-t", "S:0", "true"]
                pid <- case out of
                    ROutput t -> pure t
                    other -> fail (show other)
                run st ["display-message", "-p", "-t", pid, "#{pane_dead}"]
                    `shouldReturn` [ROutput "1"]
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
        it "-d and -e turn a pane's input off and on, -T replaces the host-name title" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                let fmt f = run st ["display-message", "-p", "-t", "S:0.0", f]
                host <- T.pack . nodeName <$> getSystemID
                fmt "#{pane_title}" `shouldReturn` [ROutput host]
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
