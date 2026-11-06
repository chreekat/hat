-- | The pane commands driven through the real command table against an
-- in-process server, with real ptys behind every pane.
module Hat.Server.PaneCommandSpec (spec) where

import Control.Concurrent.STM
import Control.Exception (IOException, bracket, catch)
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM_, unless)
import Data.ByteString.Char8 qualified as B8
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Temp (mkdtemp)
import System.Posix.Unistd (getSystemID, nodeName)
import System.Timeout (timeout)
import Test.Hspec hiding (Arg)

import Hat.Command.Arg (Arg (..))

import Hat.Geometry (Size (..))
import Hat.Model
import Hat.Model.Options (Options (..))
import Hat.Server (Reply (..), dispatch, reconcileThrough)
import Hat.Server.Command.Types (errQuoting)
import Hat.Server.Conn (handleInput)
import Hat.Server.Dispatch (runArgArgv, runArgv)
import Hat.Server.Layout (fitLayout, singlePane)
import Hat.Server.LayoutString (emitLayout)
import Hat.Server.Persist
    (PaneSnap (..), SessionSnap (..), Snapshot (..), WindowSnap (..)
    , decodeSnapshotJson, encodeSnapshotJson, saveSnapshot, withStore)
import Hat.Server.Snapshot (captureSnapshot, restoreSaved, restoreSnapshot)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu
import Hat.Term.Pty qualified as Pty
import Hat.TestSupport (addClient, realPanes, withServer, withServerStore)
import Data.Vector qualified as V

-- | A private directory for one test's files, removed afterwards.
withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir = bracket (mkdtemp "/tmp/hat-panecommandspec-") removeDirectoryRecursive

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

-- | The named session's window at an index.
windowAt :: ServerState -> Text -> Int -> IO (Maybe Window)
windowAt st name ix = atomically $ do
    sessions <- Map.elems <$> readTVar st.sessions
    named <- filterM (\s -> (== name) <$> readTVar s.name) sessions
    wins <- concat <$> mapM (fmap Map.toList . readTVar . (.windows)) named
    pure (lookup ix wins)

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

-- | The output lines among a command's replies.
outputs :: [Reply] -> [Text]
outputs out = [ t | ROutput t <- out ]

-- | The pane a @%N@ id names.
paneNamed :: ServerState -> Text -> IO Pane
paneNamed st pid = do
    ps <- realPanes st
    case [ p | p <- ps, "%" <> T.pack (show (rawPane p.id)) == pid ] of
        p : _ -> pure p
        [] -> fail ("no pane " <> T.unpack pid)

-- | Whether a pane's screen holds the text.
screenShows :: ServerState -> Text -> Text -> IO Bool
screenShows st tgt needle =
    any (T.isInfixOf needle) . outputs <$> run st ["capture-pane", "-p", "-t", tgt]

-- | The live sessions' names, by id.
sessionNames :: ServerState -> IO [Text]
sessionNames st = mapM (readTVarIO . (.name)) . Map.elems =<< readTVarIO st.sessions

-- | Poll a file (which may not exist yet) until its contents satisfy @ok@.
awaitFile :: FilePath -> (B8.ByteString -> Bool) -> IO B8.ByteString
awaitFile path ok = do
    r <- timeout 5_000_000 (untilM (ok <$> contents))
    unless (isJust r) (expectationFailure ("timed out waiting on " <> path))
    contents
  where
    contents = B8.readFile path `catch` \(_ :: IOException) -> pure ""

-- | Whether a process still exists, zombie included: this process's child
-- reads as gone only once it has been reaped.
pidAlive :: Int -> IO Bool
pidAlive pid = (True <$ signalProcess nullSignal (fromIntegral pid))
    `catch` \(_ :: IOException) -> pure False

-- | A saved tree of one session holding one window of one empty pane, its
-- layout fitted to a 30x100 area.
seedSnapshot :: Text -> Snapshot
seedSnapshot nm = Snapshot
    { lastActiveSession = Nothing
    , sessions =
        [ SessionSnap
            { name = nm, startCwd = "/tmp", currentIx = 0, windowHist = []
            , createdAt = Nothing, restores = 0, group = Nothing
            , windows =
                [ WindowSnap
                    { ix = 0, name = "one", active = 0, paneHist = []
                    , layout = emitLayout (fitLayout Size { rows = 30, cols = 100 } (singlePane (PaneId 0)))
                    , autoRename = False, key = Nothing, linkOf = Nothing
                    , modal = Nothing, size = Nothing
                    , panes =
                        [ PaneSnap
                            { cwd = "/tmp", command = Nothing, shellSpawned = False
                            , kind = EmptyPane, inputOff = False, title = Nothing
                            , titleStack = [] } ] } ] } ] }

spec :: Spec
spec = do
    describe "set-option -p" $
        it "sets the option on the pane a full -t target names" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["split-window", "-t", "S:0", "sleep 30"] `shouldReturn` []
                run st ["set", "-p", "-t", "S:0.0", "@where", "first"] `shouldReturn` []
                run st ["show", "-pv", "-t", "S:0.0", "@where"] `shouldReturn` [ROutput "first"]
                run st ["show", "-pqv", "-t", "S:0.1", "@where"] `shouldReturn` []

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

    describe "kill-session" $
        it "SIGKILLs a SIGHUP-ignoring pane linked twice into the session" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "a", "sleep 30"] `shouldReturn` []
                run st ["new-window", "-d", "-t", "a:1", "trap '' HUP; exec sleep 300"]
                    `shouldReturn` []
                run st ["link-window", "-d", "-s", "a:1", "-t", "a:5"] `shouldReturn` []
                [s] <- paneIds st "a:1"
                stubborn <- paneNamed st s
                run st ["kill-session", "-t", "a"] `shouldReturn` []
                sessionNames st `shouldReturn` []
                reaped <- timeout 5_000_000 (Pty.waitExit stubborn.pty)
                reaped `shouldBe` Just (Just (Pty.Terminated 9 False))

    describe "server formats" $ do
        it "#{version} is hat's package version" $
            withServer $ \st ->
                run st ["display-message", "-p", "#{version}"]
                    `shouldReturn` [ROutput "0.0.1"]
        it "#{pid} is the server's process id" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                pid <- getProcessID
                run st ["display-message", "-p", "#{pid}"]
                    `shouldReturn` [ROutput (T.pack (show pid))]

    describe "new-session" $ do
        it "execs a command of several words as its argv, which the start formats quote" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "Q", "--", "sh", "-c", "sleep 30", "it's", "a b"]
                    `shouldReturn` []
                run st ["display-message", "-p", "-t", "Q:0.0", "#{pane_start_command_list}"]
                    `shouldReturn` [ROutput "'sh' '-c' 'sleep 30' 'it'\\''s' 'a b'"]
                run st ["display-message", "-p", "-t", "Q:0.0", "#{pane_start_command}"]
                    `shouldReturn` [ROutput "sh -c \"sleep 30\" \"it's\" \"a b\""]
        it "-A without a target attaches a new client where a bare attach would" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "first", "sleep 30"] `shouldReturn` []
                run st ["new-session", "-d", "-s", "second", "sleep 30"] `shouldReturn` []
                first : _ <- Map.elems <$> readTVarIO st.sessions
                client <- addClient st (SessionId 99) Size { rows = 24, cols = 80 } 1
                runArgv st (Just client) ["new-session", "-A"] `shouldReturn` []
                readTVarIO client.session `shouldReturn` first.id

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
                run st ["run-shell", "-t", "S:0.0", "echo out"] `shouldReturn` [RExitStatus 0]
                run st ["display-message", "-p", "-t", "S:0.0", "#{pane_mode}"]
                    `shouldReturn` [ROutput "view-mode"]
                run st ["run-shell", "-b", "-t", "S:0.0", "echo later"] `shouldReturn` []
                [p] <- realPanes st
                shown <- timeout 5_000_000 $ untilM $ do
                    mpm <- atomically (topMode p)
                    pure $ case mpm of
                        Just (CopyMode pm) | pm.frozen.fgSy > 1 ->
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

    describe "pipe-pane" $ do
        it "pipes the pane a full -t target names, not the current one" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["new-window", "-d", "-t", "S:9", "cat"] `shouldReturn` []
                run st ["pipe-pane", "-I", "-t", "S:9.0", "echo pasted-here"]
                    `shouldReturn` []
                landed <- timeout 5_000_000 (untilM (screenShows st "S:9.0" "pasted-here"))
                landed `shouldBe` Just ()
        it "tees the pane's output to its command until stopped, which reaps the command" $
            withServer $ \st -> withTmpDir $ \dir -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                [p] <- paneIds st "S:0"
                let logPath = dir </> "pane.log"
                    pidPath = dir </> "pipe.pid"
                run st ["pipe-pane", "-t", p, T.pack ("echo $$ > " <> pidPath <> "; cat >> " <> logPath)]
                    `shouldReturn` []
                run st ["send-keys", "-t", p, "PIPEPANEMARKER", "Enter"] `shouldReturn` []
                _ <- awaitFile logPath (B8.isInfixOf "PIPEPANEMARKER")
                Just (pid, _) <- B8.readInt <$> awaitFile pidPath (not . B8.null)
                run st ["pipe-pane", "-t", p] `shouldReturn` []
                reaped <- timeout 5_000_000 (untilM (not <$> pidAlive pid))
                reaped `shouldBe` Just ()
                -- The tap feeds the pipe before the emulator, so once the line
                -- is on screen a still-live pipe would already have logged it.
                run st ["send-keys", "-t", p, "AFTERSTOPMARKER", "Enter"] `shouldReturn` []
                echoed <- timeout 5_000_000 (untilM (screenShows st p "AFTERSTOPMARKER"))
                echoed `shouldBe` Just ()
                B8.readFile logPath >>= (`shouldNotSatisfy` B8.isInfixOf "AFTERSTOPMARKER")
        it "reaps the pipe's command when the pane is killed" $
            withServer $ \st -> withTmpDir $ \dir -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                [p] <- paneIds st "S:0"
                let pidPath = dir </> "pipe.pid"
                run st ["pipe-pane", "-t", p, T.pack ("echo $$ > " <> pidPath <> "; cat > /dev/null")]
                    `shouldReturn` []
                Just (pid, _) <- B8.readInt <$> awaitFile pidPath (not . B8.null)
                pidAlive pid `shouldReturn` True
                run st ["kill-pane", "-t", p] `shouldReturn` []
                reaped <- timeout 5_000_000 (untilM (not <$> pidAlive pid))
                reaped `shouldBe` Just ()

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

    describe "the title stack" $ do
        let stacked = "printf '\\033]2;base\\007\\033[22;0t\\033]2;temp\\007\\033[23;0t'; sleep 30"
        it "CSI 22/23 t push and pop the pane title, each pop a pane-title-changed" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["set", "-g", "@t", "0"] `shouldReturn` []
                run st ["set-hook", "-g", "pane-title-changed", "set -gF @t '#{@t}|#{hook_new_title}'"]
                    `shouldReturn` []
                run st ["split-window", "-d", "-t", "S:0", stacked] `shouldReturn` []
                got <- timeout 5_000_000 $ untilM $
                    (== [ROutput "0|base|temp|base"]) <$> run st ["show", "-gv", "@t"]
                got `shouldBe` Just ()
                run st ["display-message", "-p", "-t", "S:0.1", "#{pane_title}"]
                    `shouldReturn` [ROutput "base"]
        it "the stack survives a snapshot round trip" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S",
                        "printf '\\033]2;under\\007\\033[22;2t\\033]2;over\\007'; sleep 30"]
                    `shouldReturn` []
                let title s = run s ["display-message", "-p", "-t", "S:0.0", "#{pane_title}"]
                got <- timeout 5_000_000 $ untilM ((== [ROutput "over"]) <$> title st)
                got `shouldBe` Just ()
                snap <- captureSnapshot st
                withServer $ \st' -> do
                    restoreSnapshot st' (fromMaybe snap (decodeSnapshotJson (encodeSnapshotJson snap)))
                    [p] <- paneIds st' "S:0"
                    pane <- atomically $ do
                        sessions <- Map.elems <$> readTVar st'.sessions
                        wins <- concat <$> mapM (fmap Map.elems . readTVar . (.windows)) sessions
                        ps <- concat <$> mapM (fmap Map.elems . readTVar . (.panes)) wins
                        pure [ x | x <- ps, "%" <> T.pack (show (rawPane x.id)) == p ]
                    stacks <- mapM (Emu.titleStack . (.emulator)) pane
                    stacks `shouldBe` [["under"]]

    describe "names" $ do
        it "refuses a name holding a control character, in tmux's words" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["split-window", "-d", "-t", "S:0", "sleep 30"] `shouldReturn` []
                [_, b] <- paneIds st "S:0"
                forM_
                    [ (["rename-session", "-t", "S", "a\tb"], "session")
                    , (["new-session", "-d", "-s", "a\tb"], "session")
                    , (["rename-window", "-t", "S:0", "a\tb"], "window")
                    , (["new-session", "-d", "-n", "a\tb"], "window")
                    , (["new-window", "-d", "-t", "S:", "-n", "a\tb"], "window")
                    , (["break-pane", "-d", "-s", b, "-n", "a\tb"], "window")
                    , (["new-session", "-d", "-t", "a\tb"], "session group")
                    , (["set-buffer", "-b", "a\tb", "data"], "buffer") ]
                    $ \(argv, kind) -> run st argv
                        `shouldReturn` [RErr ("invalid " <> kind <> " name: a\tb")]
                run st ["select-pane", "-t", b, "-T", "a\tb"]
                    `shouldReturn` [RErr "invalid pane title: a\tb"]
                run st ["list-windows", "-a", "-F", "#{session_name}:#{window_name}"]
                    `shouldReturn` [ROutput "S:sleep"]

        it "refuses a name that is not UTF-8, quoting its bytes" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["split-window", "-d", "-t", "S:0", "sleep 30"] `shouldReturn` []
                [_, b] <- paneIds st "S:0"
                let bad = RawArg "a\xff\&b"
                forM_
                    [ (["rename-session", "-t", "S"], bad, "session")
                    , (["new-session", "-d", "-s"], bad, "session")
                    , (["rename-window", "-t", "S:0"], bad, "window")
                    , (["new-session", "-d", "-n"], bad, "window")
                    , (["new-window", "-d", "-t", "S:", "-n"], bad, "window")
                    , (["break-pane", "-d", "-s", b, "-n"], bad, "window")
                    , (["new-session", "-d", "-t"], bad, "session group")
                    , (["set-buffer", "-b"], bad, "buffer") ]
                    $ \(argv, name, kind) ->
                        runArgArgv st Nothing (map Utf8Arg argv <> [name, Utf8Arg "x"])
                            `shouldReturn` [errQuoting ("invalid " <> kind <> " name: ") bad]
                runArgArgv st Nothing [Utf8Arg "select-pane", Utf8Arg "-t", Utf8Arg b, Utf8Arg "-T", bad]
                    `shouldReturn` [errQuoting "invalid pane title: " bad]
                run st ["list-windows", "-a", "-F", "#{session_name}:#{window_name}"]
                    `shouldReturn` [ROutput "S:sleep"]

        it "fails any other command a word that is not UTF-8 reaches, quoting it" $
            withServer $ \st ->
                runArgArgv st Nothing [Utf8Arg "display", Utf8Arg "-p", RawArg "\xff"]
                    `shouldReturn`
                        [errQuoting "display-message: argument is not valid UTF-8: " (RawArg "\xff")]

    describe "join-pane" $ do
        it "refuses to join a pane to itself" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["join-pane", "-s", "S:0.0", "-t", "S:0.0"]
                    `shouldReturn` [RErr "source and target panes must be different"]
        -- 0ebf668: the reader routes each chunk by where the pane lives now.
        it "routes the joined pane's later output to its new window" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["new-window", "-d", "-t", "S:1", "cat"] `shouldReturn` []
                run st ["join-pane", "-h", "-s", "S:1.0", "-t", "S:0"] `shouldReturn` []
                Just w0 <- windowAt st "S" 0
                joined <- atomically $
                    (Map.!) <$> readTVar w0.panes <*> readTVar w0.activeId
                stamp <- readTVarIO w0.activityAt
                Pty.writePty joined.pty "joined\n"  -- echoed back as pane output
                routed <- timeout 5_000_000 $
                    untilM ((> stamp) <$> readTVarIO w0.activityAt)
                routed `shouldBe` Just ()

    describe "break-pane" $ do
        it "moves the pane to a new window, printing where it went" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["split-window", "-d", "-t", "S:0", "sleep 30"] `shouldReturn` []
                [_, b] <- paneIds st "S:0"
                run st ["break-pane", "-d", "-P", "-F", "#{window_index}:#{pane_id}",
                        "-n", "out", "-s", b, "-t", "S:"]
                    `shouldReturn` [ROutput ("1:" <> b)]
                windowIndices st "S" `shouldReturn` [0, 1]
        it "numbers the new window from base-index (a4)" $
            withServer $ \st -> do
                run st ["set", "-g", "base-index", "1"] `shouldReturn` []
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["split-window", "-d", "-t", "S:1", "sleep 30"] `shouldReturn` []
                [_, b] <- paneIds st "S:1"
                run st ["break-pane", "-d", "-s", b, "-t", "S:"] `shouldReturn` []
                windowIndices st "S" `shouldReturn` [1, 2]
        it "a broken-out pane whose program then exits closes its new window, not its old one" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["split-window", "-d", "-t", "S:0", "read line"] `shouldReturn` []
                [a, b] <- paneIds st "S:0"
                run st ["break-pane", "-d", "-s", b, "-t", "S:"] `shouldReturn` []
                windowIndices st "S" `shouldReturn` [0, 1]
                run st ["send-keys", "-t", b, "Enter"] `shouldReturn` []
                closed <- timeout 5_000_000 (untilM ((== [0]) <$> windowIndices st "S"))
                closed `shouldBe` Just ()
                paneIds st "S:0" `shouldReturn` [a]

    describe "a zoom" $
        it "shrinks the formerly-zoomed pane's pty back on select-pane away" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "-x", "80", "-y", "24", "sleep 30"]
                    `shouldReturn` []
                run st ["split-window", "-h", "-t", "S:0", "sleep 30"] `shouldReturn` []
                [_, b] <- paneIds st "S:0"
                right <- paneNamed st b
                let ptySize = do
                        _ <- reconcileThrough st =<< readTVarIO st.dirty
                        Pty.getWinsize (Pty.masterFd right.pty)
                narrow <- ptySize
                narrow.cols `shouldSatisfy` (< 80)
                run st ["resize-pane", "-Z", "-t", b] `shouldReturn` []
                ptySize `shouldReturn` Size { rows = 24, cols = 80 }
                run st ["select-pane", "-L"] `shouldReturn` []
                run st ["select-pane", "-R"] `shouldReturn` []
                ptySize `shouldReturn` narrow

    describe "remain-on-exit" $ do
        it "on keeps a dead pane, telling how its program ended" $
            withServer $ \st -> do
                run st ["set", "-g", "remain-on-exit", "on"] `shouldReturn` []
                run st ["new-session", "-d", "-s", "S", "-x", "60", "-y", "5", "echo DEADBEACON; exit 3"]
                    `shouldReturn` []
                died <- timeout 5_000_000 $ untilM $ (== [ROutput "1 3"])
                    <$> run st ["display", "-p", "-t", "S:0.0", "#{pane_dead} #{pane_dead_status}"]
                died `shouldBe` Just ()
                screen <- outputs <$> run st ["capture-pane", "-p", "-t", "S:0.0", "-S", "-"]
                screen `shouldSatisfy` any (T.isInfixOf "DEADBEACON")
                screen `shouldSatisfy` any (T.isInfixOf "Pane is dead (status 3, ")
        it "failed keeps only a pane whose program failed" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["set", "-g", "remain-on-exit", "failed"] `shouldReturn` []
                run st ["new-window", "-d", "-t", "S:", "true"] `shouldReturn` []
                run st ["new-window", "-d", "-t", "S:", "exit 1"] `shouldReturn` []
                settled <- timeout 5_000_000 $ untilM $ (== ["0 ", "1 1"]) . List.sort . outputs
                    <$> run st ["list-panes", "-a", "-F", "#{pane_dead} #{pane_dead_status}"]
                settled `shouldBe` Just ()
        it "key closes a dead pane on the next key typed into it" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["set", "-g", "remain-on-exit", "key"] `shouldReturn` []
                run st ["split-window", "-t", "S:0", "true"] `shouldReturn` []
                [a, b] <- paneIds st "S:0"
                died <- timeout 5_000_000 $ untilM $ (== [ROutput "1"])
                    <$> run st ["display", "-p", "-t", b, "#{pane_dead}"]
                died `shouldBe` Just ()
                [sess] <- Map.elems <$> readTVarIO st.sessions
                client <- addClient st sess.id Size { rows = 24, cols = 80 } 1
                handleInput dispatch st client "x"
                paneIds st "S:0" `shouldReturn` [a]

    describe "respawn-pane" $ do
        it "reruns a dead pane's command" $
            withServer $ \st -> withTmpDir $ \dir -> do
                let runs = dir </> "runs"
                    dead = (== [ROutput "1 3"]) <$> run st
                        ["display", "-p", "-t", "S:0.0", "#{pane_dead} #{pane_dead_status}"]
                run st ["set", "-g", "remain-on-exit", "on"] `shouldReturn` []
                run st ["new-session", "-d", "-s", "S", T.pack ("echo run >> " <> runs <> "; exit 3")]
                    `shouldReturn` []
                died <- timeout 5_000_000 (untilM dead)
                died `shouldBe` Just ()
                run st ["respawn-pane", "-t", "S:0.0"] `shouldReturn` []
                _ <- awaitFile runs ((== 2) . length . B8.lines)
                rerun <- timeout 5_000_000 (untilM dead)
                rerun `shouldBe` Just ()
        it "needs -k to replace a live pane, as respawn-window does a live window" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                [p] <- paneIds st "S:0"
                live <- paneNamed st p
                run st ["respawn-pane", "-t", p, "true"]
                    `shouldReturn` [RErr "respawn pane failed: pane S:0.0 still active"]
                run st ["respawn-pane", "-k", "-t", p, "sleep 40"] `shouldReturn` []
                replaced <- timeout 5_000_000 (Pty.waitExit live.pty)
                replaced `shouldSatisfy` isJust
                run st ["display", "-p", "-t", p, "#{pane_dead}"] `shouldReturn` [ROutput "0"]
                run st ["split-window", "-d", "-t", "S:0", "sleep 30"] `shouldReturn` []
                others <- mapM (paneNamed st) =<< paneIds st "S:0"
                run st ["respawn-window", "-t", "S:0", "true"]
                    `shouldReturn` [RErr "respawn window failed: window S:0 still active"]
                run st ["respawn-window", "-k", "-t", "S:0", "sleep 40"] `shouldReturn` []
                run st ["display", "-p", "-t", "S:0", "#{window_panes}"] `shouldReturn` [ROutput "1"]
                forM_ others $ \o -> timeout 5_000_000 (Pty.waitExit o.pty) >>= (`shouldSatisfy` isJust)

    describe "the editor option" $
        it "runs its program on a buffer chosen in choose-buffer, keeping what it saves" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "sleep 30"] `shouldReturn` []
                run st ["set", "-g", "editor", "sh -c 'printf edited > \"$1\"' edit"]
                    `shouldReturn` []
                run st ["set-buffer", "-b", "b1", "old"] `shouldReturn` []
                [sess] <- Map.elems <$> readTVarIO st.sessions
                client <- addClient st sess.id Size { rows = 24, cols = 80 } 1
                runArgv st (Just client) ["choose-buffer"] `shouldReturn` []
                handleInput dispatch st client "e"
                edited <- timeout 5_000_000 $ untilM $
                    (== [ROutput "edited"]) <$> run st ["show-buffer", "-b", "b1"]
                edited `shouldBe` Just ()

    describe "a saved tree" $ do
        it "is restored at its captured area, not the default size (4b)" $
            withServer $ \st -> do
                restoreSnapshot st (seedSnapshot "stale")
                run st ["list-panes", "-a", "-F", "#{pane_width}x#{pane_height}"]
                    `shouldReturn` [ROutput "100x30"]
        it "is archived as a generation list-snapshots lists when a restart restores it (bb)" $
            withTmpDir $ \dir -> withServerStore (Just (dir </> "store.db")) $ \st -> do
                withStore (dir </> "store.db") (\c -> saveSnapshot c (seedSnapshot "stale"))
                restoreSaved st (dir </> "store.db")
                sessionNames st `shouldReturn` ["stale"]
                listed <- outputs <$> run st ["list-snapshots"]
                listed `shouldSatisfy` \case
                    [l] -> "1: " `T.isPrefixOf` l && "(1 sessions, 1 windows)" `T.isSuffixOf` l
                    _ -> False
        it "comes back from restore-snapshot beside the live tree, under a free name (bb)" $
            withTmpDir $ \dir -> withServerStore (Just (dir </> "store.db")) $ \st -> do
                withStore (dir </> "store.db") (\c -> saveSnapshot c (seedSnapshot "stale"))
                restoreSaved st (dir </> "store.db")
                run st ["restore-snapshot", "1"]
                    `shouldReturn` [ROutput "restored session 'stale' as 'stale-2'"]
                sessionNames st `shouldReturn` ["stale", "stale-2"]
