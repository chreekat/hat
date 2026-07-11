-- | End-to-end tests driving the real @hat@ binary through a pty.
module Hat.IntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, finally)
import Control.Monad (unless, when)
import qualified Data.List as List
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import System.Directory (removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Posix.Temp (mkdtemp)
import System.Process (readProcess)
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec

import qualified Data.Text as T
import qualified Data.Vector as V

import Hat.Geometry
import Hat.Pty
import Hat.Socket (connectTo)
import Hat.Term.Cell (Cell (..), Style (..))
import qualified Hat.Term.Emulator as Emu

-- An isolated hat instance for one test: a private HOME (so @hat@ never
-- reads any ambient config) and a private socket. See 'withHat'.
data Hat = Hat
    { bin  :: FilePath
    , home :: FilePath
    , sock :: FilePath
    }

-- Minimal PATH for spawned shells and the hat binary. Deliberately does
-- not inherit the ambient environment.
testPath :: String
testPath = "/run/current-system/sw/bin:/usr/bin:/bin"

-- | Run a test against a freshly isolated hat, tearing the server and
-- its temp dir down afterwards no matter how the test ends. The socket
-- lives at @<tmp>/socket@.
withHat :: FilePath -> (Hat -> IO ()) -> IO ()
withHat hatBin = withHatOn hatBin "socket"

-- | 'withHat' with a custom socket path relative to the temp HOME (used
-- by the test that needs the socket's parent dirs to not exist yet).
withHatOn :: FilePath -> FilePath -> (Hat -> IO ()) -> IO ()
withHatOn hatBin sockRel action = do
    dir <- mkdtemp "/tmp/hat-test-"
    let h = Hat { bin = hatBin, home = dir, sock = dir <> "/" <> sockRel }
    action h `finally` teardown h

-- Kill the server (harmless if already gone) and remove the temp dir.
teardown :: Hat -> IO ()
teardown h = do
    _ <- hatCtl h ["kill-server"]
        `catch` \(_ :: SomeException) -> pure (ExitFailure 1, "", "")
    _ <- pollServerGone h.sock 50
    removeDirectoryRecursive h.home
        `catch` \(_ :: SomeException) -> pure ()

-- Run a hat control command with the isolated HOME so it never resolves
-- the ambient user config (even when it autostarts a server).
hatCtl :: Hat -> [String] -> IO (ExitCode, String, String)
hatCtl h args =
    P.readCreateProcessWithExitCode
        (P.proc h.bin (["-S", h.sock] <> args))
            { P.env = Just [("HOME", h.home), ("PATH", testPath)] }
        ""

-- Stdout of a hat control command.
ctlOut :: Hat -> [String] -> IO String
ctlOut h args = (\(_, out, _) -> out) <$> hatCtl h args

-- A hat client running inside a test-owned pty. The raw transcript
-- catches out-of-band messages ("[detached]"); the emulator models
-- what a human would see on the screen.
data Driver = Driver
    { pty :: PtyHandle
    , transcript :: IORef B8.ByteString
    , screen :: Emu.Emulator
    }

startClient :: Hat -> IO Driver
startClient h = startClientArgs h []

startClientArgs :: Hat -> [String] -> IO Driver
startClientArgs h extra = do
    -- Pane children need terminfo for TERM=tmux-256color on NixOS.
    terminfo <- lookupEnv "TERMINFO_DIRS"
    p <- spawn Spawn
        { cmd = h.bin
        , args = ["-S", h.sock] <> extra
        , env =
            [ ("PATH", testPath)
            , ("TERM", "xterm-256color")
            , ("SHELL", "/bin/sh")
            , ("HOME", h.home)
            , ("PS1", "$ ")
            ]
            <> maybe [] (\v -> [("TERMINFO_DIRS", v)]) terminfo
        , cwd = Just "/tmp"
        , size = Size { rows = 24, cols = 80 }
        }
    t <- newIORef ""
    emu <- Emu.newEmulator Size { rows = 24, cols = 80 } 1000
    pure Driver { pty = p, transcript = t, screen = emu }

ingest :: Driver -> B8.ByteString -> IO ()
ingest d chunk = do
    modifyIORef' d.transcript (<> chunk)
    _ <- Emu.feed d.screen chunk
    pure ()

screenText :: Driver -> IO T.Text
screenText d = do
    scr <- Emu.snapshot d.screen
    pure $ T.unlines [Emu.screenRowText scr r | r <- [0 .. 23]]

-- Read until the given check passes; fail after 10s.
awaitWith :: String -> (Driver -> IO Bool) -> Driver -> IO ()
awaitWith what check d = do
    r <- timeout 10_000_000 go
    case r of
        Just () -> pure ()
        Nothing -> do
            scr <- screenText d
            expectationFailure $
                "timed out waiting for " <> what
                <> "\nscreen:\n" <> T.unpack scr
  where
    go = do
        ok <- check d
        if ok
            then pure ()
            else do
                chunk <- readPty d.pty
                when (B8.null chunk) $
                    expectationFailure ("pty closed while waiting for " <> what)
                ingest d chunk
                go

-- What a user would see on screen.
awaitScreen :: Driver -> T.Text -> IO ()
awaitScreen d needle = awaitWith (show needle) check d
  where
    check drv = T.isInfixOf needle <$> screenText drv

awaitExit :: Driver -> IO ProcessStatus
awaitExit d = do
    r <- timeout 10_000_000 drainAndWait
    maybe (expectationFailure "timed out waiting for exit"
            >> waitExit d.pty) pure r
  where
    drainAndWait = do
        chunk <- readPty d.pty
        if B8.null chunk
            then waitExit d.pty
            else do
                ingest d chunk
                drainAndWait

typeInto :: Driver -> B8.ByteString -> IO ()
typeInto d = writePty d.pty

-- Count cells drawn with the reverse-video attribute (how a copy-mode
-- selection renders).
reverseCellCount :: Driver -> IO Int
reverseCellCount d = do
    scr <- Emu.snapshot d.screen
    pure $ length
        [ () | row <- V.toList scr.cells, cell <- V.toList row
             , let Style { reverse = r } = cell.style, r ]

spec :: Spec
spec = parallel $ do
    hatBin <- runIO (init <$> readProcess "cabal" ["list-bin", "hat"] "")

    it "attaches, survives detach/reattach, and shuts down cleanly" $
        withHat hatBin $ \h -> do
        -- First client autostarts the server and gets a shell.
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "echo hat-integration-$((6*7))\r"
        awaitScreen c1 "hat-integration-42"

        -- Detach with prefix-d.
        typeInto c1 "\x02\&d"
        status1 <- awaitExit c1
        status1 `shouldBe` Exited ExitSuccess
        t1 <- readIORef c1.transcript
        t1 `shouldSatisfy` B8.isInfixOf "[detached]"

        -- Server must still be alive.
        alive <- connectTo h.sock
        alive `shouldSatisfy` \case
            Just _ -> True
            Nothing -> False

        -- Reattach: the redrawn screen still shows our marker.
        c2 <- startClient h
        awaitScreen c2 "hat-integration-42"

        -- Ending the shell ends the session, the client, and the server.
        typeInto c2 "exit\r"
        status2 <- awaitExit c2
        status2 `shouldBe` Exited ExitSuccess
        t2 <- readIORef c2.transcript
        t2 `shouldSatisfy` B8.isInfixOf "[exited]"

        gone <- pollServerGone h.sock 50
        unless gone $ expectationFailure "server did not exit"

    it "renders vim to two simultaneous clients" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "vim -u NONE -i NONE\r"
        awaitScreen c1 "VIM - Vi IMproved"

        -- A second client on the same session sees vim too.
        c2 <- startClient h
        awaitScreen c2 "VIM - Vi IMproved"

        -- Typing in one client shows up in both.
        typeInto c1 "ihello from hat"
        awaitScreen c2 "hello from hat"
        awaitScreen c1 "hello from hat"

        -- Quit vim, then the shell; both clients exit.
        typeInto c1 "\ESC:q!\r"
        awaitScreen c1 "$"
        typeInto c1 "exit\r"
        status1 <- awaitExit c1
        status2 <- awaitExit c2
        status1 `shouldBe` Exited ExitSuccess
        status2 `shouldBe` Exited ExitSuccess
        t2 <- readIORef c2.transcript
        t2 `shouldSatisfy` B8.isInfixOf "[exited]"

    it "runs htop (colors, alt screen, live redraw)" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "htop\r"
        awaitScreen c1 "Mem"
        awaitScreen c1 "Tasks"
        typeInto c1 "q"
        awaitScreen c1 "$"
        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

    it "splits panes, navigates, zooms, and kills" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"

        -- Vertical split (side by side); border appears.
        typeInto c1 "\x02%"
        awaitScreen c1 "\x2502"  -- │
        typeInto c1 "echo right-pane-here\r"
        awaitScreen c1 "right-pane-here"

        -- Navigate left, mark that pane.
        typeInto c1 "\x02h"
        typeInto c1 "echo left-pane-here\r"
        awaitScreen c1 "left-pane-here"

        -- Zoom hides the border and the other pane.
        typeInto c1 "\x02z"
        awaitWith "zoomed (no border)" (\d -> do
            t <- screenText d
            pure (not ("\x2502" `T.isInfixOf` t)
                  && "left-pane-here" `T.isInfixOf` t)) c1
        -- Unzoom brings it back.
        typeInto c1 "\x02z"
        awaitScreen c1 "\x2502"

        -- Horizontal split below, then kill it.
        typeInto c1 "\x02\""
        awaitScreen c1 "\x2500"  -- ─
        typeInto c1 "\x02x"
        awaitWith "bottom pane gone" (\d -> do
            t <- screenText d
            pure (not ("\x2500" `T.isInfixOf` t))) c1

        -- Kill remaining panes; session ends.
        typeInto c1 "exit\r"
        awaitWith "one pane left" (\d -> do
            t <- screenText d
            pure (not ("\x2502" `T.isInfixOf` t))) c1
        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

    it "creates and switches windows with a live status line" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"       -- status line is up
        typeInto c1 "echo window-zero-marker\r"
        awaitScreen c1 "window-zero-marker"

        -- New window: status shows it as current; screen is a fresh shell.
        typeInto c1 "\x02\&c"
        awaitScreen c1 "1:sh*"
        awaitWith "window 0 hidden" (\d -> do
            t <- screenText d
            pure (not ("window-zero-marker" `T.isInfixOf` t))) c1
        typeInto c1 "echo window-one-marker\r"
        awaitScreen c1 "window-one-marker"

        -- Direct index select back to 0.
        typeInto c1 "\x02\&0"
        awaitScreen c1 "window-zero-marker"
        awaitScreen c1 "0:sh*"

        -- last-window toggles back to 1.
        typeInto c1 "\x02\&l"
        awaitScreen c1 "window-one-marker"

        -- Kill both shells; session and server end.
        typeInto c1 "exit\r"
        awaitScreen c1 "window-zero-marker"
        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

    it "marks only viewed windows: window_active_clients is per-window" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf")
            "set -g window-status-format \
            \'#I:#W#{?window_active_clients,<watched>,}'\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh"

        -- New window: window 0 is now idle, viewed by nobody.
        typeInto c1 "\x02\&c"
        awaitScreen c1 "1:sh*"

        -- With a single client, the idle window has no viewers, so its
        -- per-window count is 0 and the marker must not appear.
        scr <- screenText c1
        scr `shouldSatisfy` T.isInfixOf "0:sh"
        scr `shouldNotSatisfy` T.isInfixOf "<watched>"

    it "swallows typing while in copy mode (prefix [)" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"

        -- Control: typed text echoes at the shell.
        typeInto c1 "echo before-copy-zone\r"
        awaitScreen c1 "before-copy-zone"

        -- prefix [ enters copy mode. Keys with no copy-mode binding are
        -- swallowed, so this marker never reaches the shell to be echoed.
        typeInto c1 "\x02["
        threadDelay 200000
        typeInto c1 "zapzap42"
        threadDelay 200000

        -- q exits copy mode; the pane takes input again.
        typeInto c1 "q"
        threadDelay 200000
        typeInto c1 "echo after-copy-zone\r"
        awaitScreen c1 "after-copy-zone"

        -- after-copy-zone came after the swallowed marker in the same
        -- input stream, so its presence proves the marker was dropped.
        scr <- screenText c1
        scr `shouldNotSatisfy` T.isInfixOf "zapzap42"

    it "reverse-videos the copy-mode selection on screen" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"

        -- Leave a line of default-styled text on the input line (unentered).
        typeInto c1 "echo SELECTMEMARKER"
        awaitScreen c1 "SELECTMEMARKER"
        baseline <- reverseCellCount c1

        -- Enter copy mode (default mode-keys = emacs) and select the
        -- line: C-a start-of-line, Space begin-selection, C-e end-of-line.
        typeInto c1 "\x02["
        threadDelay 200000
        typeInto c1 "\x01"           -- C-a
        typeInto c1 " "              -- Space: begin-selection
        typeInto c1 "\x05"           -- C-e
        awaitWith "selection highlighted" (\d ->
            (> baseline) <$> reverseCellCount d) c1

        -- C-g clears the selection, so the highlight goes away.
        typeInto c1 "\x07"           -- C-g: clear-selection
        awaitWith "highlight cleared" (\d ->
            (<= baseline) <$> reverseCellCount d) c1

    it "save-buffer writes a buffer to disk, and -a appends" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        let path = h.home <> "/out.txt"
        _ <- ctlOut h ["set-buffer", "alpha"]
        _ <- ctlOut h ["save-buffer", path]
        readFile path >>= (`shouldBe` "alpha")
        -- a fresh set-buffer pushes a new top buffer; -a appends it.
        _ <- ctlOut h ["set-buffer", "beta"]
        _ <- ctlOut h ["save-buffer", "-a", path]
        readFile path >>= (`shouldBe` "alphabeta")

    it "prefix ] pastes the top buffer into the pane" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        -- Run cat so pasted input is echoed straight back.
        typeInto c1 "cat\r"
        threadDelay 300000
        _ <- ctlOut h ["set-buffer", "bracketpastemarker"]
        typeInto c1 "\x02]"          -- C-b ]  -> paste-buffer
        awaitScreen c1 "bracketpastemarker"
        -- Finish the line, EOF cat, exit the shell.
        typeInto c1 "\r"
        typeInto c1 "\x04"
        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

    it "loads a user config: C-Space prefix, vim keys, base-index 1" $
        withHat hatBin $ \h -> do
        let confPath = h.home <> "/hat.conf"
        writeFile confPath $ unlines
            [ "# hat test config"
            , "set -g prefix C-Space"
            , "unbind C-b"
            , "set -g base-index 1"
            , "set -g status-position top"
            , "bind v split-window -h -c '#{pane_current_path}'"
            , "bind s split-window -v"
            , "bind h select-pane -L"
            , "bind l select-pane -R"
            , "bind X kill-pane"
            , "bind r source-file " <> confPath <> " \\; display-message reloaded"
            , "set -g status-right 'clients=#{window_active_clients}'"
            ]
        c1 <- startClientArgs h ["-f", confPath]
        -- base-index 1 shows in the status line, at the TOP
        awaitScreen c1 "1:sh*"
        awaitWith "status on top row" (\d -> do
            scr <- Emu.snapshot d.screen
            pure ("1:sh*" `T.isInfixOf` Emu.screenRowText scr 0)) c1

        -- C-Space v splits; C-b must do nothing (unbound).
        typeInto c1 "\x00v"
        awaitScreen c1 "\x2502"
        typeInto c1 "echo cfg-right-pane\r"
        awaitScreen c1 "cfg-right-pane"
        typeInto c1 "\x00h"
        typeInto c1 "echo cfg-left-pane\r"
        awaitScreen c1 "cfg-left-pane"

        -- the format engine drives the status line
        awaitScreen c1 "clients=1"

        -- config reload binding shows the toast
        typeInto c1 "\x00r"
        awaitScreen c1 "reloaded"

        -- detach still works via the default d binding under new prefix
        typeInto c1 "\x00\&d"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

        -- control commands from a shell
        out1 <- ctlOut h ["list-sessions"]
        out1 `shouldSatisfy` List.isInfixOf "1 windows"
        out2 <- ctlOut h ["list-panes"]
        out2 `shouldSatisfy` List.isInfixOf "%"
        out3 <- ctlOut h ["display-message", "-p", "hello-cli"]
        out3 `shouldSatisfy` List.isInfixOf "hello-cli"

    it "autostarts the server when the socket directory does not exist yet" $
        -- the parent dirs of the socket must be created by the server.
        withHatOn hatBin "fresh/subdir/socket" $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

    it "attaches without a controlling terminal: exits with 'not a terminal'" $
        withHat hatBin $ \h -> do
        (code, _, err) <- hatCtl h []
        code `shouldBe` ExitFailure 1
        err `shouldSatisfy` List.isInfixOf "not a terminal"

    it "runs cat: line-buffered echo, not doubled" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "cat\r"
        threadDelay 300000
        -- Type without Enter: canonical mode echoes each char exactly once.
        typeInto c1 "abcxyz"
        awaitScreen c1 "abcxyz"
        threadDelay 200000
        scr <- screenText c1
        -- doubled echo would show "aabbcc..." somewhere
        scr `shouldNotSatisfy` T.isInfixOf "aabbccxxyyzz"
        typeInto c1 "\r"
        typeInto c1 "\x04"
        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

    it "runs cat: Ctrl-D sends EOF and returns to the shell" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "cat\r"
        threadDelay 300000
        typeInto c1 "hello world\r"
        awaitScreen c1 "hello world"
        -- Ctrl-D on an empty line EOFs cat; the shell then evaluates
        -- arithmetic that cat could never produce by echo alone.
        typeInto c1 "\x04"
        threadDelay 200000
        typeInto c1 "echo done-$((21+21))\r"
        awaitScreen c1 "done-42"
        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

pollServerGone :: FilePath -> Int -> IO Bool
pollServerGone _ 0 = pure False
pollServerGone path n = do
    m <- connectTo path
    case m of
        Nothing -> pure True
        Just _ -> do
            threadDelay 100_000
            pollServerGone path (n - 1)
