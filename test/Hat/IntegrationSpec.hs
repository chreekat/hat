-- | End-to-end tests driving the real @hat@ binary through a pty.
module Hat.IntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import qualified Data.List as List
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Posix.Temp (mkdtemp)
import System.Process (readProcess)
import System.Timeout (timeout)
import Test.Hspec

import qualified Data.Text as T

import Hat.Geometry
import Hat.Pty
import Hat.Socket (connectTo)
import qualified Hat.Term.Emulator as Emu

-- A hat client running inside a test-owned pty. The raw transcript
-- catches out-of-band messages ("[detached]"); the emulator models
-- what a human would see on the screen.
data Driver = Driver
    { pty :: PtyHandle
    , transcript :: IORef B8.ByteString
    , screen :: Emu.Emulator
    }

startClient :: FilePath -> FilePath -> IO Driver
startClient hatBin sockPath = startClientWith ["-S", sockPath] hatBin

startClientWith :: [String] -> FilePath -> IO Driver
startClientWith hatArgs hatBin = do
    -- Pane children need terminfo for TERM=screen-256color on NixOS.
    terminfo <- lookupEnv "TERMINFO_DIRS"
    p <- spawn Spawn
        { cmd = hatBin
        , args = hatArgs
        , env =
            [ ("PATH", "/run/current-system/sw/bin:/usr/bin:/bin")
            , ("TERM", "xterm-256color")
            , ("SHELL", "/bin/sh")
            , ("HOME", "/tmp")
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

-- Raw bytes, for out-of-band client messages after the alt screen closes.
awaitOutput :: Driver -> B8.ByteString -> IO ()
awaitOutput d needle = awaitWith (show needle) check d
  where
    check drv = B8.isInfixOf needle <$> readIORef drv.transcript

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

spec :: Spec
spec = do
    it "attaches, survives detach/reattach, and shuts down cleanly" $ do
        hatBin <- init <$> readProcess "cabal" ["list-bin", "hat"] ""
        dir <- mkdtemp "/tmp/hat-test-"
        let sockPath = dir <> "/test-socket"

        -- First client autostarts the server and gets a shell.
        c1 <- startClient hatBin sockPath
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
        alive <- connectTo sockPath
        alive `shouldSatisfy` \case
            Just _ -> True
            Nothing -> False

        -- Reattach: the redrawn screen still shows our marker.
        c2 <- startClient hatBin sockPath
        awaitScreen c2 "hat-integration-42"

        -- Ending the shell ends the session, the client, and the server.
        typeInto c2 "exit\r"
        status2 <- awaitExit c2
        status2 `shouldBe` Exited ExitSuccess
        t2 <- readIORef c2.transcript
        t2 `shouldSatisfy` B8.isInfixOf "[exited]"

        gone <- pollServerGone sockPath 50
        unless gone $ expectationFailure "server did not exit"

    it "renders vim to two simultaneous clients" $ do
        hatBin <- init <$> readProcess "cabal" ["list-bin", "hat"] ""
        dir <- mkdtemp "/tmp/hat-test-"
        let sockPath = dir <> "/test-socket"

        c1 <- startClient hatBin sockPath
        awaitScreen c1 "$"
        typeInto c1 "vim -u NONE -i NONE\r"
        awaitScreen c1 "VIM - Vi IMproved"

        -- A second client on the same session sees vim too.
        c2 <- startClient hatBin sockPath
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

    it "runs htop (colors, alt screen, live redraw)" $ do
        hatBin <- init <$> readProcess "cabal" ["list-bin", "hat"] ""
        dir <- mkdtemp "/tmp/hat-test-"
        let sockPath = dir <> "/test-socket"
        c1 <- startClient hatBin sockPath
        awaitScreen c1 "$"
        typeInto c1 "htop\r"
        awaitScreen c1 "Mem"
        awaitScreen c1 "Tasks"
        typeInto c1 "q"
        awaitScreen c1 "$"
        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

    it "splits panes, navigates, zooms, and kills" $ do
        hatBin <- init <$> readProcess "cabal" ["list-bin", "hat"] ""
        dir <- mkdtemp "/tmp/hat-test-"
        let sockPath = dir <> "/test-socket"
        c1 <- startClient hatBin sockPath
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

    it "creates and switches windows with a live status line" $ do
        hatBin <- init <$> readProcess "cabal" ["list-bin", "hat"] ""
        dir <- mkdtemp "/tmp/hat-test-"
        let sockPath = dir <> "/test-socket"
        c1 <- startClient hatBin sockPath
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

    it "loads a user config: C-Space prefix, vim keys, base-index 1" $ do
        hatBin <- init <$> readProcess "cabal" ["list-bin", "hat"] ""
        dir <- mkdtemp "/tmp/hat-test-"
        let sockPath = dir <> "/test-socket"
            confPath = dir <> "/hat.conf"
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
            ]
        c1 <- startClientWith ["-S", sockPath, "-f", confPath] hatBin
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

        -- config reload binding shows the toast
        typeInto c1 "\x00r"
        awaitScreen c1 "reloaded"

        -- detach still works via the default d binding under new prefix
        typeInto c1 "\x00\&d"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

        -- control commands from a shell
        out1 <- readProcess hatBin ["-S", sockPath, "list-sessions"] ""
        out1 `shouldSatisfy` List.isInfixOf "1 windows"
        out2 <- readProcess hatBin ["-S", sockPath, "list-panes"] ""
        out2 `shouldSatisfy` List.isInfixOf "%"
        out3 <- readProcess hatBin ["-S", sockPath, "display-message", "-p", "hello-cli"] ""
        out3 `shouldSatisfy` List.isInfixOf "hello-cli"
        _ <- readProcess hatBin ["-S", sockPath, "kill-server"] ""
        gone <- pollServerGone sockPath 50
        gone `shouldBe` True

pollServerGone :: FilePath -> Int -> IO Bool
pollServerGone _ 0 = pure False
pollServerGone path n = do
    m <- connectTo path
    case m of
        Nothing -> pure True
        Just _ -> do
            threadDelay 100_000
            pollServerGone path (n - 1)
