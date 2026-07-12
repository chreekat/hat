-- | End-to-end tests driving the real @hat@ binary through a pty.
module Hat.IntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, catch, finally, try)
import Control.Monad (forM_, unless, when)
import qualified Data.List as List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import System.Directory (removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (Handle, hSetBuffering, BufferMode (..))
import System.Posix.IO (fdToHandle)
import System.Posix.Process (ProcessStatus (..))
import System.Posix.Temp (mkdtemp)
import System.Posix.Terminal (openPseudoTerminal)
import System.Process (readProcess)
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec

import qualified Data.Text as T
import qualified Data.Vector as V

import Data.Maybe (listToMaybe)
import Hat.Geometry
import Hat.Pty (setWinsize)
import Hat.Socket (connectTo)
import Hat.Term.Cell (Cell (..), Color, Style (..))
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
withHat :: FilePath -> (Hat -> IO a) -> IO a
withHat hatBin = withHatOn hatBin "socket"

-- | 'withHat' with a custom socket path relative to the temp HOME (used
-- by the test that needs the socket's parent dirs to not exist yet).
withHatOn :: FilePath -> FilePath -> (Hat -> IO a) -> IO a
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
    { pty :: Client
    , transcript :: IORef B8.ByteString
    , screen :: Emu.Emulator
    }

-- The hat binary driven as a black box: a plain child process wired to a
-- test-owned pty. No controlling-terminal setup or in-process fork — just
-- 'createProcess' onto the pty slave, so the test binary is never forked.
data Client = Client
    { master :: Handle           -- our end of the pty
    , proch  :: P.ProcessHandle   -- the hat client process
    }

startClient :: Hat -> IO Driver
startClient h = startClientArgs h []

startClientArgs :: Hat -> [String] -> IO Driver
startClientArgs h extra = do
    -- Pane children need terminfo for TERM=tmux-256color on NixOS.
    terminfo <- lookupEnv "TERMINFO_DIRS"
    let size = Size { rows = 24, cols = 80 }
    (masterFd, slaveFd) <- openPseudoTerminal
    setWinsize slaveFd size
    slaveH <- fdToHandle slaveFd
    (_, _, _, ph) <-
        P.createProcess (P.proc h.bin (["-S", h.sock] <> extra))
            { P.std_in  = P.UseHandle slaveH
            , P.std_out = P.UseHandle slaveH
            , P.std_err = P.UseHandle slaveH
            , P.new_session = True
            , P.cwd = Just "/tmp"
            , P.env = Just $
                [ ("PATH", testPath)
                , ("TERM", "xterm-256color")
                , ("SHELL", "/bin/sh")
                , ("HOME", h.home)
                , ("PS1", "$ ")
                ]
                <> maybe [] (\v -> [("TERMINFO_DIRS", v)]) terminfo
            }
    masterH <- fdToHandle masterFd
    hSetBuffering masterH NoBuffering
    t <- newIORef ""
    emu <- Emu.newEmulator size 1000
    pure Driver
        { pty = Client { master = masterH, proch = ph }
        , transcript = t
        , screen = emu
        }

-- Blocking read of available bytes; empty means the pty closed (the hat
-- client exited and the slave went away — Linux reports EIO at pty EOF).
readPty :: Client -> IO B8.ByteString
readPty c = do
    r <- try (B.hGetSome c.master 65536)
    pure $ case r of
        Left (_ :: IOException) -> B.empty
        Right bs -> bs

writePty :: Client -> B8.ByteString -> IO ()
writePty c bs = B.hPut c.master bs `catch` \(_ :: IOException) -> pure ()

-- Block on the real child exit — no polling, no clock.
waitExit :: Client -> IO ProcessStatus
waitExit c = Exited <$> P.waitForProcess c.proch

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
                when (B8.null chunk) $ do
                    t <- readIORef d.transcript
                    expectationFailure $ "pty closed while waiting for " <> what
                        <> "\ntranscript: " <> show t
                ingest d chunk
                go

-- What a user would see on screen.
awaitScreen :: Driver -> T.Text -> IO ()
awaitScreen d needle = awaitWith (show needle) check d
  where
    check drv = T.isInfixOf needle <$> screenText drv

-- The command prompt replaces the status line, so the current window's
-- status marker (e.g. "0:sh*") disappearing is a reliable "the prompt is
-- open" signal — unlike the bare ':', which the status line also shows.
awaitPromptOpen :: Driver -> T.Text -> IO ()
awaitPromptOpen d marker = awaitWith "command prompt to open" check d
  where
    check drv = not . T.isInfixOf marker <$> screenText drv

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

-- The foreground colour of a bold pane-border cell (│ or heavy ┃), if
-- any — how the active-border style shows up on screen.
boldBorderFg :: Driver -> IO (Maybe Color)
boldBorderFg d = do
    scr <- Emu.snapshot d.screen
    pure $ listToMaybe
        [ cell.style.fg
        | row <- V.toList scr.cells, cell <- V.toList row
        , cell.text `elem` (["\x2502", "\x2503"] :: [T.Text])
        , cell.style.bold ]

-- The column of the vertical pane border (│) on a content row, if any.
verticalBorderCol :: Driver -> IO (Maybe Int)
verticalBorderCol d = do
    scr <- Emu.snapshot d.screen
    pure $ case scr.cells V.!? 5 of
        Just row -> listToMaybe
            [ c | (c, cell) <- zip [0 ..] (V.toList row), cell.text == "\x2502" ]
        Nothing -> Nothing

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

    it "resize-pane -t ! -Z zooms the alternate pane, not the active one" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        -- Original pane gets a marker; it becomes the alternate after split.
        typeInto c1 "echo alt-pane-zone\r"
        awaitScreen c1 "alt-pane-zone"
        -- Vertical split: the new right pane is active, the original is last.
        typeInto c1 "\x02%"
        awaitScreen c1 "\x2502"      -- │
        typeInto c1 "echo main-pane-zone\r"
        awaitScreen c1 "main-pane-zone"

        -- Zoom the ALTERNATE pane (-t !): the original fills the screen,
        -- the border and the active pane's content both vanish.
        _ <- ctlOut h ["resize-pane", "-t", "!", "-Z"]
        awaitWith "alternate pane zoomed" (\d -> do
            t <- screenText d
            pure ("alt-pane-zone" `T.isInfixOf` t
                  && not ("\x2502" `T.isInfixOf` t)
                  && not ("main-pane-zone" `T.isInfixOf` t))) c1

    it "clear-history drops the pane's scrollback" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        -- Fill scrollback, then confirm copy mode reports a non-empty history.
        -- The arithmetic echo output ("hist199") appears only once the 200
        -- lines above it have scrolled into scrollback, avoiding a race with
        -- the echoed input line.
        typeInto c1 "seq 1 200\r"
        typeInto c1 "echo hist$((100+99))\r"
        awaitScreen c1 "hist199"
        typeInto c1 "\x02["                 -- copy mode
        awaitScreen c1 "[0/"
        scr <- screenText c1
        scr `shouldNotSatisfy` T.isInfixOf "[0/0]"
        typeInto c1 "q"                     -- exit copy mode
        awaitWith "indicator gone" (\d ->
            not . T.isInfixOf "[0/" <$> screenText d) c1

        -- clear-history empties the scrollback; copy mode now shows [0/0].
        _ <- ctlOut h ["clear-history"]
        typeInto c1 "\x02["
        awaitScreen c1 "[0/0]"

    it "moves a pane to the far edge (splitw -f; swap-pane; kill-pane)" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "echo edge-marker\r"
        awaitScreen c1 "edge-marker"
        -- Stack a second pane below; the original (top) keeps the marker.
        typeInto c1 "\x02\""                -- split -v
        awaitScreen c1 "\x2500"             -- ─
        -- Focus the marked pane, then run the tmux move-to-edge idiom.
        _ <- ctlOut h ["select-pane", "-U"]
        _ <- ctlOut h ["split-window", "-fh"]
        _ <- ctlOut h ["swap-pane", "-t", "!"]
        _ <- ctlOut h ["kill-pane", "-t", "!"]
        -- The marker survives and now lives in a full-height edge column
        -- (a vertical border proves two side-by-side panes remain).
        awaitWith "pane moved to edge, content preserved" (\d -> do
            t <- screenText d
            pure ("edge-marker" `T.isInfixOf` t && "\x2502" `T.isInfixOf` t)) c1

    it "break-pane moves the active pane into its own window" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        typeInto c1 "\x02%"                 -- split -h; new pane active
        awaitScreen c1 "\x2502"
        typeInto c1 "echo b-marker\r"
        awaitScreen c1 "b-marker"
        _ <- ctlOut h ["break-pane"]
        -- The pane is now window 1 (current), sharing no split border, and
        -- window 0 still exists in the status line.
        awaitWith "broken out into window 1" (\d -> do
            t <- screenText d
            pure ("b-marker" `T.isInfixOf` t
                  && not ("\x2502" `T.isInfixOf` t)
                  && "0:" `T.isInfixOf` t && "1:" `T.isInfixOf` t)) c1

    it "join-pane merges a pane in from another window" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        typeInto c1 "echo win0-marker\r"
        awaitScreen c1 "win0-marker"
        typeInto c1 "\x02\&c"               -- new window (index 1)
        awaitScreen c1 "1:sh*"
        typeInto c1 "echo win1-marker\r"
        awaitScreen c1 "win1-marker"
        -- Grab window 1's pane id, switch back to window 0, join it in.
        pidOut <- ctlOut h ["list-panes"]
        let paneId = takeWhile (/= ':') pidOut
        typeInto c1 "\x02\&0"
        awaitScreen c1 "win0-marker"
        _ <- ctlOut h ["join-pane", "-h", "-s", paneId]
        awaitWith "both panes share window 0" (\d -> do
            t <- screenText d
            pure ("win0-marker" `T.isInfixOf` t
                  && "win1-marker" `T.isInfixOf` t
                  && "\x2502" `T.isInfixOf` t)) c1

    it "choose-tree lists windows; search + Enter switches to one" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf")
            "bind / choose-tree -GZw \\; send-keys /\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh*"
        typeInto c1 "echo win0-marker\r"
        awaitScreen c1 "win0-marker"
        typeInto c1 "\x02\&c"               -- new window 1
        awaitScreen c1 "1:sh*"
        typeInto c1 "echo win1-marker\r"
        awaitScreen c1 "win1-marker"
        typeInto c1 "\x02\&0"               -- back to window 0
        awaitScreen c1 "win0-marker"
        -- prefix / opens choose-tree and (via send-keys /) enters search.
        typeInto c1 "\x02/"
        awaitScreen c1 "choose a window"
        -- Filter to the window-1 row, then select it.
        typeInto c1 "1"
        typeInto c1 "\r"
        awaitScreen c1 "win1-marker"

    it "choose-window joins the chosen window's pane (V binding)" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf")
            "bind V choose-window 'join-pane -hs \"%%\"'\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh*"
        typeInto c1 "echo win0-marker\r"
        awaitScreen c1 "win0-marker"
        typeInto c1 "\x02\&c"               -- new window 1
        awaitScreen c1 "1:sh*"
        typeInto c1 "echo win1-marker\r"
        awaitScreen c1 "win1-marker"
        typeInto c1 "\x02\&0"               -- back to window 0
        awaitScreen c1 "win0-marker"
        -- prefix V opens choose-window; pick window 1 and join its pane.
        typeInto c1 "\x02V"
        awaitScreen c1 "choose a window"
        typeInto c1 "j"                     -- cursor to window 1
        typeInto c1 "\r"
        awaitWith "window 1's pane joined into window 0" (\d -> do
            t <- screenText d
            pure ("win0-marker" `T.isInfixOf` t
                  && "win1-marker" `T.isInfixOf` t
                  && "\x2502" `T.isInfixOf` t)) c1

    it "prefix R toggles the pane theme, recoloring borders (styling)" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") $ unlines
            [ "set -g pane-border-style 'fg=colour240'"
            , "set -g pane-active-border-style 'fg=brightwhite,bold'"
            , "set -g pane-border-lines heavy"
            , "set -g pane-border-indicators both"
            , "set -g @pane-theme dark"
            , "bind R if-shell '[ \"#{@pane-theme}\" = \"dark\" ]' "
              <> "'set -g pane-active-border-style \"fg=black,bold\" ; "
              <> "set -g @pane-theme light ; display-message \"Theme: light\"' "
              <> "'set -g pane-active-border-style \"fg=brightwhite,bold\" ; "
              <> "set -g @pane-theme dark ; display-message \"Theme: dark\"'"
            ]
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh*"
        typeInto c1 "\x02%"                  -- split -h; heavy border ┃ appears
        awaitScreen c1 "\x2503"              -- ┃ proves pane-border-lines heavy
        -- Dark theme: the active border is bold; capture its colour.
        awaitWith "bold active border" (\d -> (/= Nothing) <$> boldBorderFg d) c1
        darkFg <- boldBorderFg c1
        -- prefix R runs the if-shell theme toggle (needs #{@pane-theme}).
        typeInto c1 "\x02R"
        awaitScreen c1 "Theme: light"
        -- Light theme recolours the (still bold) active border.
        awaitWith "active border recoloured" (\d -> do
            mfg <- boldBorderFg d
            pure (mfg /= Nothing && mfg /= darkFg)) c1

    it "monitor-activity flags a background window; next-window -a jumps to it" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") $ unlines
            [ "set -g monitor-activity on"
            , "bind C-a next-window -a" ]
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh*"
        -- Arm delayed output in window 0, then leave it for a new window.
        typeInto c1 "sleep 1 && echo bg-activity-marker\r"
        typeInto c1 "\x02\&c"                -- new window 1
        awaitScreen c1 "1:sh*"
        -- When window 0 emits, it is flagged with activity (# in the flags;
        -- it is also the last window, so "-#").
        awaitScreen c1 "0:sh-#"
        -- prefix C-a jumps to the window carrying activity (window 0).
        typeInto c1 "\x02\x01"
        awaitScreen c1 "bg-activity-marker"
        awaitScreen c1 "0:sh*"               -- now current; activity cleared

    it "drops focus reports for a pane that never enabled ?1004" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g focus-events on\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "$"
        typeInto c1 "cat -v\r"               -- render control chars visibly
        threadDelay 300000
        typeInto c1 "\ESC[I"                 -- a focus-in report from the terminal
        typeInto c1 "sentinel\r"
        -- The bare shell/cat never requested focus reporting, so the report
        -- is dropped rather than echoed as a stray "^[[I".
        awaitWith "no stray focus report" (\d -> do
            t <- screenText d
            pure ("sentinel" `T.isInfixOf` t
                  && not ("^[[I" `T.isInfixOf` t))) c1

    it "forwards focus reports to a pane that enabled ?1004" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g focus-events on\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "$"
        typeInto c1 "printf '\\033[?1004h'\r" -- the app requests focus reporting
        threadDelay 200000
        typeInto c1 "cat -v\r"               -- render control chars visibly
        threadDelay 300000
        typeInto c1 "\ESC[I"                 -- a focus-in report from the terminal
        awaitScreen c1 "^[[I"                -- forwarded to the pane, echoed by cat -v

    it "select-layout rearranges panes; main-vertical honors main-pane-width" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g main-pane-width 50\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh*"
        typeInto c1 "\x02%"                  -- 2 panes
        awaitScreen c1 "\x2502"
        typeInto c1 "\x02%"                  -- 3 panes
        threadDelay 200000
        -- even-vertical stacks them: horizontal borders, no vertical.
        _ <- ctlOut h ["select-layout", "even-vertical"]
        awaitWith "stacked (no vertical border)" (\d -> do
            t <- screenText d
            pure ("\x2500" `T.isInfixOf` t
                  && not ("\x2502" `T.isInfixOf` t))) c1
        -- main-vertical: a ~50-column main pane on the left, stack on the right.
        _ <- ctlOut h ["select-layout", "main-vertical"]
        awaitWith "main pane ~50 cols wide" (\d -> do
            mcol <- verticalBorderCol d
            t <- screenText d
            pure (maybe False (\c -> c >= 46 && c <= 52) mcol
                  && "\x2500" `T.isInfixOf` t)) c1

    it "round-trips @-options via set -gq / show -gqv (resurrect config)" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        _ <- ctlOut h ["set", "-gq", "@resurrect-save", "M-s"]
        out <- ctlOut h ["show", "-gqv", "@resurrect-save"]
        out `shouldSatisfy` List.isInfixOf "M-s"

    it "list-panes -aF emits resurrect's per-pane fields" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "\x02%"                  -- two panes
        awaitScreen c1 "\x2502"
        out <- ctlOut h
            [ "list-panes", "-a", "-F"
            , "#{pane_id}|#{pane_current_path}|#{pane_current_command}|#{window_layout}" ]
        -- a row per pane, each carrying id, cwd, command and a layout string
        length (lines out) `shouldSatisfy` (>= 2)
        out `shouldSatisfy` List.isInfixOf "%"     -- pane_id
        out `shouldSatisfy` List.isInfixOf "/"     -- pane_current_path
        out `shouldSatisfy` List.isInfixOf "x"     -- WxH in window_layout

    it "moves a window to a new index (move-window)" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        typeInto c1 "\x02\&c"                -- window 1
        awaitScreen c1 "1:sh*"
        _ <- ctlOut h ["move-window", "-s", "1", "-t", "5"]
        out <- ctlOut h ["list-windows", "-F", "#{window_index}"]
        lines out `shouldSatisfy` elem "5"

    it "restores a saved window_layout (resurrect's select-layout replay)" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "\x02%"                  -- split -h: side-by-side panes
        awaitScreen c1 "\x2502"
        -- "save": capture the window_layout string, as save.sh does
        layoutOut <- ctlOut h ["list-windows", "-F", "#{window_layout}"]
        let layout = takeWhile (/= '\n') layoutOut
        -- mangle: stack the panes (no vertical border)
        _ <- ctlOut h ["select-layout", "even-vertical"]
        awaitWith "stacked" (\d -> not . T.isInfixOf "\x2502" <$> screenText d) c1
        -- "restore": replay the saved layout -> side-by-side returns
        _ <- ctlOut h ["select-layout", layout]
        awaitScreen c1 "\x2502"

    it "save -> kill-server -> restart -> restore rebuilds the window tree" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        -- Build a tree: window 0 renamed, plus a named window 1.
        _ <- ctlOut h ["rename-window", "built0"]
        typeInto c1 "\x02\&c"
        awaitScreen c1 "1:sh*"
        _ <- ctlOut h ["rename-window", "editor"]
        awaitScreen c1 "1:editor*"
        -- Save, as resurrect's save.sh does: dump index+name per window.
        saved <- ctlOut h ["list-windows", "-F", "#{window_index}:#{window_name}"]

        -- Kill the server; wait for it to be gone.
        _ <- ctlOut h ["kill-server"]
        _ <- awaitExit c1
        gone <- pollServerGone h.sock 50
        unless gone $ expectationFailure "server did not die"

        -- Restart: a fresh client autostarts a new server and session.
        c2 <- startClient h
        awaitScreen c2 "0:sh*"
        -- Restore: replay the saved windows (resurrect replays new-window).
        forM_ (lines saved) $ \line -> case break (== ':') line of
            (ix, ':' : nm)
                | ix == "0" -> ctlOut h ["rename-window", "-t", "0", nm] >> pure ()
                | otherwise ->
                    ctlOut h ["new-window", "-d", "-t", ix, "-n", nm] >> pure ()
            _ -> pure ()
        -- The saved tree is back: window 0 renamed, window 1 'editor'.
        awaitScreen c2 "0:built0"
        awaitScreen c2 "1:editor"

    it "opens the command prompt (:) and runs the typed command" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"       -- status line is up

        -- prefix : opens the prompt; typed text echoes on the status row.
        typeInto c1 "\x02:"
        awaitPromptOpen c1 "0:sh*"
        typeInto c1 "rename-window promptwin"
        awaitScreen c1 ":rename-window promptwin"

        -- Enter runs it: the status line shows the renamed window. The
        -- trailing '*' (current-window marker) never appears in the
        -- echoed prompt text, so this only matches after execution.
        typeInto c1 "\r"
        awaitScreen c1 "promptwin*"

        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

    it "recalls a previous command from prompt history with Up" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"

        -- Run one command through the prompt.
        typeInto c1 "\x02:"
        awaitPromptOpen c1 "0:sh*"
        typeInto c1 "rename-window histwin"
        awaitScreen c1 ":rename-window histwin"
        typeInto c1 "\r"
        awaitScreen c1 "histwin*"

        -- Reopen the prompt; Up recalls that command onto the line.
        typeInto c1 "\x02:"
        awaitPromptOpen c1 "histwin*"
        typeInto c1 "\ESC[A"  -- Up
        awaitScreen c1 ":rename-window histwin"

        typeInto c1 "\ESC"    -- cancel; wait for the prompt to close
        awaitWith "prompt closed" (\d -> do
            t <- screenText d
            pure ("histwin*" `T.isInfixOf` t
                  && not (":rename-window" `T.isInfixOf` t))) c1
        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

    it "cancels the command prompt on Escape without running it" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        typeInto c1 "\x02:"
        awaitPromptOpen c1 "0:sh*"
        typeInto c1 "rename-window nope"
        awaitScreen c1 ":rename-window nope"
        -- Escape closes the prompt; the window keeps its name.
        typeInto c1 "\ESC"
        awaitWith "prompt gone, name unchanged" (\d -> do
            t <- screenText d
            pure ("0:sh*" `T.isInfixOf` t
                  && not ("nope" `T.isInfixOf` t))) c1
        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

    it "renames the current window via prefix , (pre-filled prompt)" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"

        -- prefix , opens a rename prompt pre-filled with the window name
        -- (#W), behind the (rename-window) label.
        typeInto c1 "\x02,"
        awaitScreen c1 "(rename-window) sh"

        -- The pre-fill is editable; Enter runs the spliced template and
        -- the status line shows the new name.
        typeInto c1 "\DEL\DELlogs\r"
        awaitScreen c1 "logs*"

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

    it "copy-pipe feeds the selection to a shell command" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        -- Leave a marker on the input line, unentered, then select it.
        typeInto c1 "echo PIPEDWORD"
        awaitScreen c1 "PIPEDWORD"
        baseline <- reverseCellCount c1
        typeInto c1 "\x02["
        threadDelay 200000
        typeInto c1 "\x01"           -- C-a start-of-line
        typeInto c1 " "              -- Space begin-selection
        typeInto c1 "\x05"           -- C-e end-of-line
        awaitWith "selection highlighted" (\d ->
            (> baseline) <$> reverseCellCount d) c1

        -- copy-pipe writes the selected line to a file via the shell.
        let outPath = h.home <> "/piped.txt"
        _ <- ctlOut h ["send-keys", "-X", "copy-pipe", "cat > " <> outPath]
        threadDelay 300000
        contents <- readFile outPath
        contents `shouldSatisfy` List.isInfixOf "PIPEDWORD"

    it "shows the [scroll/history] position indicator on entering copy mode" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        -- No indicator until copy mode is entered.
        scr0 <- screenText c1
        scr0 `shouldNotSatisfy` T.isInfixOf "[0/"
        typeInto c1 "\x02["              -- C-b [
        awaitScreen c1 "[0/"             -- top-right box appears immediately
        typeInto c1 "q"                  -- exit
        awaitWith "indicator gone" (\d ->
            not . T.isInfixOf "[0/" <$> screenText d) c1

    it "surfaces copy mode in the status line via pane_in_mode" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf")
            "set -g status-right '#{?pane_in_mode,COPYMODE,normal}'\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "normal"          -- not in copy mode yet
        typeInto c1 "\x02["              -- C-b [ enters copy mode
        awaitScreen c1 "COPYMODE"        -- the indicator flips
        typeInto c1 "q"                  -- exit copy mode
        awaitScreen c1 "normal"          -- and it reverts

    it "pipe-pane tees pane output to a command, and stops on demand" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        let logPath = h.home <> "/pane.log"
        _ <- ctlOut h ["pipe-pane", "cat >> " <> logPath]
        typeInto c1 "echo PIPEPANEMARKER\r"
        awaitScreen c1 "PIPEPANEMARKER"
        threadDelay 300000
        readFile logPath >>= (`shouldSatisfy` List.isInfixOf "PIPEPANEMARKER")

        -- No-arg pipe-pane stops the pipe; later output is not captured.
        _ <- ctlOut h ["pipe-pane"]
        typeInto c1 "echo AFTERSTOPMARKER\r"
        awaitScreen c1 "AFTERSTOPMARKER"
        threadDelay 300000
        readFile logPath >>= (`shouldNotSatisfy` List.isInfixOf "AFTERSTOPMARKER")

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
