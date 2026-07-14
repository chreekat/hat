-- | End-to-end tests driving the real @hat@ binary through a pty.
module Hat.IntegrationSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, catch, evaluate, finally, throwIO, try)
import Control.Monad (forM, unless, when)
import qualified Data.List as List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (Handle, hSetBuffering, BufferMode (..))
import System.Posix.IO (fdToHandle, handleToFd)
import System.Posix.Process (ProcessStatus (..))
import System.Posix.Temp (mkdtemp)
import System.Posix.Terminal
    ( TerminalMode (..), TerminalState (Immediately), getTerminalAttributes
    , openPseudoTerminal, setTerminalAttributes, terminalMode )
import System.Process (readProcess)
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec

import qualified Data.Text as T
import qualified Data.Vector as V

import Data.Maybe (listToMaybe)
import Data.Ratio ((%))
import System.FilePath (takeDirectory, takeFileName, (</>))
import Hat.Geometry
import Hat.Model.Ids (PaneId (..))
import Hat.Server.Persist (PaneSnap (..), SessionSnap (..), Snapshot (..), WindowSnap (..))
import qualified Hat.Server.Persist as Persist
import Hat.Term.Pty (setWinsize)
import Hat.Server.Layout (Layout (..), Orientation (..), sizeRect)
import Hat.Server.LayoutString (emitLayout)
import Hat.Transport.Socket (connectTo)
import Hat.Term.Cell (Cell (..), Color (..), Style (..))
import qualified Hat.Term.Emulator as Emu

-- An isolated hat instance for one test: a private HOME (so @hat@ never
-- reads any ambient config) and a private socket. See 'withHat'.
data Hat = Hat
    { bin     :: FilePath
    , home    :: FilePath
    , sock    :: FilePath
    , persist :: Bool   -- ^ whether the server keeps its SQLite store
    }

-- Disable the SQLite persistence layer unless a test opts in, so most
-- servers start from a clean slate and skip the background poll thread.
-- When on, point the store at a private dir so isolation does not lean on
-- the server's HOME/XDG derivation.
persistEnv :: Hat -> [(String, String)]
persistEnv h
    | h.persist = [("HAT_STORE_DIR", storeDir h)]
    | otherwise = [("HAT_PERSIST", "0")]

-- The private store directory and file for a persistence-enabled Hat.
storeDir :: Hat -> FilePath
storeDir h = h.home </> "store"

-- Minimal PATH for spawned shells and the hat binary. Deliberately does
-- not inherit the ambient environment.
testPath :: String
testPath = "/run/current-system/sw/bin:/usr/bin:/bin"

-- | Run a test against a freshly isolated hat, tearing the server and
-- its temp dir down afterwards no matter how the test ends. The socket
-- lives at @<tmp>/socket@.
withHat :: FilePath -> (Hat -> IO a) -> IO a
withHat hatBin = withHatOn hatBin False "socket"

-- | 'withHat' with persistence left on, for the tests that exercise
-- save/restore across a server restart.
withHatPersist :: FilePath -> (Hat -> IO a) -> IO a
withHatPersist hatBin = withHatOn hatBin True "socket"

-- | 'withHat' with a custom socket path relative to the temp HOME (used
-- by the test that needs the socket's parent dirs to not exist yet).
withHatOn :: FilePath -> Bool -> FilePath -> (Hat -> IO a) -> IO a
withHatOn hatBin persistOn sockRel action = do
    dir <- mkdtemp "/tmp/hat-test-"
    let h = Hat
            { bin = hatBin, home = dir
            , sock = dir <> "/" <> sockRel, persist = persistOn }
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
            { P.env = Just ([("HOME", h.home), ("PATH", testPath)] <> persistEnv h) }
        ""

-- Stdout of a hat control command.
ctlOut :: Hat -> [String] -> IO String
ctlOut h args = (\(_, out, _) -> out) <$> hatCtl h args

-- Where the server writes its persistence store for this isolated Hat,
-- matching the HAT_STORE_DIR the harness hands it in 'persistEnv'.
storeOf :: Hat -> FilePath
storeOf h = storeDir h </> (takeFileName h.sock <> ".db")

-- | Write an executable file via subprocesses (sh + chmod), never via
-- writeFile: a write fd held in this forking, parallel test process
-- leaks into other spawns' fork→exec windows, and exec'ing a
-- still-write-open file flakes with ETXTBSY (scripts included — the
-- kernel write-denies the exec'd file before the shebang is ever read).
writeExecutable :: FilePath -> String -> IO ()
writeExecutable path content = do
    _ <- P.readProcess "/bin/sh"
        ["-c", "cat > " <> path <> " && chmod 755 " <> path] content
    pure ()

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
startClientArgs h = startClientEnv h []

-- | 'startClientArgs' plus extra environment entries; an entry whose key
-- collides with a default (e.g. @PATH@) replaces it. The server the
-- first client autostarts inherits this environment too.
startClientEnv :: Hat -> [(String, String)] -> [String] -> IO Driver
startClientEnv h extraEnv extra = do
    -- Pane children need terminfo for TERM=tmux-256color on NixOS.
    terminfo <- lookupEnv "TERMINFO_DIRS"
    let size = Size { rows = 24, cols = 80 }
    (masterFd, slaveFd) <- openPseudoTerminal
    setWinsize slaveFd size
    masterH <- fdToHandle masterFd
    -- hSetBuffering NoBuffering on the master does a hidden tcsetattr
    -- (GHC's setRaw clears ICANON) on the pty's one shared termios — the
    -- very state the hat client is about to inherit as its "original".
    -- Take that side effect before the client spawns and undo it, so the
    -- client sees (and must hand back) a cooked terminal.
    cooked <- getTerminalAttributes slaveFd
    hSetBuffering masterH NoBuffering
    setTerminalAttributes slaveFd cooked Immediately
    slaveH <- fdToHandle slaveFd
    (_, _, _, ph) <-
        P.createProcess (P.proc h.bin (["-S", h.sock] <> extra))
            { P.std_in  = P.UseHandle slaveH
            , P.std_out = P.UseHandle slaveH
            , P.std_err = P.UseHandle slaveH
            , P.new_session = True
            , P.cwd = Just "/tmp"
            , P.env = Just $
                let defaults =
                        [ ("PATH", testPath)
                        , ("TERM", "xterm-256color")
                        , ("SHELL", "/bin/sh")
                        , ("HOME", h.home)
                        , ("PS1", "$ ")
                        ]
                        <> persistEnv h
                        <> maybe [] (\v -> [("TERMINFO_DIRS", v)]) terminfo
                in extraEnv
                   <> [kv | kv@(k, _) <- defaults
                          , k `notElem` map fst extraEnv]
            }
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

-- Poll a file until its contents satisfy the predicate; fail after ~5s.
-- pipe-pane's long-lived subprocess flushes on its own schedule and nothing
-- in hat publishes a "the file is ready" event (only OS-level inotify would),
-- so this retries until the condition holds rather than assuming a fixed delay.
awaitFile :: FilePath -> (String -> Bool) -> IO String
awaitFile path ok = go (500 :: Int)
  where
    go n = do
        c <- readStrict `catch` \(_ :: IOException) -> pure ""
        if ok c || n <= 0 then pure c else threadDelay 10000 >> go (n - 1)
    readStrict = do
        s <- readFile path
        _ <- evaluate (length s)
        pure s

-- Poll until the current pane's foreground command matches, so a test can
-- wait for a program (e.g. cat) to actually be running before driving it.
-- This is genuinely a poll, not a laziness: Unix publishes no event when a
-- child becomes a terminal's foreground process group (tcsetpgrp notifies
-- nobody), so the only way to observe it is to sample it.
awaitForeground :: Hat -> String -> IO ()
awaitForeground h cmd = go (500 :: Int)
  where
    go n = do
        out <- ctlOut h ["list-panes", "-F", "#{pane_current_command}"]
        if cmd `List.isInfixOf` out || n <= 0
            then pure ()
            else threadDelay 10000 >> go (n - 1)

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

-- Run all actions concurrently, collect their results, and re-raise the
-- first exception. (The test suite has no async dependency.)
runConcurrently :: forall a. [IO a] -> IO [a]
runConcurrently actions = do
    vars <- forM actions $ \act -> do
        v <- newEmptyMVar
        _ <- forkIO $ do
            r <- try act
            putMVar v (r :: Either SomeException a)
        pure v
    forM vars $ \v -> takeMVar v >>= either throwIO pure

-- Run a shell line that ends in @stty -a@ and report whether the rendered
-- line discipline is non-canonical (@-icanon@) — the state that stops
-- Ctrl-D from signalling EOF. The command runs everything on one line so
-- bash/readline does not restore its saved termios between a preceding
-- @stty@ and the read-back.
paneNonCanonicalVia :: B8.ByteString -> Driver -> IO Bool
paneNonCanonicalVia line c = do
    typeInto c (line <> "\r")
    awaitScreen c "baud"          -- the stty -a report has rendered
    T.isInfixOf "-icanon" <$> screenText c

-- A healthy pane is canonical (@icanon@); the bug leaves it @-icanon@.
paneIsNonCanonical :: Driver -> IO Bool
paneIsNonCanonical = paneNonCanonicalVia "stty -a"

-- Print a line of digits and wait until BOTH the output row and the next
-- prompt below it have rendered. A bare @awaitScreen "0123456789"@ would
-- match the echoed printf command itself, letting the test race ahead
-- (e.g. into copy mode) while the cursor is still rows away from where
-- the digit line will land.
printDigitLine :: Driver -> IO ()
printDigitLine c = do
    typeInto c "printf '0123456789\\n'\r"
    awaitWith "digit line printed and prompt back" (\d -> do
        scr <- Emu.snapshot d.screen
        let row r = T.strip (Emu.screenRowText scr r)
        pure $ case List.findIndex (\r -> row r == "0123456789") [0 .. 23] of
            Just r -> "$" `T.isPrefixOf` row (r + 1)
            Nothing -> False) c

-- The cooked-mode flags still missing from the test pty's line discipline,
-- read after the hat client on its slave side has exited. An empty list
-- means the client left the terminal the way it found it.
rawModesLeftBehind :: Driver -> IO [String]
rawModesLeftBehind d = do
    fd <- handleToFd d.pty.master
    attrs <- getTerminalAttributes fd
    pure [ nm
         | (nm, mode) <-
             [ ("icanon", ProcessInput)
             , ("echo", EnableEcho)
             , ("opost", ProcessOutput)
             , ("isig", KeyboardInterrupts)
             ]
         , not (terminalMode mode attrs) ]

spec :: Spec
spec = parallel $ do
    hatBin <- runIO (init <$> readProcess "cabal" ["list-bin", "hat"] "")

    it "gives every pane a canonical line discipline under concurrent spawn load" $ do
        let n = 24 :: Int
        bads <- runConcurrently
            [ withHat hatBin $ \h -> do
                c <- startClient h
                awaitScreen c "$"
                paneIsNonCanonical c
            | _ <- [1 .. n] ]
        let bad = length (filter id bads)
        when (bad > 0) $ expectationFailure $
            show bad <> " of " <> show n
                <> " panes came up non-canonical (-icanon), which breaks Ctrl-D/EOF"

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

    -- A pane whose shell ignores SIGHUP (and keeps the pty slave open) gives
    -- the reader thread no EOF, so a teardown that closes the master Handle
    -- while the reader still blocks in it deadlocks. kill-server must instead
    -- interrupt the reader and complete within a bounded deadline.
    it "kill-server completes even when a pane ignores SIGHUP" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "trap '' HUP; sleep 300\r"
        awaitForeground h "sleep"

        killed <- timeout 5_000_000 (hatCtl h ["kill-server"])
        case killed of
            Nothing -> expectationFailure "kill-server did not return within 5s"
            Just _ -> do
                gone <- pollServerGone h.sock 50
                unless gone $
                    expectationFailure "server did not exit after kill-server"

    it "restores the terminal line discipline when the session ends" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "exit\r"
        _ <- awaitExit c1
        left <- rawModesLeftBehind c1
        left `shouldBe` []

    it "restores the terminal line discipline after detach" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "\x02\&d"
        _ <- awaitExit c1
        left <- rawModesLeftBehind c1
        left `shouldBe` []

    it "new-session creates and attaches to a fresh session" $
        withHat hatBin $ \h -> do
        -- First client autostarts the server on session $1.
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "echo one-$((1+1))\r"
        awaitScreen c1 "one-2"

        -- `hat new` from a shell must attach this terminal to a brand-new
        -- session with its own fresh shell, not create-and-exit.
        c2 <- startClientArgs h ["new"]
        awaitScreen c2 "$"
        typeInto c2 "echo two-$((2+2))\r"
        awaitScreen c2 "two-4"

        -- Two independent sessions now exist.
        out <- ctlOut h ["list-sessions"]
        length (lines out) `shouldBe` 2

        -- The new session is a distinct shell: c1's marker is not on c2.
        scr2 <- screenText c2
        T.isInfixOf "one-2" scr2 `shouldBe` False

        typeInto c2 "exit\r"
        _ <- awaitExit c2
        typeInto c1 "exit\r"
        _ <- awaitExit c1
        pure ()

    it "refuses to nest: attaching from inside a pane errors out" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"

        -- hat-in-hat: attaching from a pane of the same server would
        -- render the session inside itself (input/draw recursion). The
        -- pane's $TMUX marks the nesting; attach must refuse, like tmux.
        typeInto c1 (B8.pack (h.bin <> " -S " <> h.sock <> " attach\r"))
        awaitScreen c1 "nested with care"

        -- The shell survives the refusal, and control commands from
        -- inside a pane still work (scripts depend on them).
        typeInto c1 (B8.pack (h.bin <> " -S " <> h.sock <> " list-sessions\r"))
        awaitWith "list-sessions output" (\d -> do
            t <- screenText d
            pure ("windows" `T.isInfixOf` t)) c1

    it "attach -t attaches this terminal to the named session" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"

        -- A detached second session named "work".
        _ <- hatCtl h ["new-session", "-d", "-s", "work"]
        out <- ctlOut h ["list-sessions"]
        length (lines out) `shouldBe` 2

        -- Attaching a fresh terminal explicitly to "work" must render it.
        c2 <- startClientArgs h ["attach", "-t", "work"]
        awaitScreen c2 "$"
        typeInto c2 "echo work-$((3+4))\r"
        awaitScreen c2 "work-7"

        typeInto c2 "\x02\&d"
        status2 <- awaitExit c2
        status2 `shouldBe` Exited ExitSuccess
        t2 <- readIORef c2.transcript
        t2 `shouldSatisfy` B8.isInfixOf "[detached]"

        typeInto c1 "exit\r"
        _ <- awaitExit c1
        pure ()

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

    it "mirrors the session/window/pane tree into the SQLite store" $
        withHatPersist hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"

        -- A second window with a live shell. The \& keeps the hex escape
        -- from swallowing the 'c' (\x02c would parse as one char, 0x2C).
        typeInto c1 "\x02\&c"
        typeInto c1 "echo win2-marker\r"
        awaitScreen c1 "win2-marker"

        -- Split that window in two.
        typeInto c1 "\x02%"
        awaitScreen c1 "\x2502"

        -- kill-server captures the tree synchronously before teardown.
        _ <- hatCtl h ["kill-server"]
        snap <- Persist.withStore (storeOf h) Persist.loadSnapshot
        let wins = concatMap (.windows) snap.sessions
            paneCounts = List.sort (map (length . (.panes)) wins)
            cwds = concatMap (.panes) wins
        length snap.sessions `shouldBe` 1
        length wins `shouldBe` 2
        paneCounts `shouldBe` [1, 2]
        all (not . T.null . (.cwd)) cwds `shouldBe` True

    it "restores a saved session tree when the server starts" $
        withHatPersist hatBin $ \h -> do
        -- Seed the store the server reads on startup: two windows, the
        -- second split into two panes.
        let rect = sizeRect (Size { rows = 24, cols = 80 })
            lay1 = emitLayout rect (Leaf (PaneId 0))
            lay2 = emitLayout rect
                (Split LeftRight (1 % 2) (Leaf (PaneId 0)) (Leaf (PaneId 1)))
            snap = Snapshot
                { lastActiveSession = Nothing, sessions =
                    [ SessionSnap
                        { name = "restored", startCwd = "/tmp", currentIx = 0
                        , lastIx = Nothing
                        , windows =
                            [ WindowSnap { ix = 0, name = "one", layout = lay1
                                , active = 0, lastActive = Nothing
                                , panes = [PaneSnap { cwd = "/tmp", command = Nothing }] }
                            , WindowSnap { ix = 1, name = "two", layout = lay2
                                , active = 0, lastActive = Nothing
                                , panes = [PaneSnap { cwd = "/tmp", command = Nothing }
                                          , PaneSnap { cwd = "/tmp", command = Nothing }] }
                            ] } ] }
        createDirectoryIfMissing True (takeDirectory (storeOf h))
        Persist.withStore (storeOf h) $ \c -> Persist.saveSnapshot c snap

        -- Attaching autostarts the server, which restores before we attach.
        c1 <- startClient h
        awaitScreen c1 "$"

        -- The restored tree is present: two windows, three panes total.
        wl <- ctlOut h ["list-windows", "-a"]
        length (lines wl) `shouldBe` 2
        pl <- ctlOut h ["list-panes", "-a"]
        length (lines pl) `shouldBe` 3

    it "forgets the saved tree when the last window exits (pristine restart)" $
        withHatPersist hatBin $ \h -> do
        -- Seed a saved tree so the autostarted server restores it.
        let rect = sizeRect (Size { rows = 24, cols = 80 })
            lay = emitLayout rect (Leaf (PaneId 0))
            snap = Snapshot
                { lastActiveSession = Nothing, sessions =
                    [ SessionSnap
                        { name = "stale", startCwd = "/tmp", currentIx = 0
                        , lastIx = Nothing
                        , windows =
                            [ WindowSnap
                                { ix = 0, name = "one", layout = lay
                                , active = 0, lastActive = Nothing
                                , panes = [PaneSnap { cwd = "/tmp", command = Nothing }] }
                            ] } ] }
        createDirectoryIfMissing True (takeDirectory (storeOf h))
        Persist.withStore (storeOf h) $ \c -> Persist.saveSnapshot c snap

        c1 <- startClient h
        awaitScreen c1 "$"
        names1 <- ctlOut h ["list-sessions", "-F", "#{session_name}"]
        names1 `shouldSatisfy` List.isInfixOf "stale"

        -- Exiting the last shell closes the last window, the session and
        -- the server. Unlike kill-server, this must drop the saved tree:
        -- the next start comes up pristine instead of resurrecting it.
        typeInto c1 "exit\r"
        _ <- awaitExit c1
        gone <- pollServerGone h.sock 50
        unless gone $ expectationFailure "server did not exit"

        c2 <- startClient h
        awaitScreen c2 "$"
        names2 <- ctlOut h ["list-sessions", "-F", "#{session_name}"]
        names2 `shouldNotSatisfy` List.isInfixOf "stale"
        wl <- ctlOut h ["list-windows", "-a", "-F", "#{window_name}"]
        length (lines wl) `shouldBe` 1

    it "restores a whitelisted running command (vim), not a bare shell" $
        withHatPersist hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "vim -u NONE -i NONE\r"
        awaitScreen c1 "VIM - Vi IMproved"

        -- kill-server saves the tree; vim is the pane's foreground command
        -- and is on the default restore whitelist.
        _ <- ctlOut h ["kill-server"]
        _ <- awaitExit c1
        gone <- pollServerGone h.sock 50
        unless gone $ expectationFailure "server did not die"

        -- Restart: the restored pane comes back running vim, not a shell.
        -- vim's empty-buffer '~' filler proves it (a restored shell shows
        -- the '$ ' prompt and never a '~'); the intro banner is skipped
        -- because restore re-runs bare `vim`, whose vimrc may suppress it.
        c2 <- startClient h
        awaitScreen c2 "~"

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

    it "default prefix w opens choose-tree" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        typeInto c1 "echo win0-marker\r"
        awaitScreen c1 "win0-marker"
        typeInto c1 "\x02\&c"               -- new window 1
        awaitScreen c1 "1:sh*"
        typeInto c1 "echo win1-marker\r"
        awaitScreen c1 "win1-marker"
        typeInto c1 "\x02\&0"               -- back to window 0
        awaitScreen c1 "win0-marker"
        -- prefix w opens choose-tree with no config binding at all. The
        -- preview beside the list shows the highlighted node's live pane —
        -- win0-marker, which a full-screen list would otherwise have erased.
        typeInto c1 "\x02w"
        awaitWith "chooser open with a live preview of window 0" (\d -> do
            t <- screenText d
            pure ("choose a window" `T.isInfixOf` t
                  && "win0-marker" `T.isInfixOf` t)) c1
        -- Navigate to the window-1 row and select it.
        typeInto c1 "/1"
        typeInto c1 "\r"
        awaitScreen c1 "win1-marker"

    it "choose-tree previews a split window with both of its panes" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        -- 'clear' pins each marker at the top-left of its pane, so it shows
        -- even in the composited (clipped) preview thumbnail.
        typeInto c1 "clear; echo LEFTMARK\r"
        awaitScreen c1 "LEFTMARK"
        typeInto c1 "\x02%"                   -- split -h; the right pane is active
        awaitScreen c1 "\x2502"
        typeInto c1 "clear; echo RIGHTMARK\r"
        awaitScreen c1 "RIGHTMARK"
        -- prefix w opens the zoomed chooser, hiding the live panes. Its
        -- preview composites the whole current window, so BOTH panes of the
        -- split appear beside the list — not just the active one, which is
        -- all the old single-pane preview could show.
        typeInto c1 "\x02w"
        awaitWith "preview shows both panes of the split" (\d -> do
            t <- screenText d
            pure ("choose a window" `T.isInfixOf` t
                  && "LEFTMARK" `T.isInfixOf` t
                  && "RIGHTMARK" `T.isInfixOf` t)) c1

    it "choose-tree previews a session as a stack of its windows' thumbnails" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        typeInto c1 "clear; echo ALPHA\r"     -- window 0's marker, pinned top-left
        awaitScreen c1 "ALPHA"
        typeInto c1 "\x02\&c"                  -- new window 1
        awaitScreen c1 "1:sh*"
        typeInto c1 "clear; echo BETA\r"       -- window 1's marker
        awaitScreen c1 "BETA"
        typeInto c1 "\x02w"                    -- open the zoomed tree chooser
        typeInto c1 "g"                        -- jump to the session row (top)
        -- The session row stacks a thumbnail per window, so a marker from
        -- BOTH windows shows — not just the current window's active pane.
        awaitWith "session preview stacks both windows" (\d -> do
            t <- screenText d
            pure ("ALPHA" `T.isInfixOf` t && "BETA" `T.isInfixOf` t)) c1

    it "choose-tree without -Z draws in the active pane, sparing the other" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "bind e choose-tree -w\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh*"
        typeInto c1 "echo left-marker\r"
        awaitScreen c1 "left-marker"
        typeInto c1 "\x02%"                  -- split -h; right pane is active
        awaitScreen c1 "\x2502"
        typeInto c1 "echo right-marker\r"
        awaitScreen c1 "right-marker"
        -- prefix e opens a non-zoomed chooser in the right (active) pane.
        -- The left pane's content must survive beside it.  (\& terminates
        -- the \x02 hex escape so the 'e' is a separate key, not 0x2e.)
        typeInto c1 "\x02\&e"
        awaitWith "chooser confined to the active pane" (\d -> do
            t <- screenText d
            pure ("choose a window" `T.isInfixOf` t
                  && "left-marker" `T.isInfixOf` t)) c1

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
            -- Keep window names stable ('sh') so the flag assertions below
            -- read "0:sh-#" regardless of the foreground command.
            , "set -g automatic-rename off"
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
        awaitForeground h "cat"              -- cat is now the foreground program
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
        typeInto c1 "cat -v\r"               -- render control chars visibly
        -- cat runs after printf, so its foreground presence proves ?1004 is
        -- already enabled on the pane.
        awaitForeground h "cat"
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
        -- The third pane renders a second vertical border to the client; wait
        -- for that (an event on the render stream), not a poll.
        awaitWith "three panes side by side" (\d -> do
            scr <- Emu.snapshot d.screen
            let cols = [ c | row <- V.toList scr.cells
                           , (c, cell) <- zip [0 :: Int ..] (V.toList row)
                           , cell.text == "\x2502" ]
            pure (length (List.nub cols) >= 2)) c1
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

    it "default prefix . renumbers the current window, keeping focus" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        typeInto c1 "\x02\&c"                -- window 1, now current
        awaitScreen c1 "1:sh*"
        typeInto c1 "\x02."                  -- prefix . opens the move prompt
        awaitPromptOpen c1 "1:sh*"
        typeInto c1 "5\r"                    -- move current window to index 5
        awaitScreen c1 "5:sh*"               -- renumbered, still current

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
        withHatPersist hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        -- Build a tree with the targeted resurrect primitives: rename
        -- window 0 by target, add a named window 1 by target.
        _ <- ctlOut h ["rename-window", "-t", "0", "built0"]
        awaitScreen c1 "0:built0*"
        _ <- ctlOut h ["new-window", "-d", "-t", "1", "-n", "editor"]
        awaitScreen c1 "1:editor"

        -- Kill the server; the tree is saved automatically on the way out.
        _ <- ctlOut h ["kill-server"]
        _ <- awaitExit c1
        gone <- pollServerGone h.sock 50
        unless gone $ expectationFailure "server did not die"

        -- Restart: a fresh client autostarts a new server, which restores
        -- the saved tree before we attach -- no manual replay needed.
        c2 <- startClient h
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

    it "automatic-rename follows the pane's foreground command" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g automatic-rename on\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh*"
        -- A foreground program becomes the window name.
        typeInto c1 "cat\r"
        awaitScreen c1 "0:cat*"
        -- Ending it (Ctrl-D) returns the name to the shell.
        typeInto c1 "\x04"
        awaitScreen c1 "0:sh*"
        typeInto c1 "exit\r"
        _ <- awaitExit c1
        pure ()

    it "names an automatic-rename window after the Nix wrapper, not .name-wrapped" $
        withHat hatBin $ \h -> do
        -- A NixOS-style wrapper pair: public `vimish` execs the real
        -- binary `.vimish-wrapped`, passing the public name along in
        -- argv[0]. The real binary is a copy of bash (the coreutils
        -- multi-call binary would dispatch on the renamed argv[0]) that
        -- blocks on stdin like an editor would.
        -- cp, not copyFile, for the same ETXTBSY reason as
        -- 'writeExecutable'.
        let bin = h.home </> "bin"
            wrapped = bin </> ".vimish-wrapped"
        createDirectoryIfMissing True bin
        P.callProcess "cp" ["/bin/sh", wrapped]
        writeExecutable (bin </> "vimish") $ unlines
            [ "#!/bin/sh"
            , "exec -a \"$0\" " <> wrapped <> " -c 'read line'"
            ]
        writeFile (h.home <> "/hat.conf") "set -g automatic-rename on\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh*"
        typeInto c1 (B8.pack ("$HOME/bin/vimish\r"))
        -- The window takes the wrapper's public name, like tmux would.
        awaitScreen c1 "0:vimish*"
        typeInto c1 "\x04"                   -- Ctrl-D ends the read
        awaitScreen c1 "0:sh*"

    it "set-titles composes hat: [window:] path: program, most specific kept" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g set-titles on\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh*"
        -- The default session name ("0") is numeric noise and the
        -- auto-renamed window would just repeat the program: both are
        -- skipped, leaving cwd and foreground program.
        awaitWith "initial composed title" (\d -> do
            t <- readIORef d.transcript
            pure ("\ESC]0;hat: /tmp: sh\BEL" `B8.isInfixOf` t)) c1
        typeInto c1 "cat\r"
        awaitWith "title follows the foreground program" (\d -> do
            t <- readIORef d.transcript
            pure ("\ESC]0;hat: /tmp: cat\BEL" `B8.isInfixOf` t)) c1
        typeInto c1 "\x04"                   -- end cat
        -- A program-set (OSC) title is the most specific component and
        -- replaces the bare program name.
        typeInto c1 "printf '\\033]2;MYAPP\\007'\r"
        awaitWith "pane's own title replaces the program" (\d -> do
            t <- readIORef d.transcript
            pure ("\ESC]0;hat: /tmp: MYAPP\BEL" `B8.isInfixOf` t)) c1
        -- A pinned (explicit) window name is signal again.
        _ <- ctlOut h ["rename-window", "pinned"]
        awaitWith "pinned window name reappears" (\d -> do
            t <- readIORef d.transcript
            pure ("\ESC]0;hat: pinned: /tmp: MYAPP\BEL" `B8.isInfixOf` t)) c1

    it "an explicit rename-window pins the name and stops auto-rename" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g automatic-rename on\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh*"
        _ <- ctlOut h ["rename-window", "pinned"]
        awaitScreen c1 "0:pinned*"
        -- Run cat in the pinned window, then open a second, UNPINNED window
        -- and run cat there too.
        typeInto c1 "cat\r"
        typeInto c1 "\x02\&c"            -- new-window (window 1, auto-rename on)
        awaitScreen c1 "1:sh*"
        typeInto c1 "cat\r"
        -- The unpinned window auto-renaming to cat proves the rename poll has
        -- run; in that same pass the pinned window was left alone, so its
        -- staying "pinned" is decidable now without waiting on a clock.
        awaitScreen c1 "1:cat*"
        nm <- T.pack <$> ctlOut h ["list-windows", "-F", "#{window_index}:#{window_name}"]
        nm `shouldSatisfy`
            (\t -> "0:pinned" `T.isInfixOf` t && not ("0:cat" `T.isInfixOf` t))

    it "follows the desktop color scheme (gsettings) and sources the dark config" $
        withHat hatBin $ \h -> do
        -- A fake gsettings on the server's PATH: `get` reports light,
        -- `monitor` tails a feed file this test appends flips to.
        let bin = h.home </> "bin"
            feed = h.home </> "scheme-feed"
        createDirectoryIfMissing True bin
        writeFile feed ""
        writeExecutable (bin </> "gsettings") $ unlines
            [ "#!/bin/sh"
            , "case \"$1\" in"
            , "get) echo \"'prefer-light'\" ;;"
            , "monitor) exec tail -f \"$HOME/scheme-feed\" ;;"
            , "esac"
            ]
        writeFile (h.home </> "dark.conf") "set -g status-left 'DARKMODE '\n"
        writeFile (h.home </> "hat.conf") $
            "set -g @color-scheme-dark " <> (h.home </> "dark.conf") <> "\n"
        c1 <- startClientEnv h [("PATH", bin <> ":" <> testPath)]
            ["-f", h.home </> "hat.conf"]
        awaitScreen c1 "0:sh*"

        -- The initial preference lands as #{color_scheme} once the
        -- watcher has run gsettings get (it waits out the config load,
        -- so poll — there is no render to sync on for "light").
        let waitScheme n = do
                out <- ctlOut h ["display-message", "-p", "#{color_scheme}"]
                if lines out == ["light"] || n <= (0 :: Int)
                    then lines out `shouldBe` ["light"]
                    else threadDelay 10000 >> waitScheme (n - 1)
        waitScheme 500

        -- The desktop flips to dark: the watcher sources dark.conf and
        -- the status line shows it.
        appendFile feed "color-scheme: 'prefer-dark'\n"
        awaitScreen c1 "DARKMODE"
        out <- ctlOut h ["display-message", "-p", "#{color_scheme}"]
        lines out `shouldBe` ["dark"]

    it "defaults hat's chrome to the dark palette when the desktop is dark" $
        withHat hatBin $ \h -> do
        let bin = h.home </> "bin"
        createDirectoryIfMissing True bin
        writeExecutable (bin </> "gsettings") $ unlines
            [ "#!/bin/sh"
            , "case \"$1\" in"
            , "get) echo \"'prefer-dark'\" ;;"
            , "monitor) exec tail -f /dev/null ;;"
            , "esac"
            ]
        c1 <- startClientEnv h [("PATH", bin <> ":" <> testPath)] []
        awaitScreen c1 "0:sh*"
        -- No styles configured: the status bar restyles itself to the
        -- dark palette (deep green 22), not tmux's classic bright green.
        awaitWith "status bar in the dark palette" (\d -> do
            scr <- Emu.snapshot d.screen
            pure $ case scr.cells V.!? 23 of
                Just row -> any
                    (\cell -> cell.style.bg == Indexed 22)
                    (V.toList row)
                Nothing -> False) c1

    it "renames the session via prefix $ (pre-filled prompt)" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        -- The default session name is its numeric id; status-left shows [0].
        awaitScreen c1 "[0]"
        -- prefix $ opens a rename-session prompt pre-filled with #S.
        typeInto c1 "\x02$"
        awaitScreen c1 "(rename-session) 0"
        -- Replace the pre-filled name; status-left reflects the new one.
        typeInto c1 "\DELwork\r"
        awaitScreen c1 "[work]"
        typeInto c1 "exit\r"
        _ <- awaitExit c1
        pure ()

    it "rename-session targets by -t and rejects a duplicate name" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        _ <- ctlOut h ["new-session", "-d", "-s", "other"]
        -- Renaming session 0 onto an existing name is refused.
        (_, _, err) <- hatCtl h ["rename-session", "-t", "0", "other"]
        err `shouldSatisfy` (\e -> "duplicate" `List.isInfixOf` e)
        -- -t names the session to rename (not the client's current one).
        _ <- ctlOut h ["rename-session", "-t", "other", "renamed"]
        names <- T.pack <$> ctlOut h ["list-sessions", "-F", "#{session_name}"]
        names `shouldSatisfy`
            (\t -> "renamed" `T.isInfixOf` t && not ("other" `T.isInfixOf` t))
        typeInto c1 "exit\r"
        _ <- awaitExit c1
        pure ()

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
        -- q exits copy mode; the pane takes input again. Keys route one at a
        -- time, so the whole sequence is correct even in a single chunk.
        typeInto c1 "\x02[zapzap42q"
        typeInto c1 "echo after-copy-zone\r"
        awaitScreen c1 "after-copy-zone"

        -- after-copy-zone came after the swallowed marker in the same
        -- input stream, so its presence proves the marker was dropped.
        scr <- screenText c1
        scr `shouldNotSatisfy` T.isInfixOf "zapzap42"

    it "vi copy-mode: g/G jump history ends, C-b pages up" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g mode-keys vi\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "$"
        -- Distinct top/bottom markers, on their own command lines so the
        -- echoed input doesn't carry the other marker.
        typeInto c1 "echo TOPmark\r"
        awaitScreen c1 "TOPmark"
        typeInto c1 "seq 1 200\r"
        awaitScreen c1 "199"
        typeInto c1 "echo BOTmark\r"
        awaitScreen c1 "BOTmark"
        -- Enter copy mode with the vi table; g jumps to the top of history.
        typeInto c1 "\x02[g"
        awaitScreen c1 "TOPmark"
        -- G returns to the bottom of history.
        typeInto c1 "G"
        awaitScreen c1 "BOTmark"
        -- C-b (page-up) scrolls a full screen up off the bottom.
        typeInto c1 "\x02"
        awaitWith "page-up scrolls off the bottom"
            (\d -> not . T.isInfixOf "BOTmark" <$> screenText d) c1
        -- q leaves copy mode; exit then reaches the shell (per-key routing).
        typeInto c1 "qexit\r"
        _ <- awaitExit c1
        pure ()

    it "vi copy-mode: a [count] repeats a motion (4l selects five cells)" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g mode-keys vi\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "$"
        -- A line of digits as output; the prompt sits directly below it.
        printDigitLine c1
        baseline <- reverseCellCount c1        -- no highlight yet
        -- k: up onto the digit line. 0: start-of-line. v: begin selection.
        -- 4l: cursor-right x4 (the [count]). The selection then covers five
        -- cells (0..4 inclusive); a broken count would move l only once and
        -- highlight two. Counting highlighted cells is a direct, render-driven
        -- check with no timing.
        typeInto c1 "\x02[k0v4l"
        awaitWith "count selects exactly five cells"
            (\d -> (== baseline + 5) <$> reverseCellCount d) c1
        typeInto c1 "qexit\r"
        _ <- awaitExit c1
        pure ()

    it "vi copy-mode: / searches the scrollback and jumps to a match" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g mode-keys vi\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "$"
        typeInto c1 "echo FINDMEHERE\r"      -- unique marker, near the top
        awaitScreen c1 "FINDMEHERE"
        typeInto c1 "seq 1 200\r"            -- push it up into scrollback
        awaitScreen c1 "199"                 -- seq output (not the echoed cmd)
        typeInto c1 "\x02["
        awaitScreen c1 "[0/"
        -- The marker has scrolled off the bottom view.
        scr <- screenText c1
        scr `shouldNotSatisfy` T.isInfixOf "FINDMEHERE"
        -- / opens the search prompt (its own chunk, like the rename tests);
        -- the query submits on Enter and the view jumps to the match.
        typeInto c1 "/"
        awaitScreen c1 "(search down)"
        typeInto c1 "FINDMEHERE\r"
        awaitScreen c1 "FINDMEHERE"

    it "vi copy-mode: f<char> captures the next key and jumps to it" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g mode-keys vi\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "$"
        printDigitLine c1
        baseline <- reverseCellCount c1
        -- k: onto the digit line. 0: start-of-line. v: begin selection.
        -- f5: arm char-search, then '5' is captured as the target, moving the
        -- cursor to column 5. The selection then spans 0..5 = six cells; if the
        -- capture were broken, '5' would fall through and only one cell selects.
        typeInto c1 "\x02[k0vf5"
        lastCount <- newIORef (-1 :: Int)
        awaitWith "f jumped through the 5 (six cells)"
            (\d -> do
                n <- reverseCellCount d
                writeIORef lastCount n
                pure (n == baseline + 6)) c1
            `catch` \(e :: SomeException) -> do
                n <- readIORef lastCount
                expectationFailure $
                    "selection count: baseline " <> show baseline
                    <> ", last seen " <> show n <> "\n" <> show e

    it "vi copy-mode: V selects the whole line" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g mode-keys vi\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "$"
        typeInto c1 "echo LINEWISEMARK"      -- unentered; cursor on this line
        awaitScreen c1 "LINEWISEMARK"
        -- Enter copy mode and select-line. Line-wise selection highlights the
        -- whole row width, far more cells than a char/word selection would.
        typeInto c1 "\x02[V"
        awaitWith "whole line highlighted"
            (\d -> (>= 40) <$> reverseCellCount d) c1

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
        typeInto c1 "\x02[\x01 \x05"
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
        awaitForeground h "cat"
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
        typeInto c1 "\x02[\x01 \x05"     -- enter; C-a, Space (select), C-e
        awaitWith "selection highlighted" (\d ->
            (> baseline) <$> reverseCellCount d) c1

        -- copy-pipe runs the shell command synchronously (readCreateProcess),
        -- so the file is fully written by the time this control call returns.
        let outPath = h.home <> "/piped.txt"
        _ <- ctlOut h ["send-keys", "-X", "copy-pipe", "cat > " <> outPath]
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
        _ <- awaitFile logPath (List.isInfixOf "PIPEPANEMARKER")

        -- No-arg pipe-pane stops the pipe (synchronously); later output is
        -- not captured. The pane taps output to the pipe BEFORE the emulator
        -- renders it, so once AFTERSTOPMARKER is on screen, a still-live pipe
        -- would already have written it — its absence is thus decidable now.
        _ <- ctlOut h ["pipe-pane"]
        typeInto c1 "echo AFTERSTOPMARKER\r"
        awaitScreen c1 "AFTERSTOPMARKER"
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
        withHatOn hatBin False "fresh/subdir/socket" $ \h -> do
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
        awaitForeground h "cat"
        -- Type without Enter: canonical mode echoes each char exactly once.
        -- A doubled echo would render "aabbccxxyyzz", which does NOT contain
        -- "abcxyz", so awaiting "abcxyz" already proves the echo was single.
        typeInto c1 "abcxyz"
        awaitScreen c1 "abcxyz"
        scr <- screenText c1
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
        awaitForeground h "cat"
        typeInto c1 "hello world\r"
        awaitScreen c1 "hello world"
        -- Ctrl-D on an empty line EOFs cat; the shell then evaluates
        -- arithmetic that cat could never produce by echo alone. "done-42"
        -- can only appear once cat has exited, so it is its own sync.
        typeInto c1 "\x04"
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
