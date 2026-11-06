-- | End-to-end tests driving the real @hat@ binary through a pty.
module Hat.IntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, concurrently, link)
import Control.Concurrent.STM
    (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry,
     writeTVar)
import Control.Exception
    (IOException, SomeException, bracket, catch, evaluate, finally, throwIO,
     try)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Char (chr)
import Data.List qualified as List
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.IORef
import System.Directory
    (createDirectoryIfMissing, createFileLink, doesFileExist, getFileSize,
     listDirectory, removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (Handle, SeekMode (..), hClose, hSetBuffering, BufferMode (..))
import System.Posix.Files (readSymbolicLink)
import System.Posix.IO (closeFd, fdToHandle, handleToFd)
import System.Posix.IO qualified as PIO
import System.Posix.Process (ProcessStatus (..))
import System.Posix.Signals (Signal, nullSignal, sigKILL, sigTERM, signalProcess)
import System.Mem
import System.Posix.Temp (mkdtemp)
import System.Posix.Terminal
    ( TerminalMode (..), TerminalState (Immediately), getTerminalAttributes
    , openPseudoTerminal, setTerminalAttributes, terminalMode )
import System.Process (readProcess)
import System.Process qualified as P
import System.Timeout (timeout)
import Test.HUnit.Lang
    (FailureReason (..), HUnitFailure (..), formatFailureReason)
import Test.Hspec
import Text.Read (readMaybe)

import Data.Text qualified as T
import Data.Vector qualified as V
import Network.Socket qualified as N

import Data.Maybe (fromMaybe, isJust, mapMaybe)
import System.FilePath (takeDirectory, takeFileName, (</>))
import Hat.Geometry
import Hat.Model.Ids (PaneId (..))
import Hat.Model.PaneKind (PaneKind (..))
import Hat.Server.Persist (PaneSnap (..), SessionSnap (..), Snapshot (..), WindowSnap (..))
import Hat.Server.Persist qualified as Persist
import Hat.Term.Pty (setWinsize)
import Hat.TestSupport (bashPath)
import Hat.Server.Layout (fitLayout, singlePane)
import Hat.Server.LayoutString (emitLayout)
import Hat.Transport.Socket (connectTo)
import Hat.Transport.Wire qualified as W
import Hat.Term.Cell (Cell (..), Color (..), Style (..))
import Hat.Term.Emulator qualified as Emu

-- An isolated hat instance for one test: a private HOME (so @hat@ never
-- reads any ambient config) and a private socket. See 'withHat'.
data Hat = Hat
    { bin     :: FilePath
    , home    :: FilePath
    , sock    :: FilePath
    , persist :: Bool   -- ^ whether the server keeps its SQLite store
    , keep    :: IORef [Driver]  -- ^ every spawned driver; see 'teardown'
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

-- PATH for spawned shells and the hat binary. Prepends the flake's
-- test-only tools (vim, htop via @HAT_TEST_TOOLS@) so the suite spawns
-- pinned binaries even though they are kept off the interactive PATH
-- (bug d9); falls back to the ambient PATH for everything else.
testPath :: IO String
testPath = do
    ambient <- fromMaybe "" <$> lookupEnv "PATH"
    tools <- lookupEnv "HAT_TEST_TOOLS"
    pure $ maybe ambient (<> (':' : ambient)) tools

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
    keepRef <- newIORef []
    let hatFor dir = Hat
            { bin = hatBin, home = dir
            , sock = dir <> "/" <> sockRel, persist = persistOn
            , keep = keepRef }
    bracket (mkdtemp "/tmp/hat-test-") (teardown . hatFor)
        (\dir -> action (hatFor dir) `catch` rethrowWithLog (hatFor dir))

-- | A failing test's assertion, annotated with the tail of its server's
-- event log before teardown deletes it -- the only trace a CI failure
-- leaves.
rethrowWithLog :: Hat -> HUnitFailure -> IO a
rethrowWithLog h (HUnitFailure loc reason) = do
    events <- (readFile (h.home </> "server.log") >>= \s -> s <$ evaluate (length s))
        `catch` \(_ :: SomeException) -> pure "(no server log)"
    -- From the last reload when there is one (the epoch failures hide in),
    -- middle elided; plain tail otherwise. Poll chatter is dropped.
    let noise l = List.isInfixOf "gsettings" l
            || List.isInfixOf "\"command\":\"list-clients" l
        interesting = filter (not . noise) (lines events)
        epoch = List.dropWhileEnd (not . List.isInfixOf "ServerReloading")
            interesting
        sliced = case drop (length epoch - 1) interesting of
            [] -> lastN 25 interesting
            fromReload
                | length fromReload <= 45 -> fromReload
                | otherwise ->
                    take 30 fromReload <> ["..."] <> lastN 15 fromReload
        lastN n xs = drop (length xs - n) xs
    throwIO $ HUnitFailure loc $ Reason $
        formatFailureReason reason <> "\nserver log:\n" <> unlines sliced

-- Kill the server (harmless if already gone) and remove the temp dir.
teardown :: Hat -> IO ()
teardown h = do
    r <- timeout 5_000_000 $ hatCtl h ["kill-server"]
        `catch` \(_ :: SomeException) -> pure (ExitFailure 1, "", "")
    -- A wedged server can hold kill-server forever: reap it by its unique
    -- socket path instead, so a red run neither hangs the suite nor leaks
    -- a server. The path lives under this test's private tmpdir, so the
    -- match can never touch an unrelated hat.
    case r of
        Nothing -> signalPids sigTERM =<< serverPids h
        Just _ -> pure ()
    -- kill-server returns before the drain finishes, and a draining server
    -- stops accepting before it exits, so "unreachable" is not "gone". The
    -- poll gives a clean drain its chance; the unconditional -9 guarantees
    -- nothing survives the socket-path unlink below.
    _ <- pollServerGone h.sock 2000
    signalPids sigKILL =<< serverPids h
    -- A driver's pty master stays open for the whole test -- closed here,
    -- never by the collector mid-test (a finalized master hangs up its
    -- still-attached client).
    ds <- readIORef h.keep
    forM_ ds $ \d -> cancel d.pump >> hClose d.pty.master
        `catch` \(_ :: SomeException) -> pure ()
    removeDirectoryRecursive h.home
        `catch` \(_ :: SomeException) -> pure ()

-- Run a hat control command with the isolated HOME so it never resolves
-- the ambient user config (even when it autostarts a server).
-- Bounded, so a command a wedged server never answers fails the test (with
-- the server log) instead of hanging the whole suite.
hatCtl :: Hat -> [String] -> IO (ExitCode, String, String)
hatCtl h args = do
    path <- testPath
    r <- timeout 30_000_000 $ P.readCreateProcessWithExitCode
        (P.proc h.bin (["-S", h.sock] <> args))
            { P.env = Just ([("HOME", h.home), ("PATH", path)] <> persistEnv h)
            , P.close_fds = True }
        ""
    maybe (expectationFailure ("hat " <> unwords args <> ": no reply in 30s")
            >> pure (ExitFailure 1, "", ""))
          pure r

-- Run a hat control command feeding it stdin, returning its raw stdout.
hatBytes :: Hat -> [String] -> B.ByteString -> IO (ExitCode, B.ByteString)
hatBytes h args input = do
    path <- testPath
    r <- timeout 30_000_000 $ do
        (Just hin, Just hout, _, ph) <- P.createProcess
            (P.proc h.bin (["-S", h.sock] <> args))
                { P.env = Just ([("HOME", h.home), ("PATH", path)] <> persistEnv h)
                , P.close_fds = True
                , P.std_in = P.CreatePipe, P.std_out = P.CreatePipe }
        B.hPut hin input >> hClose hin
        out <- B.hGetContents hout
        code <- P.waitForProcess ph
        pure (code, out)
    maybe (expectationFailure ("hat " <> unwords args <> ": no reply in 30s")
            >> pure (ExitFailure 1, ""))
          pure r

-- | 'hatCtl' with argv, stdout and stderr as bytes, in the C locale. The
-- words reach @hat@ through a shell's @printf@, so each byte arrives as it is.
hatRaw :: Hat -> [B.ByteString] -> IO (ExitCode, B.ByteString, B.ByteString)
hatRaw h args = do
    path <- testPath
    sh <- bashPath
    let octal w = ['\\', d (w `div` 64), d (w `div` 8 `mod` 8), d (w `mod` 8)]
        d n = chr (fromEnum '0' + fromIntegral n)
        word b = "\"$(printf '" <> concatMap octal (B.unpack b) <> "')\""
        script = unwords (map word ([B8.pack h.bin, "-S", B8.pack h.sock] <> args))
    r <- timeout 30_000_000 $ do
        (Just hin, Just hout, Just herr, ph) <- P.createProcess
            (P.proc sh ["-c", script])
                { P.env = Just ([("HOME", h.home), ("PATH", path)] <> persistEnv h)
                , P.close_fds = True
                , P.std_in = P.CreatePipe, P.std_out = P.CreatePipe
                , P.std_err = P.CreatePipe }
        hClose hin
        (out, err) <- concurrently (B.hGetContents hout) (B.hGetContents herr)
        code <- P.waitForProcess ph
        pure (code, out, err)
    maybe (expectationFailure ("hat " <> show args <> ": no reply in 30s")
            >> pure (ExitFailure 1, "", ""))
          pure r

-- Stdout of a hat control command.
ctlOut :: Hat -> [String] -> IO String
ctlOut h args = (\(_, out, _) -> out) <$> hatCtl h args

-- This Hat's server pid, found by the socket path in its argv — the same
-- handle 'teardown' reaps by.
serverPid :: Hat -> IO Int
serverPid h = serverPids h >>= \case
    (p : _) -> pure p
    [] -> fail ("no server running on " <> h.sock)

-- | The pids of every server process on this Hat's socket: each process
-- whose argv holds @--server@ then the socket path.
serverPids :: Hat -> IO [Int]
serverPids h = do
    entries <- listDirectory "/proc"
    filterM serves (mapMaybe readMaybe entries)
  where
    serves pid = (servesSock . B8.split '\0' <$> B.readFile (procFile pid "cmdline"))
        `catch` \(_ :: IOException) -> pure False
    servesSock argv = ["--server", B8.pack h.sock] `List.isInfixOf` argv

procFile :: Int -> FilePath -> FilePath
procFile pid file = "/proc" </> show pid </> file

-- | Send each process a signal, skipping any already gone.
signalPids :: Signal -> [Int] -> IO ()
signalPids sig = mapM_ $ \pid ->
    signalProcess sig (fromIntegral pid) `catch` \(_ :: IOException) -> pure ()

-- How many fds the server holds on one path.
fdsOn :: Hat -> FilePath -> IO Int
fdsOn h path = do
    pid <- serverPid h
    let dir = "/proc/" <> show pid <> "/fd"
    entries <- listDirectory dir
    links <- forM entries $ \e ->
        readSymbolicLink (dir </> e) `catch` \(_ :: IOException) -> pure ""
    pure (length (filter (== path) links))

-- The pid holding a write lock on the server's lock file, if any. Asks with
-- F_GETLK, which takes no lock, so this process's open and close are inert.
lockHolder :: Hat -> IO (Maybe Int)
lockHolder h = bracket
    (PIO.openFd (h.sock <> ".lock") PIO.ReadWrite PIO.defaultFileFlags { PIO.cloexec = True })
    closeFd
    (\fd -> fmap (fromIntegral . fst) <$> PIO.getLock fd (PIO.WriteLock, AbsoluteSeek, 0, 0))

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
    , fed :: TVar PtyFeed
    , pump :: Async ()  -- ^ see 'pumpPty'
    }

-- | How far a driver's pump has read its pty.
data PtyFeed
    = Feeding Int  -- ^ chunks ingested so far
    | PtyClosed    -- ^ the client side hung up; nothing more will come
    deriving Eq

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
    path <- testPath
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
    -- A Handle of its own, so the pump's blocked read never holds the lock
    -- 'typeInto' writes through.
    readH <- fdToHandle =<< PIO.dup masterFd
    (_, _, _, ph) <-
        P.createProcess (P.proc h.bin (["-S", h.sock] <> extra))
            { P.std_in  = P.UseHandle slaveH
            , P.std_out = P.UseHandle slaveH
            , P.std_err = P.UseHandle slaveH
            , P.close_fds = True
            , P.new_session = True
            , P.cwd = Just "/tmp"
            , P.env = Just $
                let defaults =
                        [ ("PATH", path)
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
    fedV <- newTVarIO (Feeding 0)
    pumpA <- async (pumpPty readH t emu fedV)
    link pumpA
    let d = Driver
            { pty = Client { master = masterH, proch = ph }
            , transcript = t
            , screen = emu
            , fed = fedV
            , pump = pumpA
            }
    d <$ modifyIORef' h.keep (d :)

-- | Drain the client's pty for as long as it is open, as a real terminal
-- does, into the transcript and the screen model.
pumpPty :: Handle -> IORef B8.ByteString -> Emu.Emulator -> TVar PtyFeed -> IO ()
pumpPty readH t emu fedV = loop `finally` hClose readH
  where
    loop = do
        -- Linux reports EIO at pty EOF, once the client and its slave are gone.
        r <- try (B.hGetSome readH 65536)
        case r of
            Right chunk | not (B8.null chunk) -> do
                modifyIORef' t (<> chunk)
                _ <- Emu.feed emu chunk
                atomically $ modifyTVar' fedV $ \case
                    Feeding n -> Feeding (n + 1)
                    PtyClosed -> PtyClosed
                loop
            Right _ -> closed
            Left (_ :: IOException) -> closed
    closed = atomically (writeTVar fedV PtyClosed)

writePty :: Client -> B8.ByteString -> IO ()
writePty c bs = B.hPut c.master bs `catch` \(_ :: IOException) -> pure ()

-- Block on the real child exit — no polling, no clock.
waitExit :: Client -> IO ProcessStatus
waitExit c = Exited <$> P.waitForProcess c.proch

screenText :: Driver -> IO T.Text
screenText d = do
    scr <- Emu.snapshot d.screen
    pure $ T.unlines [Emu.screenRowText scr r | r <- [0 .. 23]]

-- Recheck as the pty delivers until the given check passes; fail after 10s.
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
        seen <- readTVarIO d.fed
        ok <- check d
        unless ok $ case seen of
            PtyClosed -> do
                t <- readIORef d.transcript
                expectationFailure $ "pty closed while waiting for " <> what
                    <> "\ntranscript: " <> show t
            Feeding _ -> do
                atomically $ readTVar d.fed >>= \now -> when (now == seen) retry
                go

-- | An attached client whose re-exec resolves `hat` on PATH to THIS build,
-- so a restart's hop is deterministic (and never the system hat).
startRestartableClient :: Hat -> [(String, String)] -> IO Driver
startRestartableClient h extraEnv = do
    ambient <- testPath
    let bindir = h.home </> "bin"
    createDirectoryIfMissing True bindir
    createFileLink h.bin (bindir </> "hat")
    startClientEnv h (("PATH", bindir <> ":" <> ambient) : extraEnv) []

-- What a user would see on screen.
awaitScreen :: Driver -> T.Text -> IO ()
awaitScreen d needle = awaitWith (show needle) check d
  where
    check drv = T.isInfixOf needle <$> screenText drv

-- The command prompt replaces the status line: the bottom row is its bare
-- ":", which no status line draws alone.
awaitPromptOpen :: Driver -> IO ()
awaitPromptOpen = awaitWith "command prompt to open" $ \d -> do
    scr <- Emu.snapshot d.screen
    pure (T.strip (Emu.screenRowText scr 23) == ":")

-- | Poll a server-side observable to a bounded deadline (the pty-less
-- sibling of 'awaitWith').
awaitTrue :: String -> IO Bool -> IO ()
awaitTrue what check = do
    r <- timeout 10_000_000 go
    maybe (expectationFailure ("timed out waiting for " <> what)) pure r
  where
    go = do
        ok <- check
        unless ok (threadDelay 20_000 >> go)

awaitExit :: Driver -> IO ProcessStatus
awaitExit d = do
    r <- timeout 10_000_000 $ do
        atomically $ readTVar d.fed >>= \f -> unless (f == PtyClosed) retry
        waitExit d.pty
    maybe (expectationFailure "timed out waiting for exit"
            >> waitExit d.pty) pure r

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

-- Whether a process id is still alive, by the null signal (just an existence
-- probe). A reaped process's pid is gone, so this reads false.
pidAlive :: Int -> IO Bool
pidAlive pid = (True <$ signalProcess nullSignal (fromIntegral pid))
    `catch` \(_ :: IOException) -> pure False

-- Poll until a pid stops being alive; fail after ~5s. A pipe-pane child that
-- teardown reaped structurally disappears; one that leaked stays alive here.
awaitReaped :: Int -> IO ()
awaitReaped pid = go (500 :: Int)
  where
    go n = do
        alive <- pidAlive pid
        if not alive
            then pure ()
            else if n <= 0
                then expectationFailure $
                    "child " <> show pid <> " was not reaped"
                else threadDelay 10000 >> go (n - 1)

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

-- | Until the active pane's child @prog@ blocks reading its terminal. less
-- drops a SIGWINCH that lands between drawing its prompt and that read, so
-- a resize must wait for it.
awaitTtyRead :: Hat -> String -> IO ()
awaitTtyRead h prog = awaitTrue (prog <> " to block on its terminal") $ do
    shell <- ctlOut h ["display-message", "-p", "#{pane_pid}"]
    (_, kids, _) <- P.readProcessWithExitCode
        "pgrep" ["-x", "-P", takeWhile (/= '\n') shell, prog] ""
    case lines kids of
        [pid] -> ((== "wait_woken") <$> B.readFile ("/proc/" <> pid <> "/wchan"))
            `catch` \(_ :: IOException) -> pure False
        _ -> pure False

-- Count cells drawn with the reverse-video attribute (how a copy-mode
-- selection renders).
reverseCellCount :: Driver -> IO Int
reverseCellCount d = do
    scr <- Emu.snapshot d.screen
    pure $ length
        [ () | row <- V.toList scr.cells, cell <- V.toList row
             , let Style { reverse = r } = cell.style, r ]

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
    -- Build and locate as one step, so the tested binary is fresh and the
    -- two agree on a builddir.
    hatBin <- runIO $ do
        P.callProcess "cabal" ["build", "-v0", "exe:hat"]
        init <$> readProcess "cabal" ["list-bin", "hat"] ""

    it "attaches, survives detach/reattach, and shuts down cleanly, leaving the terminal cooked" $
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
        rawModesLeftBehind c1 `shouldReturn` []

        -- Server must still be alive.
        alive <- serverUp h.sock
        alive `shouldBe` True

        -- Reattach: the redrawn screen still shows our marker.
        c2 <- startClient h
        awaitScreen c2 "hat-integration-42"

        -- Ending the shell ends the session, the client, and the server.
        typeInto c2 "exit\r"
        status2 <- awaitExit c2
        status2 `shouldBe` Exited ExitSuccess
        t2 <- readIORef c2.transcript
        t2 `shouldSatisfy` B8.isInfixOf "[exited]"
        rawModesLeftBehind c2 `shouldReturn` []

        gone <- pollServerGone h.sock 5000
        unless gone $ expectationFailure "server did not exit"

    it "exit-unattached ends a server left with only detached sessions" $
        withHat hatBin $ \h -> do
        _ <- hatCtl h ["new-session", "-d", "sleep 300"]
        _ <- hatCtl h ["set-option", "-g", "exit-unattached", "on"]
        gone <- pollServerGone h.sock 5000
        unless gone $ expectationFailure "unattached server did not exit"

    -- A pane whose program ignores SIGHUP (and keeps the pty slave open) gives
    -- the reader thread no EOF, so a teardown that closes the master Handle
    -- while the reader still blocks in it deadlocks. kill-server must instead
    -- interrupt the reader and complete within a bounded deadline. A kill
    -- escalates to SIGKILL for such a pane however many times its window is
    -- linked.
    it "kill-server SIGKILLs a SIGHUP-ignoring pane in a linked window, and exits" $
        withHat hatBin $ \h -> do
        killsLinkedStubborn h "b:5" ["kill-server"]
        gone <- pollServerExited h 5000
        unless gone $
            expectationFailure "server did not exit after kill-server"
    it "kill-session SIGKILLs a SIGHUP-ignoring pane linked twice into it" $
        withHat hatBin $ \h -> killsLinkedStubborn h "a:5" ["kill-session", "-t", "a"]

    -- A command issued once kill-server returns must reach a fresh server,
    -- even while the killed one is still draining a SIGHUP-ignoring pane.
    it "a command after kill-server starts a fresh server, not the draining one" $
        withHat hatBin $ \h -> do
        _ <- hatCtl h ["new-session", "-d", "-s", "old", "trap '' HUP; exec sleep 300"]
        _ <- hatCtl h ["set-option", "-g", "@marker", "old"]
        _ <- hatCtl h ["kill-server"]
        (code, _, err) <- hatCtl h ["new-session", "-d", "-s", "fresh"]
        (code, err) `shouldBe` (ExitSuccess, "")
        sessions <- ctlOut h ["list-sessions", "-F", "#{session_name}"]
        marker <- ctlOut h ["display-message", "-p", "#{@marker}"]
        (sessions, marker) `shouldBe` ("fresh\n", "\n")

    -- bug 6: zooming a short pane that runs less (alternate screen) grew the
    -- pane; less redrew once for the new size via SIGWINCH. If the emulator is
    -- resized after the pty, that one-shot redraw lands on the old-size model
    -- and persists as garbage. The redraw must land clean at the new height.
    it "cleanly redraws a zoomed pane running less" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        -- Horizontal split: the new (bottom) pane is active and short.
        typeInto c "\x02\""
        awaitScreen c "\x2500"  -- ─ border
        -- Run less on distinctive numbered lines in the short pane.
        let zf = B8.pack (h.home </> "zf.txt")
        typeInto c $ "seq 1 200 | sed 's/^/ZOOMLINE-/' > " <> zf <> "; less " <> zf <> "\r"
        awaitScreen c "ZOOMLINE-1"
        -- Press h for less's full-screen help overlay (what the screenshot
        -- showed), then zoom via the control path (returns after reconcile).
        typeInto c "h"
        awaitScreen c "HELP -- Press RETURN"
        awaitTtyRead h "less"
        _ <- hatCtl h ["resize-pane", "-Z"]
        awaitScreen c "MOVING"        -- a help section that only fits post-zoom
        scr <- screenText c
        -- The help redrew clean at the new height: its header survives and the
        -- file content it overlays does not bleed through.
        T.isInfixOf "SUMMARY OF LESS COMMANDS" scr `shouldBe` True
        T.isInfixOf "ZOOMLINE-" scr `shouldBe` False
        typeInto c "q"                -- leave help
        typeInto c "q"                -- exit less
        awaitScreen c "$"

    -- The real client's byte paths: an argv in the C locale, and a
    -- buffer's stdin and stdout.
    it "carries a C-locale client's non-ASCII argv and a buffer's stdio bytes unmangled" $
        withHat hatBin $ \h -> do
        _ <- hatRaw h ["new-session", "-d", "-s", "x"]
        (code, _, _) <- hatRaw h ["rename-session", "-t", "x", "caf\xc3\xa9"]
        code `shouldBe` ExitSuccess
        (_, out, _) <- hatRaw h ["display-message", "-p", "#{session_name}"]
        out `shouldBe` "caf\xc3\xa9\n"
        let bytes = B.pack [0x61, 0x00, 0x1b, 0x5b, 0x31, 0x6d, 0xff, 0xc3, 0xa9, 0x0a]
        (ExitSuccess, _) <- hatBytes h ["load-buffer", "-b", "b", "-"] bytes
        hatBytes h ["save-buffer", "-b", "b", "-"] ""
            `shouldReturn` (ExitSuccess, bytes)

    -- An overlay scrollbar a copy-mode scroll or a hover reveals, hidden
    -- again once pane-scrollbars-timeout passes.
    it "flashes an overlay scrollbar for its timeout" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        let barShown want = awaitTrue ("the scrollbar shown: " <> show want) $ do
                scr <- Emu.snapshot c.screen
                let Style { bg = b } = ((scr.cells V.! 5) V.! 79).style
                pure ((b /= DefaultColor) == want)
        _ <- hatCtl h ["set", "-g", "pane-scrollbars-timeout", "300"]
        _ <- hatCtl h ["set", "-g", "pane-scrollbars", "modal"]
        _ <- hatCtl h ["copy-mode", "-u"]
        barShown True
        barShown False
        _ <- hatCtl h ["send-keys", "-X", "cancel"]
        _ <- hatCtl h ["set", "-g", "mouse", "on"]
        _ <- hatCtl h ["set", "-g", "pane-scrollbars", "auto-hide"]
        typeInto c "\ESC[<35;80;6M"
        barShown True
        barShown False

    it "adds a user option from customize-mode's Server Options, then edits it in the editor" $
        withHat hatBin $ \h -> do
        let editor = h.home <> "/editor.sh"
        writeExecutable editor "#!/bin/sh\nprintf edited > \"$1\"\n"
        c <- startClient h
        awaitScreen c "$"
        _ <- hatCtl h ["set", "-g", "editor", editor]
        _ <- hatCtl h ["customize-mode"]
        awaitScreen c "Server Options"
        ctlOut h ["display", "-p", "#{pane_mode}"] `shouldReturn` "options-mode\n"
        typeInto c "\r"
        awaitScreen c "New user option: @"
        typeInto c "cm added\r"
        awaitTrue "the option to be set" $
            (== "added\n") <$> ctlOut h ["show", "-sv", "@cm"]
        typeInto c "lje"
        awaitTrue "the editor's text to become the value" $
            (== "edited\n") <$> ctlOut h ["show", "-sv", "@cm"]

    it "edits a buffer from choose-buffer in the editor option's program" $
        withHat hatBin $ \h -> do
        let editor = h.home <> "/editor.sh"
        writeExecutable editor "#!/bin/sh\nprintf edited > \"$1\"\n"
        c <- startClient h
        awaitScreen c "$"
        _ <- hatCtl h ["set", "-g", "editor", editor]
        _ <- hatCtl h ["set-buffer", "-b", "b1", "old"]
        _ <- hatCtl h ["choose-buffer"]
        awaitScreen c "b1"
        typeInto c "e"
        awaitTrue "the buffer to hold what the editor saved" $
            (== "edited") <$> ctlOut h ["show-buffer", "-b", "b1"]

    -- A client the farewell sends away closes its popup on its way out,
    -- inside the window the linger holds open before the exec; a command
    -- deaf to that hangup must still not outlive the old image unreaped.
    it "reaps a popup's command across restart-server when its client goes first" $
        withHat hatBin $ \h -> do
        let pidFile = h.home <> "/popup.pid"
        c <- startClientEnv h [("HAT_TEST_RELOAD_LINGER", "300000")] []
        awaitScreen c "$"
        _ <- hatCtl h ["bind", "P", "display-popup"
            , "trap '' TERM HUP; echo $$ > " <> pidFile <> "; exec sleep 1000"]
        typeInto c "\x02P"
        pid <- read <$> awaitFile pidFile (not . null . words)
        _ <- hatCtl h ["restart-server", h.bin]
        _ <- awaitExit c
        awaitTrue "the handover to be consumed" $
            doesFileExist (h.sock <> ".reload.last")
        awaitTrue "the popup's command to be reaped" (not <$> pidAlive pid)

    -- bug 64: an outer terminal's extended-key sequence must arrive as the
    -- key it names, not as bytes. Pins that a mapping keyed on the raw ctrl
    -- byte still fires after one has been pressed.
    it "delivers ctrl keys to vim in every outer spelling (64, 1b)" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "vim -u NONE -i NONE -N\r"
        awaitScreen c1 "VIM - Vi IMproved"
        -- Every C-n bumps a counter and echoes it, whichever spelling
        -- delivers the key. (C-n, not the bug's C-s: raw 0x13 is XOFF to
        -- the pane's tty.)
        typeInto c1 ":let g:n=0\r"
        typeInto c1 ":nmap <C-n> :let g:n+=1<Bar>echom \"PING\" . g:n<CR>\r"
        typeInto c1 "\x0e"
        awaitScreen c1 "PING1"
        -- The outer terminal's spelling of the same key.
        typeInto c1 "\ESC[27;5;110~"
        awaitScreen c1 "PING2"
        typeInto c1 "\x0e"
        awaitScreen c1 "PING3"

        -- Ctrl punctuation: C-] must land as 0x1d whichever spelling the
        -- outer terminal sends (1b).
        typeInto c1 ":nmap <C-]> :let g:n+=1<Bar>echom \"PING\" . g:n<CR>\r"
        typeInto c1 "\x1d"
        awaitScreen c1 "PING4"
        typeInto c1 "\ESC[93;5u"
        awaitScreen c1 "PING5"
        typeInto c1 "\ESC[27;5;93~"
        awaitScreen c1 "PING6"
        -- C-[ is Escape in every spelling: the CSI-u form some outer
        -- terminals send must leave insert mode, never insert a '['.
        typeInto c1 "iqq"
        awaitScreen c1 "qq"
        typeInto c1 "\ESC[91;5u"
        typeInto c1 "AZ!"
        awaitScreen c1 "qqZ!"

        typeInto c1 "\ESC:q!\r"
        awaitScreen c1 "$"
        typeInto c1 "exit\r"
        _ <- awaitExit c1
        pure ()

    it "restores the saved tree at its captured area, and forgets it when the last window exits (4b)" $
        withHatPersist hatBin $ \h -> do
        -- Seed a saved tree so the autostarted server restores it.
        let lay = emitLayout (fitLayout (Size { rows = 30, cols = 100 }) (singlePane (PaneId 0)))
            snap = Snapshot
                { lastActiveSession = Nothing, sessions =
                    [ SessionSnap
                        { name = "stale", startCwd = "/tmp", currentIx = 0
                        , windowHist = []
                        , createdAt = Nothing, restores = 0
                        , group = Nothing
                        , windows =
                            [ WindowSnap
                                { ix = 0, name = "one", layout = lay
                                , active = 0, paneHist = []
                                , autoRename = False
                                , key = Nothing, linkOf = Nothing, modal = Nothing, size = Nothing, panes = [PaneSnap { cwd = "/tmp", command = Nothing, shellSpawned = False, kind = ProgramPane, inputOff = False, title = Nothing, titleStack = [] }] }
                            ] } ] }
        createDirectoryIfMissing True (takeDirectory (storeOf h))
        Persist.withStore (storeOf h) $ \c -> Persist.saveSnapshot c snap

        -- 4b: the tree comes back at its captured area, not 24x80. start-server
        -- only returns once startup (including the restore) has landed, and
        -- no client has attached yet to resize it.
        _ <- hatCtl h ["start-server"]
        ctlOut h ["list-panes", "-a", "-F", "#{pane_width}x#{pane_height}"]
            `shouldReturn` "100x30\n"

        c1 <- startClient h
        awaitScreen c1 "$"
        names1 <- ctlOut h ["list-sessions", "-F", "#{session_name}"]
        names1 `shouldSatisfy` List.isInfixOf "stale"

        -- bb: the restore archived the seed as generation 1; it is listable
        -- and restores by id, under a fresh name when the live session
        -- collides.
        listed <- ctlOut h ["list-snapshots"]
        lines listed `shouldSatisfy` any ("1: " `List.isPrefixOf`)
        out <- ctlOut h ["restore-snapshot", "1"]
        out `shouldContain` "as 'stale-2'"
        names <- ctlOut h ["list-sessions", "-F", "#{session_name}"]
        lines names `shouldSatisfy`
            (\ns -> "stale" `elem` ns && "stale-2" `elem` ns)
        _ <- ctlOut h ["kill-session", "-t", "stale-2"]

        -- Exiting the last shell closes the last window, the session and
        -- the server. Unlike kill-server, this must drop the saved tree:
        -- the next start comes up pristine instead of resurrecting it.
        typeInto c1 "exit\r"
        _ <- awaitExit c1
        gone <- pollServerGone h.sock 5000
        unless gone $ expectationFailure "server did not exit"

        -- bb: the drain wipes only the live tree; the history keeps it.
        hist <- Persist.withStore (storeOf h) Persist.listArchived
        [ s.name | a <- hist, s <- a.snapshot.sessions ]
            `shouldContain` ["stale"]

        c2 <- startClient h
        awaitScreen c2 "$"
        names2 <- ctlOut h ["list-sessions", "-F", "#{session_name}"]
        names2 `shouldNotSatisfy` List.isInfixOf "stale"
        wl <- ctlOut h ["list-windows", "-a", "-F", "#{window_name}"]
        length (lines wl) `shouldBe` 1

    it "restores a whitelisted running command (vim) under a live shell" $
        withHatPersist hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "vim -u NONE -i NONE\r"
        awaitScreen c1 "VIM - Vi IMproved"

        -- kill-server saves the tree; vim is the pane's foreground command
        -- and is on the default restore whitelist.
        _ <- ctlOut h ["kill-server"]
        _ <- awaitExit c1
        gone <- pollServerExited h 5000
        unless gone $ expectationFailure "server did not die"

        -- Restart: the restored pane comes back running vim, not a shell.
        -- vim's empty-buffer '~' filler proves it (a restored shell shows
        -- the '$ ' prompt and never a '~'); the intro banner is skipped
        -- because restore re-runs bare `vim`, whose vimrc may suppress it.
        c2 <- startClient h
        awaitScreen c2 "~"

        -- 82: vim comes back as a JOB of a live shell (typed in, not exec'd in
        -- place), so Ctrl-z suspends it back to a real prompt that still runs
        -- commands. Sync on the shell reclaiming the foreground before typing,
        -- so the keystrokes can't land in a still-foreground vim.
        awaitForeground h "vim"
        typeInto c2 "\x1a"                          -- Ctrl-Z: suspend vim
        awaitForeground h "sh"
        typeInto c2 "echo dropped-to-$((3*7))\r"
        awaitScreen c2 "dropped-to-21"

    it "notifies a formerly-zoomed pane's child that it shrank on select-pane away" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"

        -- Split side by side; the new active (right) pane is 39 cols wide.
        typeInto c1 "\x02%"
        awaitScreen c1 "\x2502"  -- │

        -- Zoom the active pane: its child now sees the full 80 cols. Zoom via
        -- the control path (not the \x02z keystroke) so the command returns
        -- only after the pty resize is reconciled; an interactive keystroke is
        -- fire-and-forget, so a following 'stty size' could read the pre-zoom
        -- 39 before SIGWINCH lands.
        _ <- ctlOut h ["resize-pane", "-Z"]
        typeInto c1 "stty size\r"
        awaitScreen c1 "23 80"

        -- Move away, cancelling the zoom, then back to the formerly-zoomed
        -- pane. Its child must have been told it shrank back to 39 cols
        -- (TIOCSWINSZ -> SIGWINCH); without that it still believes it is 80.
        _ <- ctlOut h ["select-pane", "-L"]   -- to the left pane, unzoom
        awaitScreen c1 "\x2502"
        _ <- ctlOut h ["select-pane", "-R"]   -- back to the ex-zoomed pane
        typeInto c1 "stty size\r"
        awaitScreen c1 "23 39"

    -- Bug a4: break-pane numbered the new window from 0, ignoring base-index.
    -- With base-index 1 the origin window is 1, so the broken-out pane must
    -- land at the next free index (2), exactly like new-window would.
    -- Bug: a pane's reader captures the window it was spawned in, but
    -- break-pane re-parents the live pane. When its program then exits, the
    -- reader must reap it from its CURRENT window (2), not the split it came
    -- from (1) — otherwise the broken-out window lingers with a dead pane.
    it "break-pane numbers the new window from base-index, which closes when its program exits (a4)" $
        withHat hatBin $ \h -> do
        let confPath = h.home <> "/hat.conf"
        writeFile confPath "set -g base-index 1\n"
        c1 <- startClientArgs h ["-f", confPath]
        awaitScreen c1 "1:sh*"
        typeInto c1 "\x02%"                 -- split -h; new pane active
        awaitScreen c1 "\x2502"
        _ <- ctlOut h ["break-pane"]        -- move it into window 2 (now current)
        out <- ctlOut h ["list-windows", "-F", "#{window_index}"]
        List.sort (filter (not . null) (lines out)) `shouldBe` ["1", "2"]
        awaitScreen c1 "2:sh*"
        typeInto c1 "exit\r"                 -- end the broken-out pane's shell
        awaitWith "window 2 collapsed, back on window 1" (\d -> do
            t <- screenText d
            -- Match the window-2 label ("2:sh"), not a bare "2:", so the
            -- status-bar clock (e.g. 12:52) can't spoof the "still there" check.
            pure (not ("2:sh" `T.isInfixOf` t) && "1:sh*" `T.isInfixOf` t)) c1

    -- See Note [Captured links].
    it "save -> kill-server -> restart -> restore rebuilds the window tree, its links and groups shared" $
        withHatPersist hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        -- Build a tree with the targeted resurrect primitives: rename
        -- window 0 by target, add a named window 1 by target.
        _ <- ctlOut h ["rename-window", "-t", "0", "built0"]
        awaitScreen c1 "0:built0*"
        _ <- ctlOut h ["new-window", "-d", "-t", "1", "-n", "editor"]
        awaitScreen c1 "1:editor"
        -- A window linked into a second session, and a session grouped
        -- with the first: each must come back shared, not copied.
        _ <- ctlOut h ["new-window", "-d", "-t", "0:2", "-n", "shared"]
        _ <- ctlOut h ["new-session", "-d", "-s", "other"]
        _ <- ctlOut h ["link-window", "-d", "-s", "0:2", "-t", "other:5"]
        _ <- ctlOut h ["new-session", "-d", "-s", "grp", "-t", "0"]
        let meta = words <$> ctlOut h
                ["display-message", "-p", "-t", "0:", "#{session_created} #{session_restores}"]
            probe = do
                linked <- ctlOut h ["display-message", "-p", "-t", "other:5"
                    , "#{window_linked_sessions_list}"]
                grouped <- ctlOut h ["display-message", "-p", "-t", "grp:"
                    , "#{session_group_list}"]
                panes <- ctlOut h ["list-panes", "-a", "-F", "#{pane_id}"]
                pure (lines linked, lines grouped, length (lines panes))
        [created0, "0"] <- meta
        saved <- probe
        saved `shouldBe` (["0,other,grp"], ["0,grp"], 8)

        -- Kill the server; the tree is saved automatically on the way out.
        _ <- ctlOut h ["kill-server"]
        _ <- awaitExit c1
        gone <- pollServerExited h 5000
        unless gone $ expectationFailure "server did not die"

        -- Restart: a fresh client autostarts a new server, which restores
        -- the saved tree before we attach -- no manual replay needed.
        c2 <- startClientArgs h ["attach", "-t", "0"]
        awaitScreen c2 "0:built0"
        awaitScreen c2 "1:editor"
        probe `shouldReturn` saved
        -- Session identity rides along: same creation stamp, one restore.
        meta `shouldReturn` [created0, "1"]

    -- b7: a window's automatic-rename status must survive save/restore, in
    -- both directions -- an auto-renaming window keeps tracking its pane, a
    -- manually-named window stays pinned.
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
        path <- testPath
        c1 <- startClientEnv h [("PATH", bin <> ":" <> path)]
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

    it "copy-pipe does not freeze the client when the command leaves a child holding stdout" $
        withHat hatBin $ \h -> do
        -- xclip forks a selection-owner daemon that inherits and never closes
        -- the command's stdout. If copy-pipe reads that stdout to EOF on the
        -- client's input thread, the thread blocks forever and the client
        -- goes dead. 'sleep' backgrounded in a subshell reproduces the leak.
        writeFile (h.home <> "/hat.conf") $
            "bind-key -T copy-mode p send-keys -X copy-pipe 'sleep 30 &'\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "$"
        typeInto c1 "echo PIPEDWORD"
        awaitScreen c1 "PIPEDWORD"
        baseline <- reverseCellCount c1
        typeInto c1 "\x02[\x01 \x05"     -- enter copy mode; C-a, Space, C-e
        awaitWith "selection highlighted" (\d ->
            (> baseline) <$> reverseCellCount d) c1
        typeInto c1 "p"                  -- copy-pipe with the leaking command
        -- The input thread must keep processing keys. If it blocked in the
        -- pipe, none of these land and the screen never shows the marker.
        typeInto c1 "q"                  -- leave copy mode
        typeInto c1 "echo STILLALIVE\r"
        awaitScreen c1 "STILLALIVE"

    -- Pins the structural reap: the pipe-pane child is owned as a scoped
    -- sub-resource, so stopping the pipe — and, on the teardown path, killing
    -- the pane out from under it — reaps the child rather than orphaning it.
    it "pipe-pane tees pane output to a command until stopped, reaping it on stop and on pane kill" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        let readPid path =
                read . takeWhile (/= '\n') <$> awaitFile path (not . null)
            logPath = h.home <> "/pane.log"
            pidPath1 = h.home <> "/pipe1.pid"
        _ <- ctlOut h ["pipe-pane", "echo $$ > " <> pidPath1 <> "; cat >> " <> logPath]
        pid1 <- readPid pidPath1
        typeInto c1 "echo PIPEPANEMARKER\r"
        awaitScreen c1 "PIPEPANEMARKER"
        _ <- awaitFile logPath (List.isInfixOf "PIPEPANEMARKER")

        -- Stop path: no-arg pipe-pane cancels the supervisor, reaping the
        -- child, and later output is not captured. The pane taps output to
        -- the pipe BEFORE the emulator renders it, so once AFTERSTOPMARKER is
        -- on screen, a still-live pipe would already have written it — its
        -- absence is thus decidable now.
        _ <- ctlOut h ["pipe-pane"]
        awaitReaped pid1
        typeInto c1 "echo AFTERSTOPMARKER\r"
        awaitScreen c1 "AFTERSTOPMARKER"
        readFile logPath >>= (`shouldNotSatisfy` List.isInfixOf "AFTERSTOPMARKER")

        -- Teardown path: killing the pane runs reapPane -> stopPipe, so a live
        -- pipe child is reaped as the pane dies, not left orphaned.
        let pidPath2 = h.home <> "/pipe2.pid"
        _ <- ctlOut h ["pipe-pane", "echo $$ > " <> pidPath2 <> "; cat > /dev/null"]
        pid2 <- readPid pidPath2
        pidAlive pid2 `shouldReturn` True
        _ <- ctlOut h ["kill-pane"]
        awaitReaped pid2

    it "a command list holding a new-session autostarts the server" $
        withHat hatBin $ \h -> do
        (code, _, err) <- hatCtl h
            ["set", "-g", "@early", "yes", ";", "new", "-d"]
        (code, err) `shouldBe` (ExitSuccess, "")
        ctlOut h ["show", "-gv", "@early"] `shouldReturn` "yes\n"

    it "a bare hat runs default-client-command; new-session -A attaches where a bare attach would" $
        withHat hatBin $ \h -> do
        _ <- hatCtl h ["new-session", "-d", "-s", "first"]
        _ <- hatCtl h ["new-session", "-d", "-s", "second"]
        _ <- hatCtl h ["set", "-g", "default-client-command", "list-sessions -F '#S'"]
        hatCtl h [] `shouldReturn` (ExitSuccess, "first\nsecond\n", "")
        c <- startClientArgs h ["new-session", "-A"]
        awaitScreen c "[first]"

    it "a bare hat that starts the server runs its config's default-client-command" $
        withHat hatBin $ \h -> do
        let confPath = h.home <> "/hat.conf"
        writeFile confPath "set -g default-client-command 'new-session -s fromconf'\n"
        c <- startClientArgs h ["-f", confPath]
        awaitScreen c "[fromconf]"

    it "remain-on-exit failed drops a pane whose program succeeded" $
        withHat hatBin $ \h -> do
        _ <- hatCtl h
            [ "set", "-g", "remain-on-exit", "failed", ";", "new", "-d", ";"
            , "new-window", "-d", "true", ";", "new-window", "-d", "exit 1" ]
        -- By status, not window index: once true's window closes, the
        -- next new-window may reuse its index.
        awaitTrue "only the failed pane to remain" $
            (== ["0 ", "1 1"]) . List.sort . lines <$> ctlOut h
                ["list-panes", "-a", "-F", "#{pane_dead} #{pane_dead_status}"]

    it "remain-on-exit keeps a dead pane with its status, respawn-pane reruns it, and a live respawn needs -k" $
        withHat hatBin $ \h -> do
        let runs = h.home <> "/runs"
        _ <- hatCtl h
            [ "set", "-g", "remain-on-exit", "on", ";"
            , "new", "-d", "-x", "60", "-y", "5"
            , "echo run >> " <> runs <> "; echo DEADBEACON; exit 3" ]
        let dead = ctlOut h ["display", "-p", "#{pane_dead}"]
        awaitTrue "the pane to die" $
            (== "1 3\n") <$> ctlOut h
                ["display", "-p", "#{pane_dead} #{pane_dead_status}"]
        screen <- ctlOut h ["capture-pane", "-p", "-S", "-"]
        screen `shouldSatisfy` List.isInfixOf "DEADBEACON"
        screen `shouldSatisfy` List.isInfixOf "Pane is dead (status 3, "
        _ <- hatCtl h ["respawn-pane"]
        _ <- awaitFile runs ((== 2) . length . lines)
        awaitTrue "the rerun to die" ((== "1\n") <$> dead)

        _ <- hatCtl h ["respawn-pane", "sleep 100"]
        dead `shouldReturn` "0\n"
        pid <- read <$> ctlOut h ["display", "-p", "#{pane_pid}"]
        (_, _, err) <- hatCtl h ["respawn-pane", "true"]
        err `shouldBe` "respawn pane failed: pane 0:0.0 still active\n"
        _ <- hatCtl h ["respawn-pane", "-k", "sleep 200"]
        awaitReaped pid
        dead `shouldReturn` "0\n"

        -- respawn-window -k reduces the window to one respawned pane.
        _ <- hatCtl h ["split-window", "-d"]
        ctlOut h ["display", "-p", "#{window_panes}"] `shouldReturn` "2\n"
        (_, _, werr) <- hatCtl h ["respawn-window", "true"]
        werr `shouldBe` "respawn window failed: window 0:0 still active\n"
        let ran = h.home <> "/ran"
        _ <- hatCtl h ["respawn-window", "-k", "echo yes > " <> ran <> "; sleep 300"]
        ctlOut h ["display", "-p", "#{window_panes}"] `shouldReturn` "1\n"
        awaitFile ran (== "yes\n") `shouldReturn` "yes\n"

    it "remain-on-exit key closes a dead pane on the next key" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        _ <- hatCtl h ["set", "-g", "remain-on-exit", "key"]
        typeInto c "exit\r"
        awaitScreen c "Pane is dead"
        typeInto c "x"
        _ <- awaitExit c
        pure ()

    -- The autostart barrier (upstream if-shell-TERM.sh): the client that
    -- spawned the server must wait out the config before its new-session
    -- runs, or the pane spawns before `set -g default-terminal` applies.
    -- The sleep is a race inducer inside the fixture: without the barrier
    -- the pane deterministically wins and captures the default TERM.
    it "an autostarting new waits out the config before spawning its pane" $
        withHat hatBin $ \h -> do
        let confPath = h.home <> "/hat.conf"
            out = h.home <> "/term.out"
        writeFile confPath
            "if 'sleep 0.3' 'set -g default-terminal vt220' 'set -g default-terminal ansi'\n"
        _ <- hatCtl h ["-f", confPath, "new", "-d", "echo \"#$TERM\" >> " <> out]
        got <- awaitFile out ("#vt220" `List.isInfixOf`)
        got `shouldSatisfy` ("#vt220" `List.isInfixOf`)

    -- A nested hat command as an if-shell condition during config load
    -- (upstream if-shell-nested.sh): with persistence on this used to
    -- deadlock the whole startup — the nested client parked on the restore
    -- gate the config thread itself held — and the pane's `show` raced the
    -- config's `set`. The bounded hatCtl pins the deadlock; the file pins
    -- the ordering.
    it "serves a nested hat command inside an if-shell condition mid-config" $
        withHatPersist hatBin $ \h -> do
        let confPath = h.home <> "/hat.conf"
            out = h.home <> "/done.out"
        writeFile confPath $
            "if 'sleep 0.3; " <> h.bin <> " -S " <> h.sock
                <> " run \"true\"' 'set -s @done yes'\n"
        r <- timeout 15_000_000 $ hatCtl h
            [ "-f", confPath, "new", "-d"
            , h.bin <> " -S " <> h.sock <> " show -vs @done >> " <> out ]
        r `shouldSatisfy` isJust
        got <- awaitFile out ("yes" `List.isInfixOf`)
        got `shouldSatisfy` ("yes" `List.isInfixOf`)

    it "autostarts the server when the socket directory does not exist yet" $
        -- the parent dirs of the socket must be created by the server.
        withHatOn hatBin False "fresh/subdir/socket" $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

    -- An autostart comes up once the lock of a server still on its way out
    -- frees.
    it "an autostart outlasts a lock held by a server still exiting" $
        withHat hatBin $ \h -> do
        let lockPath = h.sock <> ".lock"
        -- Non-empty, so the autostarted server's O_TRUNC open is observable.
        B.writeFile lockPath "held"
        fd <- PIO.openFd lockPath PIO.ReadWrite PIO.defaultFileFlags
        released <- newIORef False
        let release = do
                first <- atomicModifyIORef' released (True,)
                unless first (closeFd fd)
        (do PIO.setLock fd (PIO.WriteLock, AbsoluteSeek, 0, 0)
            c <- startClient h
            -- By stat: closing any fd on the file would drop our lock.
            awaitTrue "the autostarted server to open the lock" $
                (== 0) <$> getFileSize lockPath
            release
            awaitScreen c "0:sh*")
            `finally` release

    it "attaches without a controlling terminal: exits with 'not a terminal'" $
        withHat hatBin $ \h -> do
        (code, _, err) <- hatCtl h []
        code `shouldBe` ExitFailure 1
        err `shouldSatisfy` List.isInfixOf "not a terminal"

    it "refuses a TERM terminfo does not know, as tmux does" $
        withHat hatBin $ \h -> do
        c <- startClientEnv h [("TERM", "hat-no-such-terminal")] []
        status <- awaitExit c
        t <- readIORef c.transcript
        status `shouldBe` Exited (ExitFailure 1)
        t `shouldSatisfy` B8.isInfixOf "missing or unsuitable terminal: hat-no-such-terminal"

    it "runs cat: single echo, then Ctrl-D EOFs back to the shell" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        typeInto c1 "cat\r"
        awaitForeground h "cat"
        -- Without Enter, canonical mode echoes each char once; a doubled
        -- echo ("aabbccxxyyzz") would never contain "abcxyz".
        typeInto c1 "abcxyz"
        awaitScreen c1 "abcxyz"
        typeInto c1 "\r"
        -- Ctrl-D on the now-empty line EOFs cat; only the shell can then
        -- evaluate the arithmetic, so "done-42" is its own sync.
        typeInto c1 "\x04"
        typeInto c1 "echo done-$((21+21))\r"
        awaitScreen c1 "done-42"
        typeInto c1 "exit\r"
        status <- awaitExit c1
        status `shouldBe` Exited ExitSuccess

    -- What only the exec shows of a reload: pid, fd (8a) and lock survival,
    -- the alt-screen grid by byte replay, and the consumed handover as .last.
    it "restart-server twice in a row keeps a full-screen program's pid, display and input, the fds named and the lock held" $
        withHat hatBin $ \h -> do
        let digits = filter (\ch -> ch >= '0' && ch <= '9')
            -- This build, never the `hat` a bare restart-server finds on PATH.
            reload c = hatCtl h ["restart-server", h.bin] >> awaitExit c
        c <- startClient h
        awaitScreen c "$"
        pidBefore <- digits <$> ctlOut h ["list-panes", "-F", "#{pane_pid}"]
        pidBefore `shouldNotBe` ""
        -- Octal escapes keep a literal ESC out of the shell's line editor;
        -- `cat` never redraws, so the beacon survives only by replay.
        typeInto c "printf '\\033[?1049h\\033[2J\\033[HBEACON-ONE'; cat\r"
        awaitScreen c "BEACON-ONE"
        _ <- reload c
        c2 <- startClient h
        awaitScreen c2 "BEACON-ONE"
        doesFileExist (h.sock <> ".reload.last") `shouldReturn` True
        _ <- reload c2
        c3 <- startClient h
        awaitScreen c3 "BEACON-ONE"
        digits <$> ctlOut h ["list-panes", "-F", "#{pane_pid}"]
            `shouldReturn` pidBefore
        typeInto c3 "after-reload\r"
        awaitScreen c3 "after-reload"
        fdsOn h (h.home </> "server.log") `shouldReturn` 1
        pid <- serverPid h
        lockHolder h `shouldReturn` Just pid
        fdsOn h (h.sock <> ".lock") `shouldReturn` 1

    -- `restart` = restart-server + restart-client: the attached client
    -- re-execs and reattaches instead of exiting, so one command upgrades
    -- both halves (bug 4f). The pane pid pins the server half; the same pty
    -- client still delivering input pins the client half. 83: the reattach
    -- must wait in the listen backlog for the next image, even when the old
    -- image lingers before its exec.
    it "restart reattaches its client across a lingering old image (4f, 83)" $
        withHat hatBin $ \h -> do
        let digits = filter (\ch -> ch >= '0' && ch <= '9')
        c <- startRestartableClient h [("HAT_TEST_RELOAD_LINGER", "200000")]
        awaitScreen c "$"
        typeInto c "echo before-restart\r"
        awaitScreen c "before-restart"
        pidBefore <- digits <$> ctlOut h ["list-panes", "-F", "#{pane_pid}"]
        pidBefore `shouldNotBe` ""
        _ <- hatCtl h ["restart", h.bin]
        -- Only type once the new image owns the ptys (the consumed handover)
        -- and the re-exec'd client is attached to it, so no keystroke can
        -- land in the old image's final instants.
        awaitTrue "the reload handover to be consumed" $
            doesFileExist (h.sock <> ".reload.last")
        -- Through the driver, so a timeout shows the pty: whatever the
        -- re-exec'd client printed before dying is the diagnosis.
        awaitWith "the re-exec'd client to reattach"
            (\_ -> not . null . words <$> ctlOut h ["list-clients"]) c
        -- "done-42" can only render if the reloaded server adopted the pane
        -- AND the re-exec'd client reattached to shuttle the keystrokes.
        typeInto c "echo done-$((21+21))\r"
        awaitScreen c "done-42"
        pidAfter <- digits <$> ctlOut h ["list-panes", "-F", "#{pane_pid}"]
        pidAfter `shouldBe` pidBefore

    it "restart reattaches a client whose PATH hat cannot answer the probe (bug 52)" $
        withHat hatBin $ \h -> do
        ambient <- testPath
        let bindir = h.home </> "bin"
        createDirectoryIfMissing True bindir
        writeExecutable (bindir </> "hat")
            "#!/bin/sh\necho 'hat: no server running' >&2\nexit 1\n"
        c <- startClientEnv h [("PATH", bindir <> ":" <> ambient)] []
        awaitScreen c "$"
        _ <- hatCtl h ["restart", h.bin]
        awaitTrue "the reload handover to be consumed" $
            doesFileExist (h.sock <> ".reload.last")
        awaitWith "the re-exec'd client to reattach"
            (\_ -> not . null . words <$> ctlOut h ["list-clients"]) c
        typeInto c "echo done-$((21+21))\r"
        awaitScreen c "done-42"

    -- b89: a pane's output stream must cross a reload without a gap, even
    -- for bytes emitted inside the capture->exec window (the linger holds
    -- that window open). A ticker pins the stream; a missing tick number
    -- is a byte the old image consumed but never handed over.
    it "restart-server drops no pane output from the capture window (b89)" $
        withHat hatBin $ \h -> do
        c <- startClientEnv h [("HAT_TEST_RELOAD_LINGER", "100000")] []
        awaitScreen c "$"
        -- The ticker stops on a flag file, not a keystroke, so stopping it
        -- cannot race the reattach; the sentinel prints as done-ticks-42 but
        -- echoes with the $(( )) unexpanded, so only the real end matches.
        typeInto c (B8.pack ("i=0; while [ ! -e " <> h.home <> "/stop-ticks ]; \
            \do i=$((i+1)); echo TICK-$i-END; sleep 0.01; done; \
            \echo done-ticks-$((40+2))\r"))
        -- The screen scrolls as it ticks: any visible tick past the third.
        awaitWith "the ticker to pass tick 3"
            (fmap (any (>= 3) . tickNumbers) . screenText) c
        _ <- hatCtl h ["restart-server", h.bin]
        _ <- awaitExit c
        c2 <- startClient h
        writeFile (h.home </> "stop-ticks") ""
        -- The sentinel is FIFO-last through the pane, so its render proves
        -- every tick the kernel buffered across the exec has flowed through.
        awaitScreen c2 "done-ticks-42"
        ticks <- tickNumbers . T.pack <$> ctlOut h ["capture-pane", "-p", "-S", "-"]
        case ticks of
            [] -> expectationFailure "no ticks captured"
            first : _ -> ticks `shouldBe` [first .. last ticks]

    -- Bug 8d: `restart` must carry a client's whole last-session stack, not
    -- just the one alternate. Visiting 0 -> a -> b leaves history [a, 0];
    -- after the re-exec, killing the head (a) must let `switch-client -l`
    -- fall through to 0. A depth-1 carry (the old behaviour) would have
    -- dropped 0, so -l would find "no last session" and stay on b.
    it "restart preserves the whole last-session stack, not just one" $
        withHat hatBin $ \h -> do
        ambient <- testPath
        let bindir = h.home </> "bin"
        createDirectoryIfMissing True bindir
        createFileLink h.bin (bindir </> "hat")
        c <- startClientEnv h [("PATH", bindir <> ":" <> ambient)] []
        awaitScreen c "[0]"
        typeInto c "\x02:"
        awaitPromptOpen c
        typeInto c "new-session -s a\r"
        awaitScreen c "[a]"
        typeInto c "\x02:"
        awaitPromptOpen c
        typeInto c "new-session -s b\r"
        awaitScreen c "[b]"
        _ <- hatCtl h ["restart", h.bin]
        awaitTrue "the reload handover to be consumed" $
            doesFileExist (h.sock <> ".reload.last")
        awaitWith "the re-exec'd client to reattach"
            (\_ -> not . null . words <$> ctlOut h ["list-clients"]) c
        awaitScreen c "[b]"
        _ <- hatCtl h ["kill-session", "-t", "a"]
        typeInto c "\x02:"
        awaitPromptOpen c
        typeInto c "switch-client -l\r"
        awaitScreen c "[0]"

    -- Bug 4: restart -C sent both clients to "no such session" exits. The
    -- slow config holds the re-exec'd image in LoadingConfig, the window
    -- the farewelled attaches race. Also pins bug 81: each client rejoins
    -- its OWN session, not the last-active one a bare attach lands on.
    it "restart -C returns each client to its own session" $
        withHat hatBin $ \h -> do
        ambient <- testPath
        let bindir = h.home </> "bin"
        createDirectoryIfMissing True bindir
        createFileLink h.bin (bindir </> "hat")
        -- The config's sleep widens the re-exec boot's load window, so the
        -- farewelled clients reattach inside it; the first boot skips it.
        let confPath = h.home <> "/hat.conf"
            holdPath = h.home <> "/hold-config"
        writeFile confPath $
            "if '! test -f " <> holdPath <> " || sleep 0.7' 'set -g escape-time 0'\n"
        let env = [("PATH", bindir <> ":" <> ambient)]
        c1 <- startClientEnv h env ["-f", confPath]
        awaitScreen c1 "$"
        c2 <- startClientEnv h env ["new-session", "-s", "beta"]
        awaitScreen c2 "$"
        let ownSessions = do
                out <- ctlOut h ["list-clients", "-F", "#{session_name}"]
                pure (List.sort (lines out) == ["0", "beta"])
        awaitTrue "two clients on their own sessions" ownSessions
        performGC  -- deterministic: drivers must survive collection mid-test
        writeFile holdPath ""
        _ <- hatCtl h ["restart", "-C", h.bin]
        awaitTrue "the reload handover to be consumed" $
            doesFileExist (h.sock <> ".reload.last")
        awaitWith "each re-exec'd client to rejoin its own session"
            (\_ -> ownSessions) c1

    it "a client that stops reading stalls no pane for anyone else" $
        withHat hatBin $ \h -> do
        go <- alertStorm h
        unread <- attachUnread h
        writeFile go ""
        awaitAlertStorm h
        N.close unread.sock

    it "fails at once, as tmux does, on a socket path too long for an address" $
        withHat hatBin $ \h -> do
        let long = h.home </> replicate 120 'x'
        timeout 5_000_000 (hatCtl h ["-S", long, "new-session", "-d"]) `shouldReturn`
            Just (ExitFailure 1, "", "error connecting to " <> long <> " (File name too long)\n")

-- A detached session whose pane, once the returned file exists, raises
-- 2000 alerts -- a bell and a 1 KiB desktop notification each, far more
-- than a socket buffer holds -- and then prints STORM-DONE.
alertStorm :: Hat -> IO FilePath
alertStorm h = do
    let go = h.home </> "go"
        script = h.home </> "storm.sh"
    writeFile script $ unlines
        [ "while [ ! -e " <> go <> " ]; do sleep 0.05; done"
        , "awk 'BEGIN { pad = sprintf(\"%1000s\", \"\"); gsub(/ /, \"x\", pad);"
        , "  for (i = 1; i <= 2000; i++) printf \"%d\\a\\033]9;%s\\033\\\\\\n\", i, pad;"
        , "  print \"STORM-\" \"DONE\" }'"
        , "exec sleep 600" ]
    _ <- hatCtl h ["new-session", "-d", "sh " <> script]
    pure go

awaitAlertStorm :: Hat -> IO ()
awaitAlertStorm h = awaitTrue "the alert storm to finish" $
    List.isInfixOf "STORM-DONE" <$> ctlOut h ["capture-pane", "-p"]

-- Attach over the raw wire and never read again: a client whose terminal
-- has stopped draining (XOFF, a stopped process, a stalled link).
attachUnread :: Hat -> IO W.ReadEnd
attachUnread h = do
    sock <- either (const (fail "attachUnread: no server")) pure =<< connectTo h.sock
    W.sendMessage sock $ W.ClientHello W.Hello
        { protoVersion = W.protocolVersion, term = "xterm", env = []
        , size = Size { rows = 24, cols = 80 }, cwd = T.pack h.home
        , intent = W.AttachIntent [], autostarted = False, sessionHist = []
        , termCaps = Nothing, argvBytes = W.TextIsExact, ttyName = Nothing }
    re <- W.newReadEnd sock
    W.recvMessage re >>= \case
        Just (W.Known (W.Welcome _)) -> pure re
        other -> fail ("attachUnread: " <> show other)

-- The @n@ of each @TICK-n-END@ word, in order.
tickNumbers :: T.Text -> [Int]
tickNumbers txt =
    [ n | w <- T.words txt
        , Just num <- [T.stripPrefix "TICK-" w >>= T.stripSuffix "-END"]
        , Just n <- [readMaybe (T.unpack num)] ]

-- A kill reaps a SIGHUP-ignoring pane whose window a:1 is also linked at
-- @dst@.
killsLinkedStubborn :: Hat -> String -> [String] -> IO ()
killsLinkedStubborn h dst kill = do
    _ <- ctlOut h ["new-session", "-d", "-s", "a"]
    let pidFile = h.home </> "stubborn.pid"
    _ <- ctlOut h ["new-window", "-d", "-t", "a:1"
        , "trap '' HUP; echo $$ > " <> pidFile <> "; exec sleep 300"]
    stubborn <- read . takeWhile (/= '\n') <$> awaitFile pidFile (not . null)
    (do _ <- ctlOut h ["new-session", "-d", "-s", "b"]
        _ <- ctlOut h ["link-window", "-d", "-s", "a:1", "-t", dst]
        _ <- ctlOut h kill
        awaitReaped stubborn)
        `finally` signalPids sigKILL [stubborn]

-- | Poll every 10ms until @done@ holds; False past @budgetMs@.
pollFor :: Int -> IO Bool -> IO Bool
pollFor budgetMs done = isJust <$> timeout (budgetMs * 1000) loop
  where
    loop = done >>= \ok -> unless ok (threadDelay 10_000 *> loop)

pollServerGone :: FilePath -> Int -> IO Bool
pollServerGone path budgetMs = pollFor budgetMs (not <$> serverUp path)

-- | Until no server process remains on @h@'s socket. kill-server unlinks the
-- socket before it replies, so a vanished socket alone no longer shows the
-- process has exited.
pollServerExited :: Hat -> Int -> IO Bool
pollServerExited h budgetMs = pollFor budgetMs (null <$> serverPids h)

-- | Whether a server answers at the path. The probe is closed before
-- returning: an open probe counts as an active connection and holds the
-- server out of its idle exit.
serverUp :: FilePath -> IO Bool
serverUp path = connectTo path >>= \case
    Left _ -> pure False
    Right sock -> True <$ N.close sock
