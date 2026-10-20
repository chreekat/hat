-- | End-to-end tests driving the real @hat@ binary through a pty.
module Hat.IntegrationSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (Async, async, cancel, link)
import Control.Concurrent.STM
    (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry,
     writeTVar)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception
    (IOException, SomeException, bracket, catch, evaluate, finally, throwIO,
     try)
import Control.Monad (forM, forM_, unless, void, when)
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
import System.Posix.IO (closeFd, createPipe, fdToHandle, handleToFd)
import System.Posix.IO qualified as PIO
import System.Posix.Process (ProcessStatus (..))
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
import Hat.TestSupport (bashPath, nodeOf)
import Hat.Server.Layout (Orientation (..), fitLayout, singlePane)
import Hat.Server.LayoutString (emitLayout)
import Hat.Transport.Socket (connectTo)
import Hat.Transport.Wire qualified as W
import Hat.Term.Cell (Cell (..), Color (..), Style (..))
import Hat.Term.Emulator qualified as Emu
import Hat.Term.HostProtocol qualified as HP

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
        Nothing -> void $ P.readProcessWithExitCode
            "pkill" ["-f", "--", "--server " <> h.sock] ""
        Just _ -> pure ()
    -- kill-server returns before the drain finishes, and a draining server
    -- stops accepting before it exits, so "unreachable" is not "gone". The
    -- poll gives a clean drain its chance; the unconditional -9 guarantees
    -- nothing survives the socket-path unlink below.
    _ <- pollServerGone h.sock 20
    void $ P.readProcessWithExitCode
        "pkill" ["-9", "-f", "--", "--server " <> h.sock] ""
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

-- Stdout of a hat control command.
ctlOut :: Hat -> [String] -> IO String
ctlOut h args = (\(_, out, _) -> out) <$> hatCtl h args

-- This Hat's server pid, found by the socket path in its argv — the same
-- handle 'teardown' reaps by.
serverPid :: Hat -> IO Int
serverPid h = do
    out <- serverPids h
    case mapMaybe readMaybe (lines out) of
        (p : _) -> pure p
        [] -> fail ("no server running on " <> h.sock)

-- The pids of every server process on this Hat's socket, one per line.
serverPids :: Hat -> IO String
serverPids h = do
    (_, out, _) <- P.readProcessWithExitCode
        "pgrep" ["-f", "--", "--server " <> h.sock] ""
    pure out

-- How many fds the server holds on one path.
fdsOn :: Hat -> FilePath -> IO Int
fdsOn h path = do
    pid <- serverPid h
    let dir = "/proc/" <> show pid <> "/fd"
    entries <- listDirectory dir
    links <- forM entries $ \e ->
        readSymbolicLink (dir </> e) `catch` \(_ :: IOException) -> pure ""
    pure (length (filter (== path) links))

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

-- The shared body of the restart-keeps-client tests: the pane pid pins the
-- server half of the upgrade, the same pty client still delivering input
-- pins the client half. @extraEnv@ seeds the autostarted server's
-- environment (e.g. the reload linger knob).
restartKeepsClient :: [(String, String)] -> Hat -> IO ()
restartKeepsClient extraEnv h = do
    let digits = filter (\ch -> ch >= '0' && ch <= '9')
    -- The re-exec'd client resolves `hat` on PATH: point it at THIS
    -- build so the hop is deterministic (and never the system hat).
    ambient <- testPath
    let bindir = h.home </> "bin"
    createDirectoryIfMissing True bindir
    createFileLink h.bin (bindir </> "hat")
    c <- startClientEnv h (("PATH", bindir <> ":" <> ambient) : extraEnv) []
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

-- Whether a process id is still alive, via @kill -0@ (no signal, just an
-- existence probe). A reaped process's pid is gone, so this reads false.
pidAlive :: Int -> IO Bool
pidAlive pid = do
    (code, _, _) <- P.readProcessWithExitCode "kill" ["-0", show pid] ""
    pure (code == ExitSuccess)

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

-- Count cells drawn with the reverse-video attribute (how a copy-mode
-- selection renders).
reverseCellCount :: Driver -> IO Int
reverseCellCount d = do
    scr <- Emu.snapshot d.screen
    pure $ length
        [ () | row <- V.toList scr.cells, cell <- V.toList row
             , let Style { reverse = r } = cell.style, r ]

-- Run all actions concurrently, collect their results, and re-raise the
-- first exception. Unlike 'mapConcurrently', a failure cancels nothing:
-- every action runs to completion before it is re-raised.
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

    it "-V prints the version without reaching for a server" $
        P.readProcessWithExitCode hatBin ["-S", "/nonexistent/hat-sock", "-V"] ""
            `shouldReturn` (ExitSuccess, "hat 0.0.1\n", "")

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

        gone <- pollServerGone h.sock 50
        unless gone $ expectationFailure "server did not exit"

    -- 28: a server that served a connection which left without creating a
    -- session must drain, not orphan. start-server autostarts an empty
    -- server (no session, no client stays), so once the control connection
    -- closes there is nothing left to serve.
    it "exits an empty server whose connection made no session (28)" $
        withHat hatBin $ \h -> do
        code <- (\(c, _, _) -> c) <$> hatCtl h ["start-server"]
        code `shouldBe` ExitSuccess
        gone <- pollServerGone h.sock 50
        unless gone $ expectationFailure "empty server did not exit"

    it "exit-unattached ends a server left with only detached sessions" $
        withHat hatBin $ \h -> do
        _ <- hatCtl h ["new-session", "-d", "sleep 300"]
        _ <- hatCtl h ["set-option", "-g", "exit-unattached", "on"]
        gone <- pollServerGone h.sock 50
        unless gone $ expectationFailure "unattached server did not exit"

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
        pid <- serverPid h

        _ <- hatCtl h ["kill-server"]
        gone <- pollPidGone pid 50
        unless gone $
            expectationFailure "server did not exit after kill-server"

    -- A kill escalates to SIGKILL for a SIGHUP-ignoring pane however many
    -- times its window is linked.
    it "kill-server SIGKILLs a SIGHUP-ignoring pane in a linked window" $
        withHat hatBin $ \h -> killsLinkedStubborn h "b:5" ["kill-server"]
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

    -- e7: a pane whose shell exits behind a child still holding the pty open
    -- must be torn down -- pane death tracks the shell, not pty EOF.
    it "closes a pane whose shell exits behind a lingering child (e7)" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        let pidFile = h.home <> "/lingerer.pid"
        -- Background a sleep that inherits the pane's pty on fd 1/2, record its
        -- pid, drop it from the job table, then exit the shell out from under
        -- it: the shell dies but the pty slave stays open.
        typeInto c1 $ B8.pack
            ("sleep 300 & echo $! > " <> pidFile <> "; disown; exit\r")
        lingerer <- read . takeWhile (/= '\n') <$> awaitFile pidFile (not . null)
        -- The pane must end on the shell's exit, so the session ends and this
        -- client is told to leave -- even though the child still holds the pty.
        status <- awaitExit c1
        gone <- pollServerGone h.sock 50
        -- Reap the orphan we intentionally leaked before asserting.
        _ <- P.readProcessWithExitCode "kill" ["-9", show (lingerer :: Int)] ""
        status `shouldBe` Exited ExitSuccess
        unless gone $ expectationFailure
            "pane leaked: server did not exit after its shell exited"

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

    it "hands the client's terminal, cooked, to lock-command until it exits, then redraws" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        typeInto c "echo marker-$((6*7))\r"
        awaitScreen c "marker-42"
        let mode = h.home </> "lock-mode"
            got = h.home </> "lock-got"
        _ <- hatCtl h ["set", "-g", "lock-command"
            , "stty -a | grep -o -- '-*icanon' > " <> mode <> "; read x; echo \"$x\" > " <> got]
        (code, _, err) <- hatCtl h ["lock-client"]
        (code, err) `shouldBe` (ExitSuccess, "")
        awaitFile mode (not . null) `shouldReturn` "icanon\n"
        awaitWith "the lock screen" (fmap (not . T.isInfixOf "marker-42") . screenText) c
        typeInto c "secret\r"
        awaitFile got (not . null) `shouldReturn` "secret\n"
        awaitScreen c "marker-42"
        typeInto c "echo after-$((1+1))\r"
        awaitScreen c "after-2"
        T.isInfixOf "secret" <$> screenText c `shouldReturn` False

    it "a client locked across restart-server rejoins once its lock command exits" $
        withHat hatBin $ \h -> do
        ambient <- testPath
        let bindir = h.home </> "bin"
        createDirectoryIfMissing True bindir
        createFileLink h.bin (bindir </> "hat")
        c <- startClientEnv h [("PATH", bindir <> ":" <> ambient)] []
        awaitScreen c "$"
        typeInto c "echo marker-$((6*7))\r"
        awaitScreen c "marker-42"
        _ <- hatCtl h ["set", "-g", "lock-command", "read x"]
        _ <- hatCtl h ["lock-client"]
        awaitWith "the lock screen" (fmap (not . T.isInfixOf "marker-42") . screenText) c
        _ <- hatCtl h ["restart", h.bin]
        awaitTrue "the reload handover to be consumed" $
            doesFileExist (h.sock <> ".reload.last")
        typeInto c "\r"
        awaitWith "the re-exec'd client to reattach"
            (\_ -> not . null . words <$> ctlOut h ["list-clients"]) c
        awaitScreen c "marker-42"

    -- bug 6: a pager (less) draws on the alternate screen; on exit the primary
    -- buffer must be repainted on the *client*, not just in the emulator, or
    -- the pager's screen bleeds through.
    it "repaints the client when a program exits the alternate screen" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        -- Enter the alt screen, show a marker, and park in `read` so the client
        -- renders the alt screen before we trigger the exit. The markers are
        -- printf-generated (ALT42MARK / BACK99SHELL) so they appear only in the
        -- rendered output, never in the echoed command line.
        typeInto c "printf '\\033[?1049hALT%dMARK' 42; read x; printf '\\033[?1049lBACK%dSHELL\\n' 99\r"
        awaitScreen c "ALT42MARK"
        typeInto c "\r"                 -- satisfy `read`, program exits the alt screen
        awaitScreen c "BACK99SHELL"
        scr <- screenText c
        T.isInfixOf "ALT42MARK" scr `shouldBe` False

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
        typeInto c "seq 1 200 | sed 's/^/ZOOMLINE-/' > zf.txt; less zf.txt\r"
        awaitScreen c "ZOOMLINE-1"
        -- Press h for less's full-screen help overlay (what the screenshot
        -- showed), then zoom via the control path (returns after reconcile).
        typeInto c "h"
        awaitScreen c "SUMMARY OF LESS COMMANDS"
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

    -- tmux prints command errors bare; scripts (tmuxed-alacritty-new)
    -- match on the exact text, e.g. `duplicate session: NAME`. A `hat: `
    -- prefix would break that match, so control errors stay unbranded.
    it "prints control-command errors bare, like tmux" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        _ <- hatCtl h ["new-session", "-d", "-s", "work"]
        (code, _, err) <- hatCtl h ["new-session", "-d", "-s", "work"]
        code `shouldBe` ExitFailure 1
        err `shouldBe` "duplicate session: work\n"
        typeInto c1 "exit\r"
        _ <- awaitExit c1
        pure ()

    it "gives the terminal the cursor-style option's shape, the program's over it" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        let outerStyle want = awaitTrue ("the outer cursor " <> show want) $
                (== want) . (.style) <$> Emu.appCursor c.screen
        _ <- hatCtl h ["set", "-g", "cursor-style", "bar"]
        outerStyle HP.StyleBar
        typeInto c "printf '\\033[4 q'\r"
        outerStyle HP.StyleUnderline
        typeInto c "exit\r"
        _ <- awaitExit c
        pure ()

    it "titles the terminal with set-titles-string under set-titles" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        let outerTitle t = awaitTrue (show t) (B8.isInfixOf ("\ESC]0;" <> t) <$> readIORef c.transcript)
        _ <- hatCtl h ["set", "-g", "set-titles", "on"]
        outerTitle "hat: "
        _ <- hatCtl h ["set", "-g", "set-titles-string", "T:#{window_index}"]
        outerTitle "T:0\BEL"
        typeInto c "exit\r"
        _ <- awaitExit c
        pure ()

    it "reveals an overlay scrollbar on a copy-mode scroll or a hover, then hides it" $
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
        typeInto c "exit\r"
        _ <- awaitExit c
        pure ()

    it "opens a display-menu over the client's window and runs the item its key picks" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        _ <- hatCtl h ["display-menu", "-T", "Pick", "Alpha", "a", "set -g @picked alpha"]
        awaitScreen c "Alpha (a)"
        typeInto c "a"
        awaitTrue "the item's command" $ do
            (_, out, _) <- hatCtl h ["show", "-gv", "@picked"]
            pure (out == "alpha\n")
        awaitWith "the menu to close" (fmap (not . T.isInfixOf "Alpha (a)") . screenText) c

    it "runs a menu item's later commands on the pane an earlier one made" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        _ <- hatCtl h ["display-menu", "Split", "s", "split-window; set -gF @cur '##{pane_id}'"]
        awaitScreen c "Split (s)"
        typeInto c "s"
        awaitTrue "the new pane to be current" $ do
            (_, out, _) <- hatCtl h ["show", "-gv", "@cur"]
            pure (out == "%1\n")

    it "shows a key's display-popup over the client, its command taking keys, until it exits" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        _ <- hatCtl h ["bind", "P", "display-popup", "-E", "printf POP-$((6*7)); read x"]
        typeInto c "\x02P"
        awaitScreen c "POP-42"
        typeInto c "\r"
        awaitWith "the popup to close" (fmap (not . T.isInfixOf "POP-42") . screenText) c

    it "runs a choose-tree -GZw \\; send-keys / binding into a fuzzy search that typing narrows and Enter chooses" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "bind / choose-tree -GZw \\; send-keys /\n"
        c <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c "$"
        _ <- hatCtl h ["new-session", "-d", "-s", "projects", "-n", "notes"]
        _ <- hatCtl h ["new-window", "-d", "-t", "projects:1", "-n", "hat-dev"]
        typeInto c "\x02/"
        awaitScreen c "(search) "
        awaitScreen c "notes"
        typeInto c "hd"
        awaitScreen c "(search) hd"
        awaitWith "the list to narrow to the match"
            (fmap (\t -> "hat-dev" `T.isInfixOf` t && not ("notes" `T.isInfixOf` t)) . screenText) c
        typeInto c "\r"
        awaitTrue "the client to switch to the match" $
            (== "projects:1\n") <$> ctlOut h ["list-clients", "-F", "#{session_name}:#{window_index}"]

    it "switches the client into a key table a binding names with switch-client -T" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        _ <- hatCtl h ["bind", "-T", "move", "y", "set", "-g", "@moved", "yes"]
        typeInto c "\x02g"
        typeInto c "y"
        awaitTrue "the move table's binding to run" $
            (== "yes\n") <$> ctlOut h ["show", "-gv", "@moved"]

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

    it "ends a popup's command across restart-server, leaving nothing behind" $
        withHat hatBin $ \h -> do
        let pidFile = h.home <> "/popup.pid"
        c <- startClient h
        awaitScreen c "$"
        _ <- hatCtl h ["bind", "P", "display-popup", "echo $$ > " <> pidFile <> "; exec sleep 1000"]
        typeInto c "\x02P"
        pid <- read <$> awaitFile pidFile (not . null . words)
        _ <- hatCtl h ["restart-server", h.bin]
        awaitTrue "the handover to be consumed" $
            doesFileExist (h.sock <> ".reload.last")
        awaitTrue "the popup's command to be reaped" (not <$> pidAlive pid)

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

    it "carries buffer bytes verbatim through load-buffer - and save-buffer -" $
        withHat hatBin $ \h -> do
        let bytes = B.pack [0x61, 0x00, 0x1b, 0x5b, 0x31, 0x6d, 0xff, 0xc3, 0xa9, 0x0a]
        _ <- hatCtl h ["new-session", "-d", "-s", "work"]
        (ExitSuccess, _) <- hatBytes h ["load-buffer", "-b", "b", "-"] bytes
        hatBytes h ["save-buffer", "-b", "b", "-"] ""
            `shouldReturn` (ExitSuccess, bytes)

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
        let area = Size { rows = 24, cols = 80 }
            lay1 = emitLayout (fitLayout area (singlePane (PaneId 0)))
            lay2 = emitLayout (fitLayout area
                (nodeOf LeftRight [singlePane (PaneId 0), singlePane (PaneId 1)]))
            snap = Snapshot
                { lastActiveSession = Nothing, sessions =
                    [ SessionSnap
                        { name = "restored", startCwd = "/tmp", currentIx = 0
                        , windowHist = []
                        , createdAt = Nothing, restores = 0
                        , group = Nothing
                        , windows =
                            [ WindowSnap { ix = 0, name = "one", layout = lay1
                                , active = 0, paneHist = []
                                , autoRename = False
                                , key = Nothing, linkOf = Nothing, modal = Nothing, panes = [PaneSnap { cwd = "/tmp", command = Nothing, shellSpawned = False, kind = ProgramPane, inputOff = False, title = Nothing }] }
                            , WindowSnap { ix = 1, name = "two", layout = lay2
                                , active = 0, paneHist = []
                                , autoRename = False
                                , key = Nothing, linkOf = Nothing, modal = Nothing, panes = [PaneSnap { cwd = "/tmp", command = Nothing, shellSpawned = False, kind = ProgramPane, inputOff = False, title = Nothing }
                                          , PaneSnap { cwd = "/tmp", command = Nothing, shellSpawned = False, kind = ProgramPane, inputOff = False, title = Nothing }] }
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

    -- A restored tree comes back at its captured area, not 24x80 (bug 4b).
    it "restores the saved tree at its captured area before any client attaches" $
        withHatPersist hatBin $ \h -> do
        let lay = emitLayout (fitLayout (Size { rows = 30, cols = 100 }) (singlePane (PaneId 0)))
            snap = Snapshot
                { lastActiveSession = Nothing, sessions =
                    [ SessionSnap
                        { name = "sized", startCwd = "/tmp", currentIx = 0
                        , windowHist = []
                        , createdAt = Nothing, restores = 0
                        , group = Nothing
                        , windows =
                            [ WindowSnap
                                { ix = 0, name = "one", layout = lay
                                , active = 0, paneHist = []
                                , autoRename = False
                                , key = Nothing, linkOf = Nothing, modal = Nothing, panes = [PaneSnap { cwd = "/tmp", command = Nothing, shellSpawned = False, kind = ProgramPane, inputOff = False, title = Nothing }] }
                            ] } ] }
        createDirectoryIfMissing True (takeDirectory (storeOf h))
        Persist.withStore (storeOf h) $ \c -> Persist.saveSnapshot c snap

        -- start-server autostarts the server and only returns once startup
        -- (including the restore) has landed; no client ever attaches.
        _ <- hatCtl h ["start-server"]
        out <- ctlOut h ["list-panes", "-a", "-F", "#{pane_width}x#{pane_height}"]
        out `shouldBe` "100x30\n"

    it "forgets the saved tree when the last window exits (pristine restart)" $
        withHatPersist hatBin $ \h -> do
        -- Seed a saved tree so the autostarted server restores it.
        let lay = emitLayout (fitLayout (Size { rows = 24, cols = 80 }) (singlePane (PaneId 0)))
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
                                , key = Nothing, linkOf = Nothing, modal = Nothing, panes = [PaneSnap { cwd = "/tmp", command = Nothing, shellSpawned = False, kind = ProgramPane, inputOff = False, title = Nothing }] }
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
        gone <- pollServerExited h 50
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

    -- bb: snapshot history is listable, and any generation restores by id.
    it "lists snapshot generations and restores one by id" $
        withHatPersist hatBin $ \h -> do
        -- Seed a saved tree, as a previous run's kill-server would have.
        let lay = emitLayout (fitLayout (Size { rows = 24, cols = 80 }) (singlePane (PaneId 0)))
            snap = Snapshot
                { lastActiveSession = Nothing, sessions =
                    [ SessionSnap
                        { name = "bbsess", startCwd = "/tmp", currentIx = 0
                        , windowHist = []
                        , createdAt = Nothing, restores = 0
                        , group = Nothing
                        , windows =
                            [ WindowSnap
                                { ix = 0, name = "one", layout = lay
                                , active = 0, paneHist = []
                                , autoRename = False
                                , key = Nothing, linkOf = Nothing, modal = Nothing, panes = [PaneSnap { cwd = "/tmp", command = Nothing, shellSpawned = False, kind = ProgramPane, inputOff = False, title = Nothing }] }
                            ] } ] }
        createDirectoryIfMissing True (takeDirectory (storeOf h))
        Persist.withStore (storeOf h) $ \c -> Persist.saveSnapshot c snap

        -- Autostart restores the tree, archiving it as generation 1.
        c1 <- startClient h
        awaitScreen c1 "$"
        listed <- ctlOut h ["list-snapshots"]
        lines listed `shouldSatisfy` any ("1: " `List.isPrefixOf`)

        -- Restoring it again collides with the live restored session, so
        -- the copy comes back under a fresh name.
        out <- ctlOut h ["restore-snapshot", "1"]
        out `shouldContain` "as 'bbsess-2'"
        names <- ctlOut h ["list-sessions", "-F", "#{session_name}"]
        lines names `shouldSatisfy`
            (\ns -> "bbsess" `elem` ns && "bbsess-2" `elem` ns)

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
        -- Bug 5: navigating away from a zoomed pane cancels the zoom and
        -- moves to the neighbor. Previously select-pane did nothing while
        -- zoomed (the collapsed arrangement hid every other pane), so the
        -- border stayed gone; now the border returns as the zoom is undone.
        typeInto c1 "\x02\ESC[D"   -- prefix + Left = select-pane -L
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
    it "break-pane numbers the new window from base-index" $
        withHat hatBin $ \h -> do
        let confPath = h.home <> "/hat.conf"
        writeFile confPath "set -g base-index 1\n"
        c1 <- startClientArgs h ["-f", confPath]
        awaitScreen c1 "1:sh*"
        typeInto c1 "\x02%"                 -- split -h; new pane active
        awaitScreen c1 "\x2502"
        _ <- ctlOut h ["break-pane"]
        out <- ctlOut h ["list-windows", "-F", "#{window_index}"]
        List.sort (filter (not . null) (lines out)) `shouldBe` ["1", "2"]

    -- Bug: a pane's reader captures the window it was spawned in, but
    -- break-pane re-parents the live pane. When its program then exits, the
    -- reader must reap it from its CURRENT window (1), not the split it came
    -- from (0) — otherwise the broken-out window lingers with a dead pane.
    it "a broken-out pane's window closes when its program exits" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        typeInto c1 "\x02%"                 -- split -h; new pane active
        awaitScreen c1 "\x2502"
        _ <- ctlOut h ["break-pane"]        -- move it into window 1 (now current)
        awaitScreen c1 "1:sh*"
        typeInto c1 "exit\r"                 -- end the broken-out pane's shell
        awaitWith "window 1 collapsed, back on window 0" (\d -> do
            t <- screenText d
            -- Match the window-1 label ("1:sh"), not a bare "1:", so the
            -- status-bar clock (e.g. 11:52) can't spoof the "still there" check.
            pure (not ("1:sh" `T.isInfixOf` t) && "0:sh*" `T.isInfixOf` t)) c1

    -- A joined pane's keystroke echo must repaint on its own; status-interval
    -- 0 removes the clock tick that would otherwise eventually flush it.
    it "repaints a joined pane's output in its new window" $
        withHat hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g status-interval 0\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh*"
        _ <- ctlOut h ["new-window"]        -- window 1, pane %1
        awaitScreen c1 "1:sh*"
        _ <- ctlOut h ["select-window", "-t", "0"]
        awaitScreen c1 "0:sh*"
        _ <- ctlOut h ["join-pane", "-h", "-s", "%1"]
        awaitScreen c1 "\x2502"             -- the joined pane arrived (active)
        typeInto c1 "joined-output"
        awaitScreen c1 "joined-output"

    -- The drop path (a bare shell that never enabled ?1004) is covered by
    -- the pure 'deliversKey' matrix in SessionSpec plus EmulatorSpec's
    -- default focusReport=False; this keeps the real-pty wiring for the
    -- harder forward direction.
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
        meta0 <- ctlOut h
            ["list-sessions", "-F", "#{session_created} #{session_restores}"]
        [created0, "0"] <- pure (words meta0)

        -- Kill the server; the tree is saved automatically on the way out.
        _ <- ctlOut h ["kill-server"]
        _ <- awaitExit c1
        gone <- pollServerExited h 50
        unless gone $ expectationFailure "server did not die"

        -- Restart: a fresh client autostarts a new server, which restores
        -- the saved tree before we attach -- no manual replay needed.
        c2 <- startClient h
        awaitScreen c2 "0:built0"
        awaitScreen c2 "1:editor"
        -- Session identity rides along: same creation stamp, one restore.
        meta1 <- ctlOut h
            ["list-sessions", "-F", "#{session_created} #{session_restores}"]
        words meta1 `shouldBe` [created0, "1"]

    -- See Note [Captured links].
    it "restores a linked window and a session group as shared, not copied" $
        withHatPersist hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "0:sh*"
        _ <- ctlOut h ["new-window", "-d", "-t", "0:1", "-n", "shared"]
        _ <- ctlOut h ["new-session", "-d", "-s", "other"]
        _ <- ctlOut h ["link-window", "-d", "-s", "0:1", "-t", "other:5"]
        _ <- ctlOut h ["new-session", "-d", "-s", "grp", "-t", "0"]
        let probe = do
                linked <- ctlOut h ["display-message", "-p", "-t", "other:5"
                    , "#{window_linked_sessions_list}"]
                grouped <- ctlOut h ["display-message", "-p", "-t", "grp:"
                    , "#{session_group_list}"]
                panes <- ctlOut h ["list-panes", "-a", "-F", "#{pane_id}"]
                pure (lines linked, lines grouped, length (lines panes))
        saved <- probe
        saved `shouldBe` (["0,other,grp"], ["0,grp"], 6)

        _ <- ctlOut h ["kill-server"]
        _ <- awaitExit c1
        gone <- pollServerExited h 50
        unless gone $ expectationFailure "server did not die"

        c2 <- startClient h
        awaitScreen c2 "shared"
        probe `shouldReturn` saved

    -- b7: a window's automatic-rename status must survive save/restore, in
    -- both directions -- an auto-renaming window keeps tracking its pane, a
    -- manually-named window stays pinned.
    it "restores each window's automatic-rename status" $
        withHatPersist hatBin $ \h -> do
        writeFile (h.home <> "/hat.conf") "set -g automatic-rename on\n"
        c1 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c1 "0:sh*"
        -- Window 0 stays automatic; window 1 is pinned by rename-window,
        -- which disables its automatic-rename.
        _ <- ctlOut h ["new-window", "-d", "-t", "1"]
        awaitScreen c1 "1:sh"
        _ <- ctlOut h ["rename-window", "-t", "1", "pinned"]
        awaitScreen c1 "1:pinned"
        flags1 <- ctlOut h ["list-windows", "-F", "#{automatic_rename}"]
        lines flags1 `shouldBe` ["1", "0"]

        _ <- ctlOut h ["kill-server"]
        _ <- awaitExit c1
        gone <- pollServerExited h 50
        unless gone $ expectationFailure "server did not die"

        c2 <- startClientArgs h ["-f", h.home <> "/hat.conf"]
        awaitScreen c2 "1:pinned"
        flags2 <- ctlOut h ["list-windows", "-F", "#{automatic_rename}"]
        lines flags2 `shouldBe` ["1", "0"]

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
        bash <- bashPath
        P.callProcess "cp" [bash, wrapped]
        writeExecutable (bin </> "vimish") $ unlines
            [ "#!" <> bash
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

    it "copy-pipe does not freeze the client when the command leaves a child holding stdout" $
        withHat hatBin $ \h -> do
        -- xclip forks a selection-owner daemon that inherits and never closes
        -- the command's stdout. If copy-pipe reads that stdout to EOF on the
        -- client's input thread, the thread blocks forever and the client
        -- goes dead. 'sleep' backgrounded in a subshell reproduces the leak.
        writeFile (h.home <> "/hat.conf")
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

    -- Pins the structural reap: the pipe-pane child is owned as a scoped
    -- sub-resource, so stopping the pipe — and, on the teardown path, killing
    -- the pane out from under it — reaps the child rather than orphaning it.
    it "pipe-pane reaps its subprocess on stop and on pane kill" $
        withHat hatBin $ \h -> do
        c1 <- startClient h
        awaitScreen c1 "$"
        let spawnPipe path = ctlOut h
                ["pipe-pane", "echo $$ > " <> path <> "; cat > /dev/null"]
            readPid path =
                read . takeWhile (/= '\n') <$> awaitFile path (not . null)

        -- Stop path: no-arg pipe-pane cancels the supervisor, reaping the child.
        let pidPath1 = h.home <> "/pipe1.pid"
        _ <- spawnPipe pidPath1
        pid1 <- readPid pidPath1
        pidAlive pid1 `shouldReturn` True
        _ <- ctlOut h ["pipe-pane"]
        awaitReaped pid1

        -- Teardown path: killing the pane runs reapPane -> stopPipe, so a live
        -- pipe child is reaped as the pane dies, not left orphaned.
        let pidPath2 = h.home <> "/pipe2.pid"
        _ <- spawnPipe pidPath2
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

    it "remain-on-exit keeps an exited pane, dead, with its status" $
        withHat hatBin $ \h -> do
        _ <- hatCtl h
            [ "set", "-g", "remain-on-exit", "on", ";"
            , "new", "-d", "-x", "60", "-y", "5", "echo DEADBEACON; exit 3" ]
        awaitTrue "the pane to die" $
            (== "1 3\n") <$> ctlOut h
                ["display", "-p", "#{pane_dead} #{pane_dead_status}"]
        screen <- ctlOut h ["capture-pane", "-p", "-S", "-"]
        screen `shouldSatisfy` List.isInfixOf "DEADBEACON"
        screen `shouldSatisfy` List.isInfixOf "Pane is dead (status 3, "

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

    it "respawn-pane reruns a dead pane's command, and needs -k when live" $
        withHat hatBin $ \h -> do
        let runs = h.home <> "/runs"
        _ <- hatCtl h
            [ "set", "-g", "remain-on-exit", "on", ";"
            , "new", "-d", "echo run >> " <> runs ]
        let dead = ctlOut h ["display", "-p", "#{pane_dead}"]
        awaitTrue "the pane to die" ((== "1\n") <$> dead)
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

    it "respawn-window -k reduces the window to one respawned pane" $
        withHat hatBin $ \h -> do
        _ <- hatCtl h ["new", "-d", "sleep 100", ";", "split-window", "-d"]
        ctlOut h ["display", "-p", "#{window_panes}"] `shouldReturn` "2\n"
        (_, _, err) <- hatCtl h ["respawn-window", "true"]
        err `shouldBe` "respawn window failed: window 0:0 still active\n"
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

    it "restart-server keeps a dead pane dead, and its command respawnable" $
        withHat hatBin $ \h -> do
        let runs = h.home <> "/runs"
        _ <- hatCtl h
            [ "set", "-g", "remain-on-exit", "on", ";"
            , "new", "-d", "echo run >> " <> runs <> "; exit 7" ]
        let deadStatus = ctlOut h
                ["display", "-p", "#{pane_dead} #{pane_dead_status}"]
        awaitTrue "the pane to die" ((== "1 7\n") <$> deadStatus)
        _ <- hatCtl h ["restart-server", h.bin]
        awaitTrue "the handover to be consumed" $
            doesFileExist (h.sock <> ".reload.last")
        deadStatus `shouldReturn` "1 7\n"
        ctlOut h ["capture-pane", "-p"]
            >>= (`shouldSatisfy` List.isInfixOf "Pane is dead (status 7, ")
        _ <- hatCtl h ["respawn-pane"]
        _ <- awaitFile runs ((== 2) . length . lines)
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

    -- A spawned client must not inherit the driver's open fds (bug 9).
    it "spawns clients with a clean fd table" $
        withHat hatBin $ \h -> do
        (r, w) <- createPipe  -- marker fd held open across the spawn
        flip finally (closeFd r >> closeFd w) $ do
            c <- startClient h
            awaitScreen c "$"
            mpid <- P.getPid c.pty.proch
            pid <- maybe (fail "client already gone") pure mpid
            marker <- readSymbolicLink ("/proc/self/fd/" <> show w)
            fds <- listDirectory ("/proc/" <> show pid <> "/fd")
            links <- forM fds $ \fd ->
                readSymbolicLink ("/proc/" <> show pid <> "/fd/" <> fd)
                    `catch` \(_ :: SomeException) -> pure ""
            links `shouldNotSatisfy` elem marker

    -- Milestone A: `restart-server` reloads the server binary in place by
    -- re-exec'ing itself and re-adopting the still-running pane programs
    -- rather than respawning them. The shell's pid is the identity that must
    -- survive the swap; the client is dropped and the user reattaches.
    it "restart-server reloads in place, keeping the pane's program alive" $
        withHat hatBin $ \h -> do
        let digits = filter (\ch -> ch >= '0' && ch <= '9')
        c <- startClient h
        awaitScreen c "$"
        typeInto c "echo before-reload\r"
        awaitScreen c "before-reload"
        pidBefore <- digits <$> ctlOut h ["list-panes", "-F", "#{pane_pid}"]
        pidBefore `shouldNotBe` ""
        -- Reload in place; the re-exec drops our client. Pass this build's
        -- path explicitly so the reload is deterministic — a bare
        -- restart-server would resolve `hat` on PATH and hijack the system
        -- install (a different, reload-unaware binary).
        _ <- hatCtl h ["restart-server", h.bin]
        _ <- awaitExit c
        -- Reattach: the rebuilt session waits out the reload's Restoring phase.
        c2 <- startClient h
        pidAfter <- digits <$> ctlOut h ["list-panes", "-F", "#{pane_pid}"]
        -- Same process — the shell never died across the binary swap.
        pidAfter `shouldBe` pidBefore
        -- And it still runs our input.
        typeInto c2 "echo after-reload\r"
        awaitScreen c2 "after-reload"

    -- Milestone B: `restart-server` preserves a pane's live screen. A
    -- full-screen program is drawn into the alt screen, then blocks in `cat`
    -- so nothing redraws it; after the reload and reattach the content is still
    -- there only because the emulator's screen was captured and replayed. The
    -- octal escapes keep a literal ESC out of the shell's line editor.
    it "restart-server preserves a full-screen program's live display" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        typeInto c "printf '\\033[?1049h\\033[2J\\033[HALTSCREEN-BEACON'; cat\r"
        awaitScreen c "ALTSCREEN-BEACON"
        _ <- hatCtl h ["restart-server", h.bin]
        _ <- awaitExit c
        c2 <- startClient h
        -- `cat` never redrew: the beacon survives only because the reload
        -- restored the alt-screen grid.
        awaitScreen c2 "ALTSCREEN-BEACON"
        -- The consumed handover is kept as .last: it is the exact reproducer
        -- when a resume crashes natively, so it must survive the reload.
        doesFileExist (h.sock <> ".reload.last") `shouldReturn` True

    -- Reloading a server that was ITSELF reloaded: the pane's emulator was
    -- rebuilt by the first reload's replay, and adopting that rebuilt state a
    -- second time must not hang (the production double-reload that fell back to
    -- a respawn from disk, losing the live programs).
    it "restart-server twice in a row keeps a full-screen pane alive" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        typeInto c "printf '\\033[?1049h\\033[2J\\033[HBEACON-ONE'; cat\r"
        awaitScreen c "BEACON-ONE"
        _ <- hatCtl h ["restart-server", h.bin]
        _ <- awaitExit c
        c2 <- startClient h
        awaitScreen c2 "BEACON-ONE"
        _ <- hatCtl h ["restart-server", h.bin]
        _ <- awaitExit c2
        c3 <- startClient h
        awaitScreen c3 "BEACON-ONE"

    -- Bug 8a: only the fds a reload names may cross the execve; the log handle
    -- stands in for every fd the successor image reopens for itself.
    it "restart-server leaves the fds it does not name behind" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        _ <- hatCtl h ["restart-server", h.bin]
        _ <- awaitExit c
        c2 <- startClient h
        awaitScreen c2 "$"
        fdsOn h (h.home </> "server.log") `shouldReturn` 1

    -- Bug cb: a reload must not forget the alternate session, so a reattached
    -- client's `switch-client -l` still toggles back to where it came from. The
    -- `[#S]` status-left names the current session, so it is the observable.
    it "restart-server preserves the alternate session (last-session)" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "[0]"                -- attached to the first session (named "0")
        -- Switch this client to a new session; the first becomes its alternate.
        typeInto c "\x02:"
        awaitPromptOpen c
        typeInto c "new-session -s work\r"
        awaitScreen c "[work]"
        -- Reload in place (deterministic binary path) and reattach.
        _ <- hatCtl h ["restart-server", h.bin]
        _ <- awaitExit c
        c2 <- startClient h
        -- Reattaches to the current session (work), not the alternate.
        awaitScreen c2 "[work]"
        -- switch-client -l must return to the first session — the alternate
        -- survived the reload.
        typeInto c2 "\x02:"
        awaitPromptOpen c2
        typeInto c2 "switch-client -l\r"
        awaitScreen c2 "[0]"

    -- `restart` = restart-server + restart-client: the attached client
    -- re-execs and reattaches instead of exiting, so one command upgrades
    -- both halves (bug 4f). The pane pid pins the server half; the same pty
    -- client still delivering input pins the client half.
    it "restart reloads the server and the attached client stays attached" $
        withHat hatBin (restartKeepsClient [])

    -- 83: a farewelled client's reattach must wait in the listen backlog for
    -- the next image, even when the old image lingers before its exec.
    it "restart reattaches the client across a lingering old image (83)" $
        withHat hatBin
            (restartKeepsClient [("HAT_TEST_RELOAD_LINGER", "200000")])

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
        let confPath = h.home <> "/hat.conf"
        writeFile confPath "if 'sleep 0.7' 'set -g escape-time 0'\n"
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
        _ <- hatCtl h ["restart", "-C", h.bin]
        awaitTrue "the reload handover to be consumed" $
            doesFileExist (h.sock <> ".reload.last")
        awaitWith "each re-exec'd client to rejoin its own session"
            (\_ -> ownSessions) c1

    -- A typo'd binary path must be caught before anything is torn down, so the
    -- running session is untouched rather than half-dropped.
    it "restart-server rejects a missing binary without dropping the session" $
        withHat hatBin $ \h -> do
        c <- startClient h
        awaitScreen c "$"
        (_, out, err) <- hatCtl h ["restart-server", "/no/such/hat"]
        (out <> err) `shouldSatisfy` List.isInfixOf "no such binary"
        -- the shell is still live: it runs our input as if nothing happened.
        typeInto c "echo survived-bad-reload\r"
        awaitScreen c "survived-bad-reload"

    it "a client that stops reading stalls no pane for anyone else" $
        withHat hatBin $ \h -> do
        go <- alertStorm h
        unread <- attachUnread h
        writeFile go ""
        awaitAlertStorm h
        N.close unread.sock

    it "restart-server completes while an attached client is not reading" $
        withHat hatBin $ \h -> do
        go <- alertStorm h
        unread <- attachUnread h
        writeFile go ""
        awaitAlertStorm h
        pidBefore <- ctlOut h ["list-panes", "-F", "#{pane_pid}"]
        done <- timeout 20_000_000 (hatCtl h ["restart-server", h.bin])
        isJust done `shouldBe` True
        awaitTrue "the reload handover to be consumed" $
            doesFileExist (h.sock <> ".reload.last")
        -- Let go, not orphaned: once it reads again its stream ends.
        timeout 10_000_000 (readToEnd unread) `shouldReturn` Just ()
        unsent <- filter (List.isInfixOf "FarewellUnsent") . lines
            <$> readFile (h.home </> "server.log")
        length unsent `shouldBe` 1
        ctlOut h ["list-panes", "-F", "#{pane_pid}"] `shouldReturn` pidBefore
        N.close unread.sock

    it "fails at once, as tmux does, on a socket path too long for an address" $
        withHat hatBin $ \h -> do
        let long = h.home </> replicate 120 'x'
        timeout 5_000_000 (hatCtl h ["-S", long, "new-session", "-d"]) `shouldReturn`
            Just (ExitFailure 1, "", "error connecting to " <> long <> " (File name too long)\n")

    it "logs a connection that closes before its hello as such, not as a crash" $
        withHat hatBin $ \h -> do
        _ <- ctlOut h ["new-session", "-d"]
        serverUp h.sock `shouldReturn` True
        logged <- awaitFile (h.home </> "server.log") $ \l ->
            any (`List.isInfixOf` l) ["ClosedBeforeHello", "ServerCrash"]
        filter (List.isInfixOf "ServerCrash") (lines logged) `shouldBe` []
        filter (List.isInfixOf "ClosedBeforeHello") (lines logged) `shouldSatisfy` (not . null)

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
        , termCaps = Nothing }
    re <- W.newReadEnd sock
    W.recvMessage re >>= \case
        Just (W.Known (W.Welcome _)) -> pure re
        other -> fail ("attachUnread: " <> show other)

-- Read a connection until the server ends it.
readToEnd :: W.ReadEnd -> IO ()
readToEnd re = loop `catch` \(_ :: IOException) -> pure ()
  where
    loop = W.recvMessage re >>= \case
        Just (W.Known (_ :: W.ServerToClient)) -> loop
        Just (W.UnknownTag _) -> loop
        _ -> pure ()

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
        `finally` P.readProcessWithExitCode
            "kill" ["-9", show (stubborn :: Int)] ""

pollServerGone :: FilePath -> Int -> IO Bool
pollServerGone _ 0 = pure False
pollServerGone path n = do
    up <- serverUp path
    if up
        then threadDelay 100_000 *> pollServerGone path (n - 1)
        else pure True

-- | Poll (every 100ms, @n@ times) until no server process remains on @h@'s
-- socket. kill-server unlinks the socket before it replies, so a vanished
-- socket alone no longer shows the process has exited.
pollServerExited :: Hat -> Int -> IO Bool
pollServerExited _ 0 = pure False
pollServerExited h n = do
    pids <- serverPids h
    if null (words pids)
        then pure True
        else threadDelay 100_000 *> pollServerExited h (n - 1)

-- | Poll (every 100ms, @n@ times) until process @pid@ has exited (a zombie
-- counts as exited). Unlike 'pollServerGone', a server that stopped
-- listening but is still draining counts as alive.
pollPidGone :: Int -> Int -> IO Bool
pollPidGone _ 0 = pure False
pollPidGone pid n = do
    stat <- (Just . B8.unpack <$> B.readFile ("/proc/" <> show pid <> "/stat"))
        `catch` \(_ :: IOException) -> pure Nothing
    -- The state field follows the parenthesised command name.
    let alive = case drop 2 . dropWhile (/= ')') <$> stat of
            Just (st : _) -> st /= 'Z'
            _ -> False
    if alive
        then threadDelay 100_000 *> pollPidGone pid (n - 1)
        else pure True

-- | Whether a server answers at the path. The probe is closed before
-- returning: an open probe counts as an active connection and holds the
-- server out of its idle exit.
serverUp :: FilePath -> IO Bool
serverUp path = connectTo path >>= \case
    Left _ -> pure False
    Right sock -> True <$ N.close sock
