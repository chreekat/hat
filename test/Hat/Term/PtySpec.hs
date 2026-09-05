module Hat.Term.PtySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.ByteString.Char8 qualified as B8
import Data.List (isPrefixOf)
import Data.Text qualified as T
import System.Directory
    (doesDirectoryExist, listDirectory, removeDirectoryRecursive, removePathForcibly)
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName)
import System.Posix.IO (FdOption (CloseOnExec), closeFd, createPipe, dup, setFdOption)
import System.Posix.Process (getProcessID)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (Fd (..))
import System.Process (callProcess)
import Test.Hspec
import Test.Hspec.Runner (defaultConfig, evalSpec)

import Hat.Geometry
import Hat.Term.Pty
import Hat.TestSupport (bashPath)

-- The test shells get a throwaway HOME: an interactive bash saves (and,
-- at its default 500-line HISTFILESIZE, truncates!) $HOME/.bash_history
-- on exit, so pointing them at the real home would eat the user's
-- shell history a few lines at a time.
baseSpawn :: FilePath -> Spawn
baseSpawn home = Spawn
    { cmd = "/bin/sh"
    , args = []
    , env =
        [ ("PATH", "/run/current-system/sw/bin:/usr/bin:/bin")
        , ("TERM", "dumb")
        , ("HOME", home)
        ]
    , cwd = Nothing
    , size = Size { rows = 24, cols = 80 }
    }

-- Read until the pty hits EOF, collecting everything.
drainPty :: PtyHandle -> IO B8.ByteString
drainPty pty = go []
  where
    go acc = do
        chunk <- readPty pty
        if B8.null chunk
            then pure (B8.concat (reverse acc))
            else go (chunk : acc)

-- Read until the accumulated output contains the needle (or EOF).
readUntil :: PtyHandle -> B8.ByteString -> IO B8.ByteString
readUntil pty needle = go B8.empty
  where
    go acc
        | needle `B8.isInfixOf` acc = pure acc
        | otherwise = do
            chunk <- readPty pty
            if B8.null chunk then pure acc else go (acc <> chunk)

-- Poll an IO action up to @n@ times (20ms apart) until it satisfies the
-- predicate, returning the last value seen.
retryFor :: Int -> IO a -> (a -> Bool) -> IO a
retryFor n act ok = do
    v <- act
    if ok v || n <= 0
        then pure v
        else threadDelay 20000 >> retryFor (n - 1) act ok

-- One test's pty, always closed. 'closePty' tolerates a child that has
-- already exited, so a test may still close early to make one exit.
withPty :: Spawn -> (PtyHandle -> IO a) -> IO a
withPty s = bracket (spawn s) closePty

-- One test's throwaway HOME and the spawn pointed at it.
data TestShell = TestShell
    { home :: FilePath
    , base :: Spawn
    }

-- The mkdtemp template for a throwaway HOME. The pid keeps a concurrent
-- suite's dirs out of 'testShellHomes'.
homeTemplate :: IO FilePath
homeTemplate = do
    self <- getProcessID
    pure ("/tmp/hat-pty-home-" <> show self <> "-")

-- Hands an item a throwaway HOME that dies with it.
withTestShell :: (TestShell -> IO a) -> IO a
withTestShell act = do
    template <- homeTemplate
    bracket (mkdtemp template) removePathForcibly $ \home ->
        act TestShell { home, base = baseSpawn home }

-- This run's throwaway HOMEs on disk right now.
testShellHomes :: IO [FilePath]
testShellHomes = do
    template <- homeTemplate
    filter (takeFileName template `isPrefixOf`)
        <$> listDirectory (takeDirectory template)

spec :: Spec
spec = do
    describe "parseCmdline" $ do
        it "splits NUL-separated argv, preserving spaces within an argument" $
            parseCmdline "vim\NULFoo Bar.txt\NUL"
                `shouldBe` Just ["vim", "Foo Bar.txt"]

    describe "the test shells' throwaway HOME" $ do
        it "is not created by building the spec (bug 13)" $ do
            existing <- testShellHomes
            _ <- evalSpec defaultConfig spec
            current <- testShellHomes
            current `shouldMatchList` existing

        it "is gone once the item holding it returns (bug 13)" $ do
            dir <- withTestShell (\sh -> pure sh.home)
            doesDirectoryExist dir `shouldReturn` False

    around withTestShell shellSpec

shellSpec :: SpecWith TestShell
shellSpec = do
    it "reads a live pane's foreground argv from /proc" $ \TestShell{base} -> do
        argv <- withPty base { args = ["-c", "exec sleep 30"] } $ \pty -> do
            argv <- retryFor 50 (foregroundArgv pty) (== Just ["sleep", "30"])
            closePty pty
            argv <$ waitExit pty
        argv `shouldBe` Just ["sleep", "30"]

    it "keeps the test shells' HOME away from the real one" $ \TestShell{home, base} -> do
        realHome <- getEnv "HOME"
        out <- withPty base { args = ["-c", "echo home=$HOME"] } $ \pty ->
            drainPty pty <* waitExit pty
        out `shouldSatisfy` B8.isInfixOf (B8.pack ("home=" <> home))
        home `shouldNotBe` realHome

    it "spawns a shell and captures its output" $ \TestShell{base} -> do
        (out, status) <- withPty base { args = ["-c", "echo hat-pty-works"] } $
            \pty -> (,) <$> drainPty pty <*> waitExit pty
        out `shouldSatisfy` B8.isInfixOf "hat-pty-works"
        status `shouldBe` Just (Exited ExitSuccess)

    it "sets the initial window size on the pty" $ \TestShell{base} -> do
        out <- withPty base
            { args = ["-c", "stty size"]
            , size = Size { rows = 30, cols = 100 }
            } $ \pty -> drainPty pty <* waitExit pty
        out `shouldSatisfy` B8.isInfixOf "30 100"

    it "propagates resize to the child" $ \TestShell{base} -> do
        out <- withPty base { args = ["-c", "read _line; stty size"] } $ \pty -> do
            resize pty Size { rows = 40, cols = 120 }
            writePty pty "\n"
            drainPty pty <* waitExit pty
        out `shouldSatisfy` B8.isInfixOf "40 120"

    it "runs the child in the given working directory" $ \TestShell{base} -> do
        out <- withPty base { args = ["-c", "pwd"], cwd = Just "/tmp" } $ \pty ->
            drainPty pty <* waitExit pty
        out `shouldSatisfy` B8.isInfixOf "/tmp"

    -- M0 demo: a live shell driven interactively over the pty.
    it "round-trips an interactive shell session" $ \TestShell{base} -> do
        (out, status) <- withPty base $ \pty -> do
            writePty pty "echo a$((1+1))b\n"
            out <- readUntil pty "a2b"
            writePty pty "exit\n"
            (,) out <$> waitExit pty
        out `shouldSatisfy` B8.isInfixOf "a2b"
        status `shouldBe` Just (Exited ExitSuccess)

    -- The foreground command follows a child running under the shell, not
    -- the shell itself: 'sleep' owns the terminal's foreground process
    -- group while it runs.
    it "reports the pane's foreground command, not the shell" $ \TestShell{base} -> do
        cmd <- withPty base $ \pty -> do
            writePty pty "sleep 5\n"
            retryFor 100 (foregroundCommand pty) (== Just (T.pack "sleep"))
        cmd `shouldBe` Just (T.pack "sleep")

    -- NixOS wrappers exec the real binary as @.<name>-wrapped@ but keep
    -- the public name in argv[0] (bash's @exec -a@). The foreground
    -- command must be argv[0] — what tmux reports — not the executable's
    -- comm, which leaks the wrapper decoration (and truncates at 15
    -- bytes besides).
    it "reports a wrapped foreground command by argv[0], not its comm" $
        \TestShell{base} -> bracket (mkdtemp "/tmp/hat-pty-")
            removeDirectoryRecursive $ \dir -> do
            -- A copy of bash stands in for the real binary: unlike the
            -- coreutils multi-call binary, it does not dispatch on
            -- argv[0]. The trailing `:` stops bash exec'ing the sleep,
            -- keeping the wrapped process the foreground group leader.
            -- Copied by a cp subprocess, not copyFile: a write fd held in
            -- this (forking, parallel) test process leaks into fork→exec
            -- windows and makes the exec below flake with ETXTBSY.
            let wrapped = dir <> "/.sleepish-wrapped"
            bash <- bashPath
            callProcess "cp" [bash, wrapped]
            cmd <- withPty base $ \pty -> do
                writePty pty (B8.pack
                    ("exec " <> bash <> " -c \"exec -a sleepish "
                        <> wrapped <> " -c 'sleep 5; :'\"\n"))
                retryFor 100 (foregroundCommand pty)
                    (== Just (T.pack "sleepish"))
            cmd `shouldBe` Just (T.pack "sleepish")

    -- An orphaned pane that kept hat's listening socket open outlived
    -- `pkill hat` and made the next start's connect() hang. The child
    -- must inherit only its stdio: a forced-inheritable fd here must not
    -- survive into it.
    it "hands the pane child a clean fd table" $ \TestShell{base} ->
        bracket createPipe (\(r, w) -> closeFd r >> closeFd w) $ \(_, w) -> do
            setFdOption w CloseOnExec False
            let Fd n = w
            out <- withPty base
                { args = ["-c", "[ -e /proc/self/fd/" ++ show n
                                 ++ " ] && echo LEAK || echo CLEAN"] } $ \pty ->
                drainPty pty <* waitExit pty
            out `shouldSatisfy` B8.isInfixOf "CLEAN"
            out `shouldNotSatisfy` B8.isInfixOf "LEAK"

    it "reports a nonzero exit" $ \TestShell{base} -> do
        status <- withPty base { args = ["-c", "exit 3"] } $ \pty ->
            drainPty pty *> waitExit pty
        status `shouldBe` Just (Exited (ExitFailure 3))

    -- Milestone A: the post-exec image re-adopts a pane it inherited across
    -- a self-exec reload — an already-open master fd and its running child —
    -- rather than spawning a fresh one. The adopted handle must read and
    -- write the same live child.
    it "adopts an inherited pty fd and child, and drives it" $ \TestShell{base} -> do
        out <- withPty base $ \pty -> do
            -- Production adopts a master fd inherited across exec, with no
            -- other Handle on it. Duplicate the fd here so the adopted handle
            -- owns a distinct one rather than a second GHC Handle racing the
            -- original over the same fd's IO-manager registration.
            dupd <- dup (masterFd pty)
            bracket (adopt dupd (pid pty)) closePty $ \pty2 -> do
                writePty pty2 "echo a$((3+4))b\n"
                out <- readUntil pty2 "a7b"
                writePty pty2 "exit\n"
                _ <- drainPty pty2
                out <$ waitExit pty
        out `shouldSatisfy` B8.isInfixOf "a7b"
