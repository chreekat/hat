module Hat.Term.PtySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import System.Directory (removeDirectoryRecursive, removePathForcibly)
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import System.Posix.IO (FdOption (CloseOnExec), closeFd, createPipe, setFdOption)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (Fd (..))
import System.Process (callProcess)
import Test.Hspec

import Hat.Geometry
import Hat.Term.Pty

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

spec :: Spec
spec = do
    home <- runIO (mkdtemp "/tmp/hat-pty-home-")
    let base = baseSpawn home
    afterAll_ (removePathForcibly home) $ specWith base home

specWith :: Spawn -> FilePath -> Spec
specWith base home = do
    describe "parseCmdline" $ do
        it "splits NUL-separated argv, preserving spaces within an argument" $
            parseCmdline "vim\NULFoo Bar.txt\NUL"
                `shouldBe` Just ["vim", "Foo Bar.txt"]

        it "is Nothing for an empty cmdline" $
            parseCmdline "" `shouldBe` Nothing

    it "reads a live pane's foreground argv from /proc" $ do
        pty <- spawn base { args = ["-c", "exec sleep 30"] }
        argv <- retryFor 50 (foregroundArgv pty) (== Just ["sleep", "30"])
        closePty pty
        _ <- waitExit pty
        argv `shouldBe` Just ["sleep", "30"]

    it "keeps the test shells' HOME away from the real one" $ do
        realHome <- getEnv "HOME"
        pty <- spawn base { args = ["-c", "echo home=$HOME"] }
        out <- drainPty pty
        _ <- waitExit pty
        out `shouldSatisfy` B8.isInfixOf (B8.pack ("home=" <> home))
        home `shouldNotBe` realHome

    it "spawns a shell and captures its output" $ do
        pty <- spawn base { args = ["-c", "echo hat-pty-works"] }
        out <- drainPty pty
        status <- waitExit pty
        out `shouldSatisfy` B8.isInfixOf "hat-pty-works"
        status `shouldBe` Just (Exited ExitSuccess)

    it "sets the initial window size on the pty" $ do
        pty <- spawn base
            { args = ["-c", "stty size"]
            , size = Size { rows = 30, cols = 100 }
            }
        out <- drainPty pty
        _ <- waitExit pty
        out `shouldSatisfy` B8.isInfixOf "30 100"

    it "propagates resize to the child" $ do
        pty <- spawn base { args = ["-c", "read _line; stty size"] }
        resize pty Size { rows = 40, cols = 120 }
        writePty pty "\n"
        out <- drainPty pty
        _ <- waitExit pty
        out `shouldSatisfy` B8.isInfixOf "40 120"

    it "runs the child in the given working directory" $ do
        pty <- spawn base { args = ["-c", "pwd"], cwd = Just "/tmp" }
        out <- drainPty pty
        _ <- waitExit pty
        out `shouldSatisfy` B8.isInfixOf "/tmp"

    -- M0 demo: a live shell driven interactively over the pty.
    it "round-trips an interactive shell session" $ do
        pty <- spawn base
        writePty pty "echo a$((1+1))b\n"
        out <- readUntil pty "a2b"
        out `shouldSatisfy` B8.isInfixOf "a2b"
        writePty pty "exit\n"
        status <- waitExit pty
        status `shouldBe` Just (Exited ExitSuccess)

    -- The foreground command follows a child running under the shell, not
    -- the shell itself: 'sleep' owns the terminal's foreground process
    -- group while it runs.
    it "reports the pane's foreground command, not the shell" $ do
        pty <- spawn base
        writePty pty "sleep 5\n"
        cmd <- retryFor 100 (foregroundCommand pty) (== Just (T.pack "sleep"))
        cmd `shouldBe` Just (T.pack "sleep")
        closePty pty

    -- NixOS wrappers exec the real binary as @.<name>-wrapped@ but keep
    -- the public name in argv[0] (bash's @exec -a@). The foreground
    -- command must be argv[0] — what tmux reports — not the executable's
    -- comm, which leaks the wrapper decoration (and truncates at 15
    -- bytes besides).
    it "reports a wrapped foreground command by argv[0], not its comm" $ do
        dir <- mkdtemp "/tmp/hat-pty-"
        flip finally (removeDirectoryRecursive dir) $ do
            -- A copy of bash stands in for the real binary: unlike the
            -- coreutils multi-call binary, it does not dispatch on
            -- argv[0]. The trailing `:` stops bash exec'ing the sleep,
            -- keeping the wrapped process the foreground group leader.
            -- Copied by a cp subprocess, not copyFile: a write fd held in
            -- this (forking, parallel) test process leaks into fork→exec
            -- windows and makes the exec below flake with ETXTBSY.
            let wrapped = dir <> "/.sleepish-wrapped"
            callProcess "cp" ["/bin/sh", wrapped]
            pty <- spawn base
            writePty pty (B8.pack
                ("exec -a sleepish " <> wrapped <> " -c 'sleep 5; :'\n"))
            cmd <- retryFor 100 (foregroundCommand pty)
                (== Just (T.pack "sleepish"))
            cmd `shouldBe` Just (T.pack "sleepish")
            closePty pty

    -- An orphaned pane that kept hat's listening socket open outlived
    -- `pkill hat` and made the next start's connect() hang. The child
    -- must inherit only its stdio: a forced-inheritable fd here must not
    -- survive into it.
    it "hands the pane child a clean fd table" $ do
        (r, w) <- createPipe
        setFdOption w CloseOnExec False
        let Fd n = w
        pty <- spawn base
            { args = ["-c", "[ -e /proc/self/fd/" ++ show n
                             ++ " ] && echo LEAK || echo CLEAN"] }
        out <- drainPty pty
        _ <- waitExit pty
        closeFd r
        closeFd w
        out `shouldSatisfy` B8.isInfixOf "CLEAN"
        out `shouldNotSatisfy` B8.isInfixOf "LEAK"

    it "reports a nonzero exit" $ do
        pty <- spawn base { args = ["-c", "exit 3"] }
        _ <- drainPty pty
        status <- waitExit pty
        status `shouldBe` Just (Exited (ExitFailure 3))
