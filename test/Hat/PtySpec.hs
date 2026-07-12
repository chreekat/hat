module Hat.PtySpec (spec) where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.Hspec

import Hat.Geometry
import Hat.Pty

baseSpawn :: Spawn
baseSpawn = Spawn
    { cmd = "/bin/sh"
    , args = []
    , env = [("PATH", "/run/current-system/sw/bin:/usr/bin:/bin"), ("TERM", "dumb")]
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
    it "spawns a shell and captures its output" $ do
        pty <- spawn baseSpawn { args = ["-c", "echo hat-pty-works"] }
        out <- drainPty pty
        status <- waitExit pty
        out `shouldSatisfy` B8.isInfixOf "hat-pty-works"
        status `shouldBe` Exited ExitSuccess

    it "sets the initial window size on the pty" $ do
        pty <- spawn baseSpawn
            { args = ["-c", "stty size"]
            , size = Size { rows = 30, cols = 100 }
            }
        out <- drainPty pty
        _ <- waitExit pty
        out `shouldSatisfy` B8.isInfixOf "30 100"

    it "propagates resize to the child" $ do
        pty <- spawn baseSpawn { args = ["-c", "read _line; stty size"] }
        resize pty Size { rows = 40, cols = 120 }
        writePty pty "\n"
        out <- drainPty pty
        _ <- waitExit pty
        out `shouldSatisfy` B8.isInfixOf "40 120"

    it "runs the child in the given working directory" $ do
        pty <- spawn baseSpawn { args = ["-c", "pwd"], cwd = Just "/tmp" }
        out <- drainPty pty
        _ <- waitExit pty
        out `shouldSatisfy` B8.isInfixOf "/tmp"

    -- M0 demo: a live shell driven interactively over the pty.
    it "round-trips an interactive shell session" $ do
        pty <- spawn baseSpawn
        writePty pty "echo a$((1+1))b\n"
        out <- readUntil pty "a2b"
        out `shouldSatisfy` B8.isInfixOf "a2b"
        writePty pty "exit\n"
        status <- waitExit pty
        status `shouldBe` Exited ExitSuccess

    -- The foreground command follows a child running under the shell, not
    -- the shell itself: 'sleep' owns the terminal's foreground process
    -- group while it runs.
    it "reports the pane's foreground command, not the shell" $ do
        pty <- spawn baseSpawn
        writePty pty "sleep 5\n"
        cmd <- retryFor 100 (foregroundCommand pty) (== Just (T.pack "sleep"))
        cmd `shouldBe` Just (T.pack "sleep")
        closePty pty

    it "reports a nonzero exit" $ do
        pty <- spawn baseSpawn { args = ["-c", "exit 3"] }
        _ <- drainPty pty
        status <- waitExit pty
        status `shouldBe` Exited (ExitFailure 3)
