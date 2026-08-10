-- | Black-box memory benchmark: drive the real @hat@ binary through the
-- ordinary server lifecycle and read what its RTS reports on the way out.
--
-- Nothing here reaches into the library's internals. The server is spawned
-- exactly as @--server@ runs it in production, exercised over the socket by
-- ordinary client commands, and asked to exit through @kill-server@ — the RTS
-- writes its stats block only on a clean shutdown.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (removeDirectoryRecursive)
import System.Environment (getEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (IOMode (..), openFile)
import System.Posix.Files (fileExist)
import System.Posix.Temp (mkdtemp)
import qualified System.Process as P

import Hat.Bench.RtsStats

main :: IO ()
main = do
    bin <- hatBinary
    dir <- mkdtemp "/tmp/hat-bench-"
    run bin dir `finally` removeDirectoryRecursive dir

-- | One measured server lifetime: spawn, exit, report.
run :: FilePath -> FilePath -> IO ()
run bin dir = do
    let sock = dir <> "/socket"
        conf = dir <> "/hat.conf"
        stats = dir <> "/rts.txt"
    writeFile conf ""
    server <- spawnServer bin sock conf stats dir
    up <- await 250 (fileExist sock)
    unless up (die "server never created its socket")
    _ <- ctl bin sock dir ["kill-server"]
    _ <- P.waitForProcess server
    report stats

-- | The server as production runs it: foreground @--server@, its own HOME so
-- no ambient config is read, and @GHCRTS@ scoped to this process alone —
-- every client below is the same binary and would otherwise overwrite the
-- same stats file.
spawnServer
    :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
    -> IO P.ProcessHandle
spawnServer bin sock conf stats home = do
    path <- getEnv "PATH"
    devNull <- openFile "/dev/null" WriteMode
    (_, _, _, h) <- P.createProcess
        (P.proc bin ["--server", sock, conf])
            { P.env = Just
                [ ("HOME", home)
                , ("PATH", path)
                , ("GHCRTS", "-t" <> stats <> " --machine-readable")
                ]
            , P.std_out = P.UseHandle devNull
            , P.std_err = P.UseHandle devNull
            }
    pure h

-- | A client command against the benchmark's server, with no @GHCRTS@ of its
-- own. See 'spawnServer'.
ctl :: FilePath -> FilePath -> FilePath -> [String] -> IO String
ctl bin sock home args = do
    path <- getEnv "PATH"
    (_, out, _) <- P.readCreateProcessWithExitCode
        (P.proc bin (["-S", sock] <> args))
            { P.env = Just [("HOME", home), ("PATH", path)] }
        ""
    pure out

report :: FilePath -> IO ()
report stats = do
    raw <- TIO.readFile stats
    case parseRtsStats raw of
        Left err -> die ("unreadable RTS stats: " <> T.unpack err)
        Right st -> do
            mapM_ (putStrLn . bytes st) byteFigures
            mapM_ (putStrLn . seconds st) secondFigures
  where
    byteFigures =
        [ ("allocated_bytes", "total allocation")
        , ("max_live_bytes", "peak live heap")
        , ("max_mem_in_use_bytes", "peak memory from the OS")
        ]
    secondFigures =
        [ ("GC_cpu_seconds", "GC cpu")
        , ("mut_cpu_seconds", "mutator cpu")
        ]
    bytes st (key, label) = case statWord key st of
        Left err -> label <> ": " <> T.unpack err
        Right v -> label <> ": " <> show v <> " B"
    seconds st (key, label) = case statDouble key st of
        Left err -> label <> ": " <> T.unpack err
        Right v -> label <> ": " <> show v <> " s"

-- | Poll until the condition holds or the attempts run out; 'False' on
-- timeout, so a caller reports a stuck server instead of measuring one.
await :: Int -> IO Bool -> IO Bool
await n act
    | n <= 0 = pure False
    | otherwise = do
        ok <- act
        if ok then pure True else threadDelay 20_000 >> await (n - 1) act

-- | The freshly built binary, resolved the way the integration suite does.
hatBinary :: IO FilePath
hatBinary = do
    (code, out, _) <- P.readProcessWithExitCode "cabal" ["list-bin", "hat"] ""
    case code of
        ExitSuccess -> pure (takeWhile (/= '\n') out)
        _ -> die "cabal list-bin hat failed; build first"

die :: String -> IO a
die msg = putStrLn ("hat-mem: " <> msg) >> exitFailure
