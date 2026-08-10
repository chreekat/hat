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
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (removeDirectoryRecursive)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (IOMode (..), openFile, readFile')
import System.Posix.Files (fileExist)
import System.Posix.Temp (mkdtemp)
import qualified System.Process as P

import Hat.Bench.RtsStats

main :: IO ()
main = do
    bin <- hatBinary
    args <- getArgs
    dir <- mkdtemp "/tmp/hat-bench-"
    run bin dir (scrollbackLines args) ("--reload" `elem` args)
        `finally` removeDirectoryRecursive dir

-- | How many scrollback lines to generate; @--lines N@, default 20000.
scrollbackLines :: [String] -> Int
scrollbackLines args = case dropWhile (/= "--lines") args of
    (_ : n : _) | [(v, "")] <- reads n -> v
    _ -> 20000

-- | One measured server lifetime: spawn, fill a pane's scrollback, exit,
-- report what the RTS charged for it.
run :: FilePath -> FilePath -> Int -> Bool -> IO ()
run bin dir n reload = do
    let sock = dir <> "/socket"
        conf = dir <> "/hat.conf"
        stats = dir <> "/rts.txt"
        gen = dir <> "/gen.sh"
    -- A history limit above the generated count, or the scrollback under
    -- measurement is just the default 2000 lines.
    writeFile conf ("set -g history-limit " <> show (n * 2) <> "\n")
    writeFile gen (generator n)
    server <- spawnServer bin sock conf stats dir
    up <- await 250 (fileExist sock)
    unless up (die "server never created its socket")
    _ <- ctl bin sock dir
        ["new-session", "-d", "-x", "200", "-y", "50", "sh " <> gen]
    filled <- await 3000 (marker `isInfixOf'` ctl bin sock dir ["capture-pane", "-p"])
    unless filled (die "pane never finished generating scrollback")
    pid <- P.getPid server
    filledRss <- traverse residentKb pid
    reloadRss <- if reload then Just <$> restartAndSettle bin sock dir pid else pure Nothing
    _ <- ctl bin sock dir ["kill-server"]
    _ <- P.waitForProcess server
    putStrLn (show n <> " scrollback lines" <> if reload then ", reloaded" else "")
    mapM_ (say "resident after fill") filledRss
    mapM_ (mapM_ (say "resident after reload")) reloadRss
    report stats
  where
    say label kb = putStrLn (label <> ": " <> show kb <> " kB")

-- | Reload the server in place and wait for the pane it adopted to show the
-- screen again. The PID survives the @execve@, so the same @\/proc@ entry
-- measures both sides of the restart; the RTS stats, by contrast, are written
-- only by the image that exits, which is the reloaded one.
restartAndSettle
    :: FilePath -> FilePath -> FilePath -> Maybe P.Pid -> IO (Maybe Integer)
restartAndSettle bin sock dir pid = do
    _ <- ctl bin sock dir ["restart-server"]
    back <- await 3000 (marker `isInfixOf'` ctl bin sock dir ["capture-pane", "-p"])
    unless back (die "server never came back from restart-server")
    traverse residentKb pid

-- | The pane program: deterministic styled lines, a marker the driver can
-- wait on, then a sleep so the pane is still live at measurement time.
generator :: Int -> String
generator n = unlines
    [ "awk 'BEGIN{for(i=0;i<" <> show n <> ";i++)"
        <> " printf \"\\033[3%dm%06d the quick brown fox jumps over"
        <> " the lazy dog %s\\033[0m\\n\", i%8, i, i%2?\"~~~\":\"...\"}'"
    , "echo " <> marker
    , "exec sleep 1000000"
    ]

-- | What the driver waits to see on screen before it measures.
marker :: String
marker = "HAT-BENCH-FILLED"

-- | Resident kB of a live process, the figure @atop@ shows. Read strictly:
-- the entry vanishes with the process, and a lazy read would be forced after
-- the server is already gone.
residentKb :: P.Pid -> IO Integer
residentKb pid = do
    raw <- readFile' ("/proc/" <> show pid <> "/status")
    pure $! case [ws | l <- lines raw, ("VmRSS:" : ws) <- [words l]] of
        ((kb : _) : _) | [(v, "")] <- reads kb -> v
        _ -> 0

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

isInfixOf' :: String -> IO String -> IO Bool
isInfixOf' needle act = isInfixOf needle <$> act

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
