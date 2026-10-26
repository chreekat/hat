-- | The @cabal bench@ gate: one small typing run through
-- @tools\/bench\/hat_perf@, judged against the committed
-- @tools\/bench\/perf-baseline@. @HAT_PERF_RECORD=1@ re-records the baseline
-- from the same run instead of judging by it.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import System.Directory (removePathForcibly)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Posix.Temp (mkdtemp)
import System.Process (callProcess, readProcess)
import Text.Read (readMaybe)

data Mode = Check | Record

main :: IO ()
main = do
    mode <- maybe Check (const Record) <$> lookupEnv "HAT_PERF_RECORD"
    awaitQuiescence
    bin <- listBin "exe:hat"
    out <- mkdtemp "/tmp/hat-perf-gate-"
    gate mode bin out `finally` removePathForcibly out

gate :: Mode -> FilePath -> FilePath -> IO ()
gate mode bin out = do
    -- Each workload runs at sizes where its slope dominates its intercept;
    -- restart's per-line cost needs thousands of lines to stand clear of
    -- the ~400M-instruction restart itself.
    callProcess "tools/bench/hat_perf"
        [ "--workloads", "type typeh typestatus", "--sizes", "200 400 800 1600"
        , "--mux", "hat", "--bin", bin, "--out", out
        ]
    callProcess "tools/bench/hat_perf"
        [ "--workloads", "restart", "--sizes", "2000 4000 8000 16000"
        , "--mux", "hat", "--bin", bin, "--out", out
        ]
    report <- listBin "bench-report"
    callProcess report $ case mode of
        Check -> ["check", baseline, out]
        Record -> ["record", baseline, out]
  where
    baseline = "tools/bench/perf-baseline"

listBin :: String -> IO FilePath
listBin component = init <$> readProcess "cabal" ["list-bin", component] ""

-- | Wait, up to two minutes, for the 1-minute load average to fall to
-- @HAT_PERF_MAX_LOAD@ (default 1.0), else refuse to measure: the typing
-- workloads' instruction counts move with load.
awaitQuiescence :: IO ()
awaitQuiescence = do
    limit <- lookupEnv "HAT_PERF_MAX_LOAD" >>= \case
        Nothing -> pure (1.0 :: Double)
        Just s -> maybe (refuse ("HAT_PERF_MAX_LOAD is not a number: " <> s))
                        pure (readMaybe s)
    let poll :: Int -> IO ()
        poll tries = do
            load <- oneMinuteLoad
            if load <= limit then pure ()
            else if tries <= 0 then refuse
                ("1-minute load " <> show load <> " stayed above "
                    <> show limit <> "; not measuring a busy box")
            else threadDelay 5_000_000 >> poll (tries - 1)
    poll 24
  where
    oneMinuteLoad :: IO Double
    oneMinuteLoad = do
        raw <- readFile "/proc/loadavg"
        maybe (refuse ("unreadable /proc/loadavg: " <> raw)) pure
            (readMaybe (takeWhile (/= ' ') raw))
    refuse :: String -> IO a
    refuse msg = hPutStrLn stderr ("hat-perf: " <> msg) >> exitFailure
