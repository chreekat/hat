-- | Report the figures a benchmark run left behind: an @+RTS -t
-- --machine-readable@ stats file, or a @perf stat -x,@ instruction series
-- over workload sizes. The orchestration lives in @tools\/bench\/hat_mem@
-- and @tools\/bench\/hat_perf@; this only reads their artifacts.
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Hat.Bench.Linear
import Hat.Bench.PerfStat
import Hat.Bench.RtsStats

main :: IO ()
main = getArgs >>= \case
    "perf" : label : points@(_ : _) -> perfReport label points
    [stats] -> report stats
    _ -> die "usage: bench-report <rts-stats-file>\n\
             \       bench-report perf <label> <N>=<perf-csv>..."

-- | Fit instructions = slope·N + intercept over one series of perf runs and
-- print the points alongside the fit.
perfReport :: String -> [String] -> IO ()
perfReport label points = do
    pts <- mapM readPoint points
    mapM_ (putStrLn . renderPoint) pts
    case fitLinear pts of
        Left err -> die (T.unpack err)
        Right l -> putStrLn $
            label <> " fit: instructions = "
                <> show l.slope <> " * N + " <> show l.intercept
  where
    readPoint arg = case break (== '=') arg of
        (n@(_ : _), '=' : file) | [(size, "")] <- reads n -> do
            raw <- TIO.readFile file
            case parsePerfStat raw >>= counterWord "instructions" of
                Left err -> die (file <> ": " <> T.unpack err)
                Right instr -> pure (size :: Double, fromIntegral instr)
        _ -> die ("not an <N>=<perf-csv> argument: " <> arg)
    renderPoint (n, instr) =
        label <> " N=" <> show (round n :: Integer)
            <> ": " <> show (round instr :: Integer) <> " instructions"

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

die :: String -> IO a
die msg = putStrLn ("bench-report: " <> msg) >> exitFailure
