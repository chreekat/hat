-- | Report the figures a benchmark run left behind: fitted summaries,
-- before\/after comparisons, and baseline checks over a directory of
-- @perf stat -x,@ CSVs, or an @+RTS -t --machine-readable@ stats file. The
-- orchestration lives in @tools\/bench\/hat_mem@ and @tools\/bench\/hat_perf@;
-- this only reads their artifacts.
module Main (main) where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Hat.Bench.Linear
import Hat.Bench.PerfStat
import Hat.Bench.Report
import Hat.Bench.RtsStats

main :: IO ()
main = getArgs >>= \case
    ["summary", dir] -> fitDir dir >>= mapM_ TIO.putStrLn . summaryTable
    ["compare", beforeDir, afterDir] -> do
        beforeRun <- fitDir beforeDir
        afterRun <- fitDir afterDir
        mapM_ TIO.putStrLn (compareTable beforeRun afterRun)
    ["check", baselineFile, dir] -> do
        baseline <- readBaseline baselineFile
        checks <- checkBaseline baseline <$> fitDir dir
        mapM_ TIO.putStrLn (renderChecks checks)
        if checkPassed checks
            then putStrLn "baseline check passed"
            else die "baseline check FAILED"
    ["record", baselineFile, dir] -> do
        fits <- fitDir dir
        let baseline = Baseline
                { tolerance = 0.2
                , entries = [ (k, l.slope) | (k, l) <- fits ]
                }
        TIO.writeFile baselineFile (renderBaseline baseline)
        putStrLn (baselineFile <> " recorded:")
        mapM_ TIO.putStrLn (renderChecks (checkBaseline baseline fits))
    [stats] -> report stats
    _ -> die "usage: bench-report summary <dir>\n\
             \       bench-report compare <before-dir> <after-dir>\n\
             \       bench-report check <baseline> <dir>\n\
             \       bench-report record <baseline> <dir>\n\
             \       bench-report <rts-stats-file>"

-- | Fit every series a directory of @<mux>-<workload>-<role>-<N>.csv@ files
-- holds. An unreadable CSV or an unfittable series dies loudly, never reads
-- as a measurement.
fitDir :: FilePath -> IO [(SeriesKey, Line)]
fitDir dir = do
    files <- listDirectory dir
    let series = Map.fromListWith (<>)
            [ (k, [(n, f)])
            | f <- files
            , Just (k, n) <- [parseSeriesFile (T.pack f)]
            ]
    if Map.null series
        then die (dir <> ": no benchmark CSVs")
        else mapM fit (Map.toList series)
  where
    fit (k, points) = do
        pts <- mapM instructions (sortOn fst points)
        case fitLinear pts of
            Left err -> die (T.unpack (renderKey k <> ": " <> err))
            Right l -> pure (k, l)
    instructions (n, f) = do
        raw <- TIO.readFile (dir <> "/" <> f)
        case parsePerfStat raw >>= counterWord "instructions" of
            Left err -> die (f <> ": " <> T.unpack err)
            Right instr ->
                pure (fromIntegral n :: Double, fromIntegral instr)

readBaseline :: FilePath -> IO Baseline
readBaseline file = do
    raw <- TIO.readFile file
    case parseBaseline raw of
        Left err -> die (file <> ": " <> T.unpack err)
        Right b -> pure b

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
