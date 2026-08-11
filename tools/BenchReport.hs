-- | Report the figures from an @+RTS -t --machine-readable@ stats file, as
-- written by a benchmark run. The orchestration lives in
-- @tools\/bench\/hat_mem@; this only reads what the RTS left behind.
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Hat.Bench.RtsStats

main :: IO ()
main = getArgs >>= \case
    [stats] -> report stats
    _ -> die "usage: bench-report <rts-stats-file>"

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
