-- | The number the Cell/Style interning work is meant to improve: live heap
-- bytes held by a fixed amount of live scrollback.
--
-- Children under real ptys generate the scrollback through the server's
-- read→feed loop ('PaneSim.fillScrollback') — the path scrollback-retain
-- shows is leak-free. The measurement is differential across two panes kept
-- alive side by side: both are fed the same output volume (so each pane's
-- fixed overhead and per-fill residue are identical) but with different
-- scrollback limits, each pinned by printing past it. Subtracting the two
-- panes' live-heap increments cancels everything except the cells of the
-- extra held lines:
--
--   increment(pane) = base + residue(lines printed) + cells(limit)
--   increment(big) − increment(small) = cells(bigLimit) − cells(smallLimit)
--
-- Nothing is torn down and nothing is cleared, deliberately: an emulator
-- that has survived a GC does not reliably release its heap on death (its
-- callback FunPtrs root the state until a finalizer runs), and
-- 'clearScrollback' currently releases nothing at all — either would put a
-- race or a falsehood inside the measurement.
--
-- Run:  cabal bench scrollback-live                  -- judge against 'baseline'
--       cabal run bench:scrollback-live -- 4000 2000 100   -- explore (no verdict)
--
-- With no arguments the figure is judged against the committed baseline: a
-- regression fails the run (exit 1); an improvement is a non-failing ratchet
-- notice — lower 'baseline' to the printed figure to lock the win in.
module Main (main) where

import Control.Monad (when)
import Data.Word (Word64)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)

import Hat.Bench.Residency
import Hat.Term.Emulator (Emulator, scrollbackLength)
import PaneSim (fillScrollback, liveBytesAfterGC)

-- | Live bytes for the extra @benchBigLimit - benchSmallLimit@ held lines of
-- dots scrollback. Ratchet down on a reported improvement.
baseline :: Baseline
baseline = Baseline (LiveBytes 3726000)

-- | Half-width of the accepted band. The figure jitters ~±7% two-sided (a
-- difference of two noisy ~5 MB pane increments), which this clears while
-- still catching the multiple-x moves the benchmark exists to see.
tolerance :: Tolerance
tolerance = mkTolerance 0.10

benchLines, benchBigLimit, benchSmallLimit :: Int
benchLines = 4000
benchBigLimit = 2000
benchSmallLimit = 100

main :: IO ()
main = do
    args <- getArgsParsed
    case args of
        Just (nlines, big, small) ->
            () <$ measure nlines big small
        Nothing -> do
            cost <- measure benchLines benchBigLimit benchSmallLimit
            let Baseline (LiveBytes base) = baseline
            printf "committed baseline: %d bytes  (±%.0f%%)\n"
                base (100 * toleranceFraction tolerance)
            case classify tolerance baseline (LiveBytes cost) of
                WithinBand -> putStrLn "verdict: WITHIN BAND"
                Improved (LiveBytes m) ->
                    printf "verdict: IMPROVEMENT — ratchet baseline down to %d\n" m
                Regressed (LiveBytes m) -> do
                    printf "verdict: REGRESSION — %d bytes over baseline\n" (m - base)
                    exitFailure

-- | Fill the two panes, measuring the settled live heap at start, after the
-- big-limit pane, and after the small-limit pane; the increments' difference
-- is the cost of the extra held lines. Both emulators are touched after the
-- last measurement so each is a live root through every GC before it.
measure :: Int -> Int -> Int -> IO Word64
measure nlines big small = do
    when (nlines <= big || big <= small) $
        error "want lines printed > big limit > small limit"
    start <- liveBytesAfterGC
    eBig <- fillScrollback nlines big
    heldBig <- liveBytesAfterGC
    eSmall <- fillScrollback nlines small
    heldBoth <- liveBytesAfterGC
    nBig <- pinned eBig big
    nSmall <- pinned eSmall small
    let incBig = heldBig - start
        incSmall = heldBoth - heldBig
    when (incBig <= incSmall) $
        error ("big pane not costlier than small: "
            <> show incBig <> " vs " <> show incSmall)
    let cost = incBig - incSmall
        extraLines = nBig - nSmall
    printf "panes: %d lines printed each; limits %d vs %d\n" nlines big small
    printf "  pane increments: big=%d small=%d\n" incBig incSmall
    printf "live bytes for %d extra held lines: %d  (%d / line)\n"
        extraLines cost (cost `div` fromIntegral (max 1 extraLines))
    pure cost

-- | The pane's held line count, which printing past the limit must have
-- pinned exactly to it — an underfull pane would judge the baseline against
-- fewer cells.
pinned :: Emulator -> Int -> IO Int
pinned e limit = do
    n <- scrollbackLength e
    when (n /= limit) $
        error ("scrollback not pinned to limit: "
            <> show n <> " /= " <> show limit)
    pure n

getArgsParsed :: IO (Maybe (Int, Int, Int))
getArgsParsed = do
    args <- getArgs
    pure $ case args of
        (a : b : c : _) -> Just (read a, read b, read c)
        _               -> Nothing
