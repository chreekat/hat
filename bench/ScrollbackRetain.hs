-- | Regression guard for the fixed pane-teardown space leak, and the
-- in-process version of tools/bench/scrollback_leak_benchmark: a child
-- generates the scrollback through a real pty and the server's read→feed
-- loop ('PaneSim.fillScrollback'), then the emulator, pty, and child all die
-- inside 'runChildAndCount' — so live heap afterwards isolates what the feed
-- path retains past pane teardown. Healthy code retains a few MB, flat in
-- lines printed; the old leak scaled with output volume.
--
-- Run:  cabal bench scrollback-retain                     (4000 lines, limit 500)
--       cabal run bench:scrollback-retain -- 12000 500    (lines, scrollback limit)
--
-- Companions: bench/FeedMem.hs drives 'feed' with raw bytes, no child or pty;
-- bench/ScrollbackLive.hs measures the live cost of scrollback still held.
module Main (main) where

import Data.Word (Word64)
import System.Environment (getArgs)
import Text.Printf (printf)

import Hat.Term.Emulator (scrollbackLength)
import PaneSim (fillScrollback, liveBytesAfterGC)

-- | Fill a scrollback and return only the retained line count, so on return
-- the emulator (and everything it holds) is provably unreachable.
runChildAndCount :: Int -> Int -> IO Int
runChildAndCount nlines limit = do
    e <- fillScrollback nlines limit
    scrollbackLength e

main :: IO ()
main = do
    args <- getArgs
    let (nlines, limit) = case args of
            (a : b : _) -> (read a, read b)
            _           -> (4000, 500)

    baseEmpty <- liveBytesAfterGC
    retainedLines <- runChildAndCount nlines limit
    afterDead <- liveBytesAfterGC   -- emulator, pty, and child are gone here

    printf "child printed=%d  scrollbackLimit=%d  retainedLines=%d\n"
        nlines limit retainedLines
    printf "  live before spawn         : %d\n" baseEmpty
    printf "  live after, pane torn down : %d\n" afterDead
    printf "  retained past teardown     : %d  (%d / line printed)\n"
        (afterDead - baseEmpty :: Word64)
        ((afterDead - baseEmpty) `div` fromIntegral (max 1 nlines))
