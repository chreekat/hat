-- | Minimal repro of a feed-scaled retention discovered while trying to build
-- a scrollback memory benchmark.
--
-- It feeds N one-character lines through a fresh emulator inside 'feedAndCount',
-- which returns only the retained scrollback line count — so once it returns,
-- the emulator (and its bounded scrollback) is provably unreachable. Yet after
-- two forced major GCs the live heap is ~248 KB × N, *linear in lines fed* and
-- *independent of the scrollback limit*. The scrollback is capped (and dies
-- with the emulator); this retention is something else.
--
-- Caveat on fidelity: this harness discards every '[Event]' that 'feed'
-- returns and never renders/drains the way the real server does — so this may
-- be a defect the server hits, or an artifact of how it is driven here.
--
-- Run:  cabal bench hat-mem            (defaults: 2000 lines, limit 500)
--       cabal bench hat-mem 8000 500   (lines, limit)
module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B8
import Data.Word (Word64)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import System.Environment (getArgs)
import System.Mem (performMajorGC)
import Text.Printf (printf)

import Hat.Geometry (Size (..))
import Hat.Term.Emulator (feed, newEmulator, scrollbackLength)

feedAndCount :: Int -> Int -> IO Int
feedAndCount nlines limit = do
    e <- newEmulator Size { rows = 24, cols = 80 } limit
    forM_ [1 .. nlines] $ \_ -> feed e (B8.pack ".\r\n")
    scrollbackLength e

main :: IO ()
main = do
    args <- getArgs
    let (nlines, limit) = case args of
            (a : b : _) -> (read a, read b)
            _           -> (2000, 500)

    baseEmpty <- liveBytesAfterGC
    retainedLines <- feedAndCount nlines limit
    afterDead <- liveBytesAfterGC   -- the emulator is unreachable here

    printf "fed=%d  scrollbackLimit=%d  retainedLines=%d\n" nlines limit retainedLines
    printf "  live before feeding      : %d\n" baseEmpty
    printf "  live after, emulator dead : %d\n" afterDead
    printf "  retained past teardown    : %d  (%d / line fed)\n"
        (afterDead - baseEmpty)
        ((afterDead - baseEmpty) `div` fromIntegral (max 1 nlines))

liveBytesAfterGC :: IO Word64
liveBytesAfterGC = do
    performMajorGC
    performMajorGC
    gcdetails_live_bytes . gc <$> getRTSStats
