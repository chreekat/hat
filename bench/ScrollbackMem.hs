-- | In-process version of tools/bench/scrollback_leak_benchmark: instead of a
-- hat server and @new-window -d print_dots.sh@, spawn the scrollback-generating
-- script directly under a pty ('Hat.Term.Pty.spawn') and run the same
-- read→feed loop the server's pane reader runs. The user feeds nothing — all
-- emulator input is the child's output, exactly as in the black-box script —
-- but the emulator and pty die inside 'runChildAndCount', so live heap
-- afterwards isolates what the feed path retains past pane teardown.
--
-- Run from the repo root (cabal bench does):
--   cabal bench scrollback-mem                     (4000 lines, limit 500)
--   cabal run bench:scrollback-mem -- 12000 500    (lines, scrollback limit)
--
-- Companion: bench/FeedMem.hs drives 'feed' with the same bytes minus the
-- child and pty, to tell a feed/emulator retention from a pty-layer one.
module Main (main) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString.Char8 as B8
import Data.Word (Word64)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import System.Environment (getArgs, getEnvironment)
import System.Mem (performMajorGC)
import Text.Printf (printf)

import Hat.Geometry (Size (..))
import Hat.Term.Emulator (Event (..), feed, newEmulator, scrollbackLength)
import qualified Hat.Term.Pty as Pty

-- | Spawn @print_dots.sh nlines@ on a fresh pty, feed everything it prints to
-- a fresh emulator (answering 'Output' events back down the pty, as the
-- server's pane reader does), and return only the retained line count — so on
-- return the emulator, pty, and child are all provably dead.
runChildAndCount :: Int -> Int -> IO Int
runChildAndCount nlines limit = do
    let sz = Size { rows = 24, cols = 80 }
    e <- newEmulator sz limit
    environ <- getEnvironment
    pty <- Pty.spawn Pty.Spawn
        { cmd = "tools/bench/print_dots.sh"
        , args = [show nlines]
        , env = environ
        , cwd = Nothing
        , size = sz
        }
    let readLoop = do
            bs <- Pty.readPty pty
            unless (B8.null bs) $ do
                events <- feed e bs
                forM_ events $ \case
                    Output out -> Pty.writePty pty out
                    _ -> pure ()
                readLoop
    readLoop
    _ <- Pty.waitExit pty
    Pty.closePty pty
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
        (afterDead - baseEmpty)
        ((afterDead - baseEmpty) `div` fromIntegral (max 1 nlines))

liveBytesAfterGC :: IO Word64
liveBytesAfterGC = do
    performMajorGC
    performMajorGC
    gcdetails_live_bytes . gc <$> getRTSStats
