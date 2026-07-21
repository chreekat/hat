-- | Shared driver for the scrollback benches: a pane in miniature, without
-- the server around it.
module PaneSim
    ( fillScrollback
    , liveBytesAfterGC
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless)
import qualified Data.ByteString.Char8 as B8
import Data.Word (Word64)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import System.Environment (getEnvironment)
import System.Mem (performMajorGC)

import Hat.Geometry (Size (..))
import Hat.Term.Emulator (Emulator, Event (..), feed, newEmulator)
import qualified Hat.Term.Pty as Pty

-- | Spawn @print_dots.sh nlines@ on a fresh 80x24 pty and feed everything it
-- prints to a fresh emulator with the given scrollback limit, answering
-- 'Output' events back down the pty the way the server's pane reader does.
-- Returns the emulator after the child has exited and the pty is closed, so
-- the scrollback it accumulated is the only interesting thing left alive.
-- Runs from the repo root (where cabal bench puts us).
fillScrollback :: Int -> Int -> IO Emulator
fillScrollback nlines limit = do
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
    pure e

-- | Live heap once the retained set has settled: major-GC repeatedly, giving
-- finalizer threads a chance to run in between, until the figure stops
-- moving (bounded, so an unsettled heap returns loud-and-wrong rather than
-- hanging). One GC alone is not enough — an emulator's C side is released by
-- a 'Foreign.Concurrent' finalizer that only runs after a GC notices death,
-- and until it does, its callback FunPtrs keep the emulator state alive.
liveBytesAfterGC :: IO Word64
liveBytesAfterGC = go (50 :: Int) =<< once
  where
    once = do
        performMajorGC
        gcdetails_live_bytes . gc <$> getRTSStats
    go budget prev = do
        threadDelay 1000
        cur <- once
        if cur == prev || budget <= 0
            then pure cur
            else go (budget - 1) cur
