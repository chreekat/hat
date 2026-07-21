-- | Shared driver for the scrollback benches: a pane in miniature, without
-- the server around it.
module PaneSim
    ( fillScrollback
    , liveBytesAfterGC
    ) where

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

-- | Live heap after forcing the retained set to settle: two major GCs, then
-- the last GC's live-bytes figure.
liveBytesAfterGC :: IO Word64
liveBytesAfterGC = do
    performMajorGC
    performMajorGC
    gcdetails_live_bytes . gc <$> getRTSStats
