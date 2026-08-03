-- | Load a preserved reload handover (@<socket>.reload.last@) offline as a
-- faithful model of the server's resume path: walk the decoded tree once,
-- restoring each pane into a libghostty emulator, and hold only the emulators
-- afterwards -- exactly as 'Hat.Server.rebuildReload' consumes its 'ReloadState'
-- into 'ServerState' and returns, leaving the tree unreferenced. The summary is
-- read back out of the emulators, never off the tree, so a heap census here
-- measures the state the server actually keeps rather than the inflated
-- @[[Cell]]@ dump it was only meant to consume. Reads only the blob, so it
-- never touches a running server.
--
-- Run from the repo root:
-- @cabal run replay-reload -- /tmp/hat-1000/default.reload.last@
--
-- Heap-profile the rehydration with @just reload_rehydrate_profile@.
module Main (main) where

import Control.Monad (forM)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

import Hat.Geometry
import Hat.Server (captureSize, replayPane)
import Hat.Server.Reload
import Hat.Term.Emulator (Screen (cells))
import qualified Hat.Term.Emulator as Emu

-- The fallback size 'adoptPane' uses for a blank capture; a real capture is
-- adopted at its own size ('captureSize'), which this tool mirrors.
rebuildSize :: Size
rebuildSize = Size { rows = 24, cols = 80 }

-- Matches the deployed config's @history-limit@, so seeded scrollback trims the
-- same way it does in the server.
historyLimit :: Int
historyLimit = 50000

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    path <- getArgs >>= \case
        [p] -> pure p
        _   -> die "usage: replay-reload <blob>"
    bs <- B.readFile path
    tree <- case decodeHandover bs of
        Left e -> die ("undecodable blob: " <> T.unpack e)
        Right h -> case h.tree of
            Left e -> die ("unusable tree: " <> T.unpack e)
            Right t -> pure t
    emus <- rebuild tree
    putStrLn ("rehydrated " <> show (length emus) <> " pane(s)")
    -- putStr =<< summarize emus

-- Mirror of 'Hat.Server.rebuildReload' down to 'adoptPane', minus the pty
-- adoption and 'ServerState' bookkeeping: the traversal consumes the tree and
-- yields only the emulators, so the tree is collectable once it returns.
rebuild :: ReloadState -> IO [(String, Emu.Emulator)]
rebuild tree = fmap concat $ forM tree.sessions $ \sess ->
    fmap concat $ forM sess.windows $ \win ->
        forM (zip [0 :: Int ..] win.panes) $ \(ordinal, rp) -> do
            e <- adopt rp
            pure (T.unpack sess.name <> ":" <> show win.ix <> "." <> show ordinal, e)

adopt :: ReloadPane -> IO Emu.Emulator
adopt rp = do
    let esz = fromMaybe rebuildSize (captureSize rp.screen)
    e <- Emu.newEmulator esz historyLimit
    let (bytes, sb) = replayPane esz rp
    _ <- Emu.feed e bytes
    Emu.seedScrollback e sb
    pure e

-- Reads every figure back through the shim from libghostty's own grid and
-- history, so it counts the emulator state the server retains, not the tree.
_summarize :: [(String, Emu.Emulator)] -> IO String
_summarize emus = do
    per <- forM emus $ \(_, e) -> do
        scr   <- Emu.snapshot e
        sbLen <- Emu.scrollbackLength e
        sbCells <- sum <$> forM [0 .. sbLen - 1]
            (\i -> maybe 0 V.length <$> Emu.scrollbackLine e i)
        pure (V.length scr.cells, V.sum (V.map V.length scr.cells), sbLen, sbCells)
    let (grs, gcs, sbl, sbc) =
            foldr (\(a, b, c, d) (w, x, y, z) -> (a + w, b + x, c + y, d + z))
                  (0, 0, 0, 0) per
    pure $ unlines
        [ "emulator state (read back from libghostty):"
        , "  panes            " <> show (length emus)
        , "  grid rows        " <> show grs
        , "  grid cells       " <> show gcs
        , "  scrollback lines " <> show sbl
        , "  scrollback cells " <> show sbc
        ]
