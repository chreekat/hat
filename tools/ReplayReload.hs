-- | Load a preserved reload handover (@<socket>.reload.last@) offline as a
-- faithful model of the server's resume path: walk the decoded tree once,
-- restoring each pane into a libghostty emulator, and hold only the emulators
-- afterwards -- exactly as 'Hat.Server.rebuildReload' consumes its 'ReloadTree'
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
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Text.Printf (printf)

import Hat.Geometry
import Hat.Server
    (ScrollbackCarry (KeepScrollback), captureReloadScreen, captureSize
    , replayPane)
import Hat.Server.Persist
    (SessionSnap (..), Snapshot (..), WindowSnap (..), encodeSnapshotJson)
import Hat.Server.Reload
import Hat.Term.Emulator (Screen (cells))
import Hat.Term.Emulator qualified as Emu

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
    t0 <- getMonotonicTime
    tree <- case decodeHandover bs of
        Left e -> die ("undecodable blob: " <> T.unpack e)
        Right h -> case h.tree of
            Left e -> die ("unusable tree: " <> T.unpack e)
            Right t -> pure t
    t1 <- getMonotonicTime
    emus <- rebuild tree
    t2 <- getMonotonicTime
    putStrLn ("rehydrated " <> show (length emus) <> " pane(s) from "
              <> show (B.length bs `div` 1000000) <> " MB")
    -- Model of the outgoing image's half: re-capture the emulators just
    -- rebuilt, then encode a fresh payload around them (dummy handles; the
    -- tree is the decoded one's, so the blob round-trips below).
    screens <- forM emus (captureReloadScreen KeepScrollback . snd)
    t3 <- getMonotonicTime
    let hots = [ HotPane { masterFd = -1, childPid = -1
                         , modes = ReloadModes False False 0 False 0
                         , screen = sc }
               | sc <- screens ]
        blob = encodeHandover
            ReloadCleanup { listenFd = -1, live = [] }
            ReloadHot { tree = treeJson tree, hot = hots
                      , lastSession = Nothing }
    printf "re-encoded blob: %d MB\n" (B.length blob `div` 1000000)
    t4 <- getMonotonicTime
    -- The incoming image's half at the CURRENT era: decode the re-encoded
    -- blob and rebuild from it.
    tree2 <- case decodeHandover blob of
        Right h | Right t <- h.tree -> pure t
        _ -> die "re-encoded blob does not round-trip"
    t5 <- getMonotonicTime
    emus2 <- rebuild tree2
    t6 <- getMonotonicTime
    printf "given blob:   decode    %6.2fs  rebuild %6.2fs\n" (t1 - t0) (t2 - t1)
    printf "current era:  recapture %6.2fs  encode  %6.2fs\n" (t3 - t2) (t4 - t3)
    printf "current era:  decode    %6.2fs  rebuild %6.2fs\n" (t5 - t4) (t6 - t5)
    printf "(rehydrated again: %d pane(s))\n" (length emus2)
    -- putStr =<< summarize emus

-- The store-codec JSON of a decoded tree, for re-encoding it into a fresh
-- handover: the inverse of 'hotTree's split.
treeJson :: ReloadTree -> T.Text
treeJson t = encodeSnapshotJson Snapshot
    { sessions = map sessionOf t.sessions
    , lastActiveSession = t.currentSession }
  where
    sessionOf s = SessionSnap
        { name = s.name, startCwd = s.startCwd, currentIx = s.currentIx
        , windowHist = s.windowHist, windows = map windowOf s.windows }
    windowOf w = WindowSnap
        { ix = w.ix, name = w.name, layout = w.layout, active = w.active
        , paneHist = w.paneHist, autoRename = w.autoRename
        , panes = map fst w.panes }

-- Mirror of 'Hat.Server.rebuildReload' down to 'adoptPane', minus the pty
-- adoption and 'ServerState' bookkeeping: the traversal consumes the tree and
-- yields only the emulators, so the tree is collectable once it returns.
rebuild :: ReloadTree -> IO [(String, Emu.Emulator)]
rebuild tree = fmap concat $ forM tree.sessions $ \sess ->
    fmap concat $ forM sess.windows $ \win ->
        forM (zip [0 :: Int ..] win.panes) $ \(ordinal, (_, rp)) -> do
            e <- adopt rp
            pure (T.unpack sess.name <> ":" <> show win.ix <> "." <> show ordinal, e)

adopt :: HotPane -> IO Emu.Emulator
adopt rp = do
    let esz = fromMaybe rebuildSize (captureSize rp.screen)
    e <- Emu.newEmulator esz historyLimit
    let (bytes, sb) = replayPane rp
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
