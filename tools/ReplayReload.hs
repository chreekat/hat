-- | Replay a preserved reload handover through the emulator restore path.
--
-- The handover blob (kept as @<socket>.reload.last@) reproduces a pane's
-- restore-then-resize offline: this replays every pane in the blob — restore
-- at the rebuild size (24x80), then resize through a ladder of geometries,
-- once against a fresh restore per size, then chained on one emulator. Each
-- step is printed before it runs, so a crash names its pane and geometry on
-- the last line. (It was written to reproduce a native resize abort in the old
-- libvterm backend, since replaced by libghostty-vt.)
--
-- Run from the repo root:
-- @cabal run replay-reload -- /tmp/hat-1000/default.reload.last [ROWSxCOLS ...]@
module Main (main) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

import Hat.Geometry
import Hat.Server (captureSize, replayPane)
import Hat.Server.Reload
import qualified Hat.Term.Emulator as Emu

-- The fallback size 'adoptPane' uses for a blank capture; a real capture is
-- adopted at its own size ('captureSize'), which this tool mirrors.
rebuildSize :: Size
rebuildSize = Size { rows = 24, cols = 80 }

-- Matches the deployed config's @history-limit@, so seeded scrollback trims
-- the same way it does in the crashing server.
historyLimit :: Int
historyLimit = 50000

-- The geometries the reconcile daemon and a client attach actually drive
-- (from the field logs' PaneResizing events). Extreme shrinks (5x20, 2x2)
-- are opt-in via arguments, not part of the default reload validation; the
-- resize abort they once triggered on the old libvterm backend is long fixed.
defaultLadder :: [Size]
defaultLadder =
    [ Size 11 80, Size 23 80, Size 42 330, Size 85 330, Size 24 80 ]

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    (path, ladder) <- case args of
        [] -> die "usage: replay-reload <blob> [ROWSxCOLS ...]"
        (p : szs) -> (p,) <$> case szs of
            [] -> pure defaultLadder
            _  -> mapM parseSize szs
    bs <- B.readFile path
    tree <- case decodeHandover bs of
        Left e -> die ("undecodable blob: " <> T.unpack e)
        Right h -> case h.tree of
            Left e -> die ("unusable tree: " <> T.unpack e)
            Right t -> pure t
    let panes =
            [ (sess.name, w.ix, ordinal, rp)
            | sess <- tree.sessions
            , w <- sess.windows
            , (ordinal, rp) <- zip [0 :: Int ..] w.panes
            ]
    putStrLn ("replaying " <> show (length panes) <> " pane(s) through "
              <> show (length ladder) <> " size(s)")
    forM_ panes $ \(sname, wix, ordinal, rp) -> do
        let label = T.unpack sname <> ":" <> show wix <> "." <> show ordinal
        forM_ ladder $ \sz -> do
            putStrLn (label <> " fresh restore, resize to " <> showSize sz)
            e <- restored rp
            Emu.resize e sz
        putStrLn (label <> " chained resizes: "
                  <> unwords (map showSize ladder))
        e <- restored rp
        forM_ ladder $ \sz -> do
            putStrLn (label <> "   -> " <> showSize sz)
            Emu.resize e sz
    putStrLn "all panes survived"
  where
    restored rp = do
        let esz = fromMaybe rebuildSize (captureSize rp.screen)
        e <- Emu.newEmulator esz historyLimit
        let (bytes, sb) = replayPane esz rp
        _ <- Emu.feed e bytes
        Emu.seedScrollback e sb
        pure e

showSize :: Size -> String
showSize sz = show sz.rows <> "x" <> show sz.cols

parseSize :: String -> IO Size
parseSize s = case break (== 'x') s of
    (r, 'x' : c)
        | [(ri, "")] <- reads r, [(ci, "")] <- reads c, ri > 0, ci > 0 -> do
            unless (ri < 10000 && ci < 10000) (die ("size out of range: " <> s))
            pure Size { rows = ri, cols = ci }
    _ -> die ("bad size (want ROWSxCOLS): " <> s)
