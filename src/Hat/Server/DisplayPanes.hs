-- | The @display-panes@ overlay, pure parts: each pane's number stamped
-- over it in tmux's 5x5 block digits, and what a key press does while
-- the overlay shows. 'Hat.Server.View' paints the stamps;
-- 'Hat.Server.Conn' routes the keys.
module Hat.Server.DisplayPanes
    ( PanesKeyAction (..)
    , panesKeyAction
    , panesStamp
    , stampCells
    , panesDeadline
    , armPanes
    , expirePanes
    , dismissPanes
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, void, when)
import Data.Char (digitToInt, isDigit)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Hat.Geometry (Pos (..))
import Hat.Model
import Hat.Model.Options (Options (..))
import Hat.Server.Locate (clientOptions)
import Hat.Term.Cell qualified as Cell

-- | What a key does while the overlay shows. See 'panesKeyAction'.
data PanesKeyAction
    = PanesSelect Int     -- ^ jump to the pane showing this number, close
    | PanesDismiss        -- ^ close, swallowing the key
    | PanesDismissForward -- ^ close; the key is processed normally
    | PanesForward        -- ^ overlay stays; the key is processed normally
    deriving (Eq, Show)

-- | Decide a key against the overlay: a digit selects that pane number,
-- @q@\/@Escape@ dismiss, and anything else falls through to normal key
-- handling — closing the overlay on the way unless @-N@ pinned it open.
panesKeyAction :: PanesKeyHold -> Text -> PanesKeyAction
panesKeyAction hold name
    | Just d <- digitOf = PanesSelect d
    | name == "q" || name == "Escape" = PanesDismiss
    | PanesHold <- hold = PanesForward
    | otherwise = PanesDismissForward
  where
    digitOf = case T.unpack name of
        [c] | isDigit c -> Just (digitToInt c)
        _ -> Nothing

-- | Show the overlay on a client: @delay@ is @-d@ when given, else
-- @display-panes-time@. The timer thread mirrors
-- 'Hat.Server.Toast.showToast'.
armPanes :: ServerState -> Client -> Maybe Int -> PanesKeyHold -> Text -> IO ()
armPanes st client mdelay hold template = do
    opts <- clientOptions st client
    now <- getMonotonicTimeNSec
    let delay = maybe opts.displayPanesTime id mdelay
        dp = DisplayPanesState
            { deadline = panesDeadline delay now
            , hold = hold
            , template = template }
    atomically $ do
        writeTVar client.displayPanes (Just dp)
        bumpDirty st
    forM_ dp.deadline $ \deadline -> void . forkIO $ do
        -- round up so the wakeup lands past the deadline
        threadDelay (fromIntegral ((deadline - now + 999) `div` 1000))
        expirePanes st client

-- | Clear the overlay once its deadline has passed; a fresher overlay (a
-- later deadline, or none) survives an older overlay's timer.
expirePanes :: ServerState -> Client -> IO ()
expirePanes st client = do
    now <- getMonotonicTimeNSec
    atomically $ do
        cur <- readTVar client.displayPanes
        forM_ cur $ \dp ->
            when (maybe False (<= now) dp.deadline) $ do
                writeTVar client.displayPanes Nothing
                bumpDirty st

-- | Clear the overlay on a key press.
dismissPanes :: ServerState -> Client -> IO ()
dismissPanes st client = atomically $ do
    cur <- readTVar client.displayPanes
    forM_ cur $ \_ -> do
        writeTVar client.displayPanes Nothing
        bumpDirty st

-- | The monotonic instant (ns) an overlay shown at @shownAt@ times out:
-- @display-panes-time@ (or @-d@) in ms, @0@ = until a key is pressed.
panesDeadline :: Int -> Word64 -> Maybe Word64
panesDeadline ms shownAt
    | ms <= 0 = Nothing
    | otherwise = Just (shownAt + fromIntegral ms * 1000000)

-- | The cells one pane's number paints, relative to the pane's top-left,
-- in tmux's layout: 5x5 block digits centered in the pane with the pane
-- size right-aligned underneath, or — when the pane cannot fit the
-- blocks — one centered text line of number and size. Never reaches
-- outside the @sx@ x @sy@ box.
panesStamp :: Cell.Color -> Int -> Int -> Int -> [(Pos, Cell.Cell)]
panesStamp colour n sx sy = filter inside stamp
  where
    inside (p, _) = p.row >= 0 && p.row < sy && p.col >= 0 && p.col < sx
    buf = show n
    lbuf = show sx <> "x" <> show sy
    len = length buf
    llen = length lbuf
    bigWidth = 6 * len - 1
    stamp
        | sx < bigWidth || sy < 5 = textAt (sy `div` 2) smallCol small
        | otherwise = blocks <> label
    -- small fallback: the number, plus the size when both fit
    small
        | sx >= len + llen + 1 = buf <> " " <> lbuf
        | otherwise = buf
    smallCol = (sx - length small) `div` 2
    px0 = (sx - bigWidth) `div` 2
    py0 = (sy - 5) `div` 2
    blocks =
        [ (Pos (py0 + r) (px0 + 6 * d + c), block)
        | (d, ch) <- zip [0 ..] buf
        , (r, rowBits) <- zip [0 ..] (digitGlyph ch)
        , (c, set) <- zip [0 ..] rowBits
        , set ]
    label
        | sy <= 6 = []
        | otherwise = textAt (py0 + 5) (px0 + 6 * len - 1 - llen) lbuf
    textAt r c0 s =
        [ (Pos r (c0 + i), glyph ch) | (i, ch) <- zip [0 ..] s ]
    glyph ch = Cell.glyphCell ch Cell.defaultStyle { Cell.fg = colour }
    block = Cell.blankCell { Cell.style = Cell.defaultStyle { Cell.bg = colour } }

-- | Paint absolute-positioned cells onto a frame, dropping any outside it.
stampCells
    :: [(Pos, Cell.Cell)]
    -> V.Vector (V.Vector Cell.Cell) -> V.Vector (V.Vector Cell.Cell)
stampCells stamps frame = frame V.//
    [ (r, (frame V.! r) V.// cols) | (r, cols) <- Map.toList byRow ]
  where
    byRow = Map.fromListWith (<>)
        [ (p.row, [(p.col, c)])
        | (p, c) <- stamps
        , p.row >= 0, p.row < V.length frame
        , p.col >= 0, p.col < maybe 0 V.length (frame V.!? p.row) ]

-- | tmux's @window_clock_table@ digit glyphs: 5 rows of 5 bits.
digitGlyph :: Char -> [[Bool]]
digitGlyph ch = maybe [] (map (map (== '#'))) (lookup ch table)
  where
    table =
        [ ('0', [ "#####"
                , "#...#"
                , "#...#"
                , "#...#"
                , "#####" ])
        , ('1', [ "....#"
                , "....#"
                , "....#"
                , "....#"
                , "....#" ])
        , ('2', [ "#####"
                , "....#"
                , "#####"
                , "#...."
                , "#####" ])
        , ('3', [ "#####"
                , "....#"
                , "#####"
                , "....#"
                , "#####" ])
        , ('4', [ "#...#"
                , "#...#"
                , "#####"
                , "....#"
                , "....#" ])
        , ('5', [ "#####"
                , "#...."
                , "#####"
                , "....#"
                , "#####" ])
        , ('6', [ "#####"
                , "#...."
                , "#####"
                , "#...#"
                , "#####" ])
        , ('7', [ "#####"
                , "....#"
                , "....#"
                , "....#"
                , "....#" ])
        , ('8', [ "#####"
                , "#...#"
                , "#####"
                , "#...#"
                , "#####" ])
        , ('9', [ "#####"
                , "#...#"
                , "#####"
                , "....#"
                , "#####" ])
        ]
