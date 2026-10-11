-- | Clock mode (tmux's @window-clock.c@): a pane showing the time in big
-- digits, in @clock-mode-colour@ and @clock-mode-style@.
module Hat.Server.Clock
    ( ClockStyle (..)
    , clockStyleOf
    , clockText
    , clockCells
    , digitGlyph
    ) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Data.Vector qualified as V

import Hat.Geometry (Size (..))
import Hat.Term.Cell qualified as Cell

-- | @clock-mode-style@.
data ClockStyle = Clock12 | Clock24 | Clock12Seconds | Clock24Seconds
    deriving (Eq, Show)

clockStyleOf :: Text -> ClockStyle
clockStyleOf = \case
    "12" -> Clock12
    "12-with-seconds" -> Clock12Seconds
    "24-with-seconds" -> Clock24Seconds
    _ -> Clock24

-- | The time as the clock spells it.
clockText :: ClockStyle -> LocalTime -> Text
clockText style t = T.pack $ case style of
    Clock12 -> formatTime defaultTimeLocale "%l:%M " t <> half
    Clock12Seconds -> formatTime defaultTimeLocale "%l:%M:%S " t <> half
    Clock24 -> formatTime defaultTimeLocale "%H:%M" t
    Clock24Seconds -> formatTime defaultTimeLocale "%H:%M:%S" t
  where
    half = if todHour (localTimeOfDay t) >= 12 then "PM" else "AM"

-- | tmux's @window_clock_draw_screen@: a blank pane with the time centred
-- in big glyphs, or as plain text when the pane is too small for them.
clockCells :: Cell.Color -> ClockStyle -> Size -> LocalTime -> V.Vector (V.Vector Cell.Cell)
clockCells colour style size t = V.generate sy (\y -> V.generate sx (cellAt y))
  where
    sx = fromIntegral size.cols
    sy = fromIntegral size.rows
    tim = clockText style t
    len = T.length tim
    big = sx >= 6 * len && sy >= 6
    cellAt y x
        | big, Set.member (x, y) bigCells = Cell.glyphCell '#' block
        | not big, sx >= len, sy /= 0, y == sy `div` 2
        , x >= small0, x < small0 + len =
            Cell.glyphCell (T.index tim (x - small0)) Cell.defaultStyle { Cell.fg = colour }
        | otherwise = Cell.blankCell
    small0 = sx `div` 2 - len `div` 2
    block = Cell.defaultStyle { Cell.fg = colour, Cell.bg = colour }
    x0 = sx `div` 2 - 3 * len
    y0 = sy `div` 2 - 3
    bigCells = Set.fromList
        [ (x0 + 6 * i + c, y0 + r)
        | (i, ch) <- zip [0 ..] (T.unpack tim)
        , (r, bits) <- zip [0 ..] (digitGlyph ch)
        , (c, True) <- zip [0 ..] bits ]

-- | tmux's @window_clock_table@: each glyph's 5 rows of 5 cells.
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
        , (':', [ "....."
                , "..#.."
                , "....."
                , "..#.."
                , "....." ])
        , ('A', [ "#####"
                , "#...#"
                , "#####"
                , "#...#"
                , "#...#" ])
        , ('P', [ "#####"
                , "#...#"
                , "#####"
                , "#...."
                , "#...." ])
        , ('M', [ "#...#"
                , "##.##"
                , "#.#.#"
                , "#...#"
                , "#...#" ])
        ]
