-- | The status line, as tmux's @status_redraw@ lays it out: each of the
-- @status@ lines expands its @status-format[i]@ entry and draws it with
-- "Hat.Server.FormatDraw" over the status style. The window list, the
-- per-window styles, @status-justify@ and the rest are all read by those
-- formats (tmux's defaults reproduce its classic layout), not here.
module Hat.Server.Status
    ( statusBaseStyle
    , statusRows
    , statusFormats
    , styleOption
    ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import Hat.Model.Options
import Hat.Server.Format (FormatCtx (..), evaluateCtx, renderFormatCtx)
import Hat.Server.FormatDraw (colourValue, formatDraw, styleCell)
import Hat.Term.Cell qualified as Cell

-- | A style option's cell style, its formats expanded first as tmux's
-- @options_string_to_style@ does.
styleOption :: FormatCtx -> Text -> Options -> Cell.Style
styleOption ctx name opts
    | "#{" `T.isInfixOf` raw = styleCell ctx.theme (evaluateCtx ctx raw)
    | otherwise = styleCell ctx.theme raw
  where
    raw = optionText name opts

-- | The status line's base cell style: @status-style@, with @status-fg@
-- and @status-bg@ overriding its colours unless they are @default@.
statusBaseStyle :: FormatCtx -> Options -> Cell.Style
statusBaseStyle ctx opts = sty
    { Cell.fg = over sty.fg (optionText "status-fg" opts)
    , Cell.bg = over sty.bg (optionText "status-bg" opts) }
  where
    sty = styleOption ctx "status-style" opts
    over c t = case colourValue ctx.theme t of
        Just (Just col) | col /= Cell.DefaultColor -> col
        _ -> c

-- | The @status-format@ entries the status line's lines draw.
statusFormats :: Options -> [Text]
statusFormats opts = case optionValue "status-format" opts of
    OVArray m -> [ f | i <- [0 .. opts.statusLines - 1]
                     , Just f <- [Map.lookup (KeyIndex (fromIntegral i)) m] ]
    OVScalar _ -> []

-- | The status line's rows, @width@ cells each: line @i@ draws
-- @status-format[i]@, or is blank when that entry is unset.
statusRows :: FormatCtx -> Options -> Int -> [V.Vector Cell.Cell]
statusRows ctx opts width = map row [0 .. opts.statusLines - 1]
  where
    gc = statusBaseStyle ctx opts
    blank = V.replicate width (Cell.glyphCell ' ' gc)
    formats = case optionValue "status-format" opts of
        OVArray m -> m
        OVScalar _ -> Map.empty
    row i = case Map.lookup (KeyIndex (fromIntegral i)) formats of
        Nothing -> blank
        Just f -> formatDraw ctx.theme gc (renderFormatCtx ctx f) blank
