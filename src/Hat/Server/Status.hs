-- | The status line, as tmux's @status_redraw@ lays it out: each of the
-- @status@ lines expands its @status-format[i]@ entry and draws it with
-- "Hat.Server.FormatDraw" over the status style. The window list, the
-- per-window styles, @status-justify@ and the rest are all read by those
-- formats (tmux's defaults reproduce its classic layout), not here.
module Hat.Server.Status
    ( statusBaseStyle
    , statusRows
    , StatusTexts (..)
    , statusTexts
    , drawStatus
    , statusRanges
    , statusFormats
    , styleOption
    ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import Hat.Model.Options
import Hat.Server.Format (FormatCtx (..), evaluateCtx, renderFormatCtx)
import Hat.Server.FormatDraw
    (DrawRange (..), colourValue, formatDraw, formatRanges, styleCell)
import Hat.Server.Theme (Theme)
import Hat.Term.Cell qualified as Cell

-- | A style option's cell style, its formats expanded first as tmux's
-- @options_string_to_style@ does.
styleOption :: FormatCtx -> Text -> Options -> Cell.Style
styleOption ctx name opts = styleCell ctx.theme (styleText ctx name opts)

-- | A style option's text, its formats expanded: see 'styleOption'.
styleText :: FormatCtx -> Text -> Options -> Text
styleText ctx name opts
    | "#{" `T.isInfixOf` raw = evaluateCtx ctx raw
    | otherwise = raw
  where
    raw = optionText name opts

-- | The status line's base cell style: @status-style@, with @status-fg@
-- and @status-bg@ overriding its colours unless they are @default@.
statusBaseStyle :: FormatCtx -> Options -> Cell.Style
statusBaseStyle ctx opts = baseStyleOf ctx.theme opts (styleText ctx "status-style" opts)

-- | 'statusBaseStyle' from @status-style@ expanded.
baseStyleOf :: Theme -> Options -> Text -> Cell.Style
baseStyleOf th opts styleT = sty
    { Cell.fg = over sty.fg (optionText "status-fg" opts)
    , Cell.bg = over sty.bg (optionText "status-bg" opts) }
  where
    sty = styleCell th styleT
    over c t = case colourValue th t of
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
statusRows ctx opts = drawStatus ctx.theme opts (statusTexts ctx opts)

-- | What the status line's drawing expands: @status-style@, and each
-- line's @status-format@ entry ('Nothing' where it is unset).
data StatusTexts = StatusTexts
    { style :: Text
    , lines :: [Maybe Text]
    }

-- | The status line's expansions, for 'drawStatus' to draw.
statusTexts :: FormatCtx -> Options -> StatusTexts
statusTexts ctx opts = StatusTexts
    { style = styleText ctx "status-style" opts
    , lines = [ renderFormatCtx ctx <$> Map.lookup (KeyIndex (fromIntegral i)) formats
              | i <- [0 .. opts.statusLines - 1] ] }
  where
    formats = case optionValue "status-format" opts of
        OVArray m -> m
        OVScalar _ -> Map.empty

-- | The status line's rows from its expansions, @width@ cells each.
drawStatus :: Theme -> Options -> StatusTexts -> Int -> [V.Vector Cell.Cell]
drawStatus th opts texts width = map row texts.lines
  where
    gc = baseStyleOf th opts texts.style
    blank = V.replicate width (Cell.glyphCell ' ' gc)
    row = maybe blank (\t -> formatDraw th gc t blank)

-- | The ranges across status line @i@ (tmux's @status_get_range@ reads
-- them): where a click on each column lands.
statusRanges :: FormatCtx -> Options -> Int -> Int -> V.Vector DrawRange
statusRanges ctx opts width i = case optionValue "status-format" opts of
    OVArray m | Just f <- Map.lookup (KeyIndex (fromIntegral i)) m ->
        formatRanges ctx.theme (statusBaseStyle ctx opts) (renderFormatCtx ctx f) width
    _ -> V.replicate width RangeNone
