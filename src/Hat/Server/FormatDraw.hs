-- | Laying an expanded format into cells, as tmux's @format_draw@ does for
-- the text it puts on a mode screen: @#[...]@ directives restyle what
-- follows (@default@ returns to the base style, @acs@\/@noacs@ switch the
-- line-drawing alphabet), and wide characters take two cells.
module Hat.Server.FormatDraw
    ( ColourUse (..)
    , drawFormat
    , formatWidth
    , acsGlyph
    ) where

import Data.Text (Text)
import Data.Text qualified as T

import Hat.Server.Style (applyStyle)
import Hat.Term.Cell qualified as Cell
import Hat.TextWidth (charWidth)

-- | Whether a format's own colours show, or every cell keeps the base
-- style's (tmux draws a selected line that way).
data ColourUse = OwnColours | BaseColours
    deriving (Eq, Show)

-- | The cells a format draws, starting from the @base@ style.
drawFormat :: ColourUse -> Cell.Style -> Text -> [Cell.Cell]
drawFormat use base = go base False
  where
    go sty acs t = case T.breakOn "#[" t of
        (pre, rest) ->
            concatMap (glyph sty acs) (T.unpack pre)
            <> case T.breakOn "]" (T.drop 2 rest) of
                _ | T.null rest -> []
                (spec, close)
                    | T.null close -> concatMap (glyph sty acs) (T.unpack rest)
                    | otherwise ->
                        let (sty', acs') = foldl directive (sty, acs)
                                (map T.strip (T.splitOn "," spec))
                        in go sty' acs' (T.drop 1 close)
    directive (sty, acs) tok = case tok of
        "default" -> (base, acs)
        "acs" -> (sty, True)
        "noacs" -> (sty, False)
        _ -> (colours (applyStyle sty tok), acs)
    colours sty = case use of
        OwnColours -> sty
        BaseColours -> sty { Cell.fg = base.fg, Cell.bg = base.bg }
    glyph sty acs c0 =
        let c = if acs then acsGlyph c0 else c0
        in case charWidth c of
            0 -> []
            2 -> [ Cell.Cell { content = Cell.Glyph c [] Cell.Wide, style = sty }
                 , Cell.Cell { content = Cell.Continuation, style = sty } ]
            _ -> [Cell.glyphCell c sty]

-- | The columns a format takes once drawn.
formatWidth :: Text -> Int
formatWidth = length . drawFormat OwnColours Cell.defaultStyle

-- | The box-drawing character a VT100 alternate-charset letter stands for.
acsGlyph :: Char -> Char
acsGlyph = \case
    'j' -> '┘'
    'k' -> '┐'
    'l' -> '┌'
    'm' -> '└'
    'n' -> '┼'
    'q' -> '─'
    't' -> '├'
    'u' -> '┤'
    'v' -> '┴'
    'w' -> '┬'
    'x' -> '│'
    '~' -> '·'
    'a' -> '▒'
    c -> c
