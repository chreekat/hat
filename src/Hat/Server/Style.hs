-- | Parse tmux style strings (@fg=colour240,bold@, @bg=colour59@, …) into
-- 'Cell.Style'. Unknown tokens are ignored, matching tmux's tolerance.
module Hat.Server.Style
    ( parseStyle
    , parseColor
    ) where

import Data.Char (digitToInt, isHexDigit)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Word (Word8)

import qualified Hat.Term.Cell as Cell

-- | Fold a comma-separated style string onto the default style.
parseStyle :: Text -> Cell.Style
parseStyle = List.foldl' apply Cell.defaultStyle . map T.strip . T.splitOn ","
  where
    apply sty tok
        | Just c <- T.stripPrefix "fg=" tok = sty { Cell.fg = parseColor c }
        | Just c <- T.stripPrefix "bg=" tok = sty { Cell.bg = parseColor c }
        | tok `elem` ["bold", "bright"] = sty { Cell.bold = True }
        | tok == "nobold" = sty { Cell.bold = False }
        | tok `elem` ["underscore", "underline"] = sty { Cell.underline = True }
        | tok `elem` ["italics", "italic"] = sty { Cell.italic = True }
        | tok == "reverse" = sty { Cell.reverse = True }
        | tok == "blink" = sty { Cell.blink = True }
        | tok `elem` ["strikethrough", "strike"] = sty { Cell.strike = True }
        | otherwise = sty  -- "default", "none", empty, unknown: no change

-- | A tmux colour name: @default@, @colourN@/@colorN@, a base or bright
-- name, or @#rrggbb@.
parseColor :: Text -> Cell.Color
parseColor t
    | t == "default" = Cell.DefaultColor
    | Just n <- indexed "colour" = Cell.Indexed n
    | Just n <- indexed "color" = Cell.Indexed n
    | Just rgb <- hexColor t = rgb
    | Just n <- List.lookup t namedColors = Cell.Indexed n
    | otherwise = Cell.DefaultColor
  where
    indexed pre = do
        rest <- T.stripPrefix pre t
        case TR.decimal rest of
            Right (n, "") | n >= (0 :: Int) && n <= 255 -> Just (fromIntegral n)
            _ -> Nothing

namedColors :: [(Text, Word8)]
namedColors = zip names [0 ..] <> zip (map ("bright" <>) names) [8 ..]
  where
    names = ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"]

hexColor :: Text -> Maybe Cell.Color
hexColor t = case T.stripPrefix "#" t of
    Just rest
        | s <- T.unpack rest, length s == 6, all isHexDigit s ->
            case s of
                [r1, r2, g1, g2, b1, b2] ->
                    Just (Cell.RGB (hx r1 r2) (hx g1 g2) (hx b1 b2))
                _ -> Nothing
    _ -> Nothing
  where
    hx a b = fromIntegral (digitToInt a * 16 + digitToInt b)
