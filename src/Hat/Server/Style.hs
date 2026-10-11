-- | Parse tmux style strings (@fg=colour240,bold@, @bg=colour59@, …) into
-- 'Cell.Style'. Unknown tokens are ignored, matching tmux's tolerance.
module Hat.Server.Style
    ( parseStyle
    , parseColor
    , styleValid
    , colourValid
    , styleText
    ) where

import Data.Char (digitToInt, isHexDigit)
import Data.Maybe (isJust)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Word (Word8)

import Hat.Server.Format.Colour (colourFromText)
import Hat.Term.Cell qualified as Cell

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
        | tok == "dim" = sty { Cell.faint = True }
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

-- | tmux's @style_parse@ acceptance: whether every token of a style string
-- is one tmux understands (a set rejects the string otherwise).
styleValid :: Text -> Bool
styleValid = all tokenValid . filter (not . T.null) . T.split (`elem` (" ,\n" :: String))
  where
    tokenValid tok0 = T.length tok0 <= 255 && valid (T.toLower tok0)
    valid tok
        | tok `elem` keywords = True
        | Just v <- T.stripPrefix "list=" tok =
            v `elem` ["on", "focus", "left-marker", "right-marker"]
        | Just v <- nonEmpty "range=" tok = rangeValid v
        | Just v <- nonEmpty "align=" tok =
            v `elem` ["left", "centre", "right", "absolute-centre"]
        | Just v <- nonEmpty "fill=" tok = colourValid v
        | Just v <- nonEmpty "dim=" tok = numberIn 0 100 (dropPercent v)
        | Just v <- nonEmpty "fg=" tok = colourValid v
        | Just v <- nonEmpty "bg=" tok = colourValid v
        | Just v <- nonEmpty "us=" tok = colourValid v
        | T.length tok > 2, Just v <- T.stripPrefix "no" tok =
            v == "link" || v == "attr" || attributesValid v
        | Just v <- nonEmpty "width=" tok = case T.unsnoc v of
            Just (n, '%') | not (T.null n) -> numberIn 0 100 n
            _ -> numberIn 0 4294967295 v
        | Just v <- nonEmpty "pad=" tok = numberIn 0 4294967295 v
        | "link=" `T.isPrefixOf` tok = True
        | otherwise = attributesValid tok
    keywords =
        [ "default", "ignore", "noignore", "push-default", "pop-default"
        , "set-default", "nolist", "norange", "noalign", "none" ]
    nonEmpty pre tok = case T.stripPrefix pre tok of
        Just v | not (T.null v) -> Just v
        _ -> Nothing
    dropPercent v = maybe v id (T.stripSuffix "%" v)
    rangeValid v = case T.breakOn "|" v of
        (kind, rest)
            | T.null rest -> kind `notElem` ["control", "pane", "window", "session", "user"]
            | otherwise ->
                let arg = T.drop 1 rest
                in not (T.null arg) && case kind of
                    "left" -> False
                    "right" -> False
                    "control" -> numberIn 0 9 arg
                    "pane" -> prefixed '%' arg
                    "window" -> numberIn 0 4294967295 arg
                    "session" -> prefixed '$' arg
                    _ -> True
    prefixed c arg = case T.uncons arg of
        Just (c', n) | c' == c, not (T.null n) -> numberIn 0 4294967295 n
        _ -> False

-- | tmux's @attributes_fromstring@ acceptance, over a lowercased string.
attributesValid :: Text -> Bool
attributesValid t
    | t `elem` ["default", "none"] = True
    | T.null t || T.any (`elem` delims) (T.take 1 t)
        || T.any (`elem` delims) (T.takeEnd 1 t) = False
    | otherwise = all (`elem` names) (filter (not . T.null) (T.split (`elem` delims) t))
  where
    delims = " ,|" :: String
    names =
        [ "acs", "bright", "bold", "dim", "underscore", "blink", "reverse"
        , "hidden", "italics", "strikethrough", "double-underscore"
        , "curly-underscore", "dotted-underscore", "dashed-underscore"
        , "overline" ]

-- | tmux's @colour_fromstring@ acceptance, including the theme colours.
colourValid :: Text -> Bool
colourValid t = T.toLower t `elem` themeColours || isJust (colourFromText t)
  where
    themeColours =
        [ "themeblack", "themewhite", "themelightgrey", "themedarkgrey"
        , "themegreen", "themeyellow", "themered", "themeblue", "themecyan"
        , "thememagenta" ]

-- | @strtonum@ acceptance over a plain decimal.
numberIn :: Integer -> Integer -> Text -> Bool
numberIn lo hi v = case TR.signed TR.decimal v of
    Right (n, "") -> n >= lo && n <= hi
    _ -> False

-- | A parsed style's display form, which 'parseStyle' reads back.
styleText :: Cell.Style -> Text
styleText s = case parts of
    [] -> "default"
    ps -> T.intercalate "," ps
  where
    parts = concat
        [ [ "fg=" <> colorText s.fg | s.fg /= Cell.DefaultColor ]
        , [ "bg=" <> colorText s.bg | s.bg /= Cell.DefaultColor ]
        , [ "bold" | s.bold ], [ "underscore" | s.underline ]
        , [ "italics" | s.italic ], [ "reverse" | s.reverse ]
        , [ "strikethrough" | s.strike ], [ "blink" | s.blink ]
        , [ "dim" | s.faint ]
        ]
    colorText = \case
        Cell.DefaultColor -> "default"
        Cell.Indexed n -> "colour" <> T.pack (show n)
        Cell.RGB r g b -> T.pack ('#' : concatMap hex2 [r, g, b])
    hex2 w = [hexDigit (w `div` 16), hexDigit (w `mod` 16)]
    hexDigit d
        | d < 10 = toEnum (fromEnum '0' + fromIntegral d)
        | otherwise = toEnum (fromEnum 'a' + fromIntegral d - 10)
