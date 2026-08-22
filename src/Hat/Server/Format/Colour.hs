-- | tmux colour handling for the @#{c:...}@ format modifier: name parsing,
-- the 256-colour palette, and SGR escape rendering, mirroring tmux's
-- @colour.c@ (@colour_fromstring@, @colour_force_rgb@, @colour_toescape@).
module Hat.Server.Format.Colour
    ( Colour (..)
    , colourFromText
    , colourToHex
    , colourToEscape
    ) where

import Data.Char (digitToInt, isHexDigit)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Numeric (showHex)

-- | A parsed tmux colour. 'CBase' holds the classic SGR index (0-7 or the
-- bright 90-97); 'C256' a palette index; 'CRGB' a packed @0xRRGGBB@.
data Colour
    = CBase Int
    | CDefault
    | CTerminal
    | C256 Int
    | CRGB Int
    deriving stock (Eq, Show)

colourFromText :: Text -> Maybe Colour
colourFromText t0
    | t == "default" = Just CDefault
    | t == "terminal" = Just CTerminal
    | Just n <- indexed "colour" = Just (C256 n)
    | Just n <- indexed "color" = Just (C256 n)
    | Just rgb <- hexColour t = Just (CRGB rgb)
    | Just n <- List.lookup t namedColours = Just (CBase n)
    | otherwise = Nothing
  where
    t = T.toLower (T.strip t0)
    indexed pre = do
        rest <- T.stripPrefix pre t
        case TR.decimal rest of
            Right (n, "") | n >= (0 :: Int) && n <= 255 -> Just n
            _ -> Nothing

-- Base names, their bright forms, and the numeric aliases tmux accepts.
namedColours :: [(Text, Int)]
namedColours =
    zip names [0 ..] <> zip (map ("bright" <>) names) [90 ..]
    <> zip (map tshow [0 .. 7 :: Int]) [0 ..]
    <> zip (map tshow [90 .. 97 :: Int]) [90 ..]
  where
    names = ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"]
    tshow = T.pack . show

hexColour :: Text -> Maybe Int
hexColour t = case T.unpack <$> T.stripPrefix "#" t of
    Just s@[_, _, _, _, _, _] | all isHexDigit s ->
        Just (List.foldl' (\acc c -> acc * 16 + digitToInt c) 0 s)
    _ -> Nothing

-- | The colour forced to RGB as six lowercase hex digits; 'Nothing' for
-- the terminal defaults, which have no fixed RGB value.
colourToHex :: Colour -> Maybe Text
colourToHex col = pack <$> case col of
    CRGB v -> Just v
    C256 n -> Just (palette256 n)
    CBase n
        | n <= 7 -> Just (palette256 n)
        | otherwise -> Just (palette256 (8 + n - 90))
    CDefault -> Nothing
    CTerminal -> Nothing
  where
    pack v = T.pack (pad (showHex v ""))
    pad s = replicate (6 - length s) '0' <> s

-- | The SGR escape selecting the colour as foreground or background.
colourToEscape :: Bool -> Colour -> Text
colourToEscape bg col = "\ESC[" <> body <> "m"
  where
    o = if bg then 40 else 30 :: Int
    body = case col of
        CDefault -> tshow (o + 9)
        CTerminal -> tshow (o + 9)
        CRGB v -> T.intercalate ";"
            [ tshow (o + 8), "2"
            , tshow (v `div` 65536), tshow (v `div` 256 `mod` 256), tshow (v `mod` 256) ]
        C256 n -> tshow (o + 8) <> ";5;" <> tshow n
        CBase n
            | n <= 7 -> tshow (n + o)
            | otherwise -> tshow (n + o - 30)
    tshow = T.pack . show

-- The xterm 256-colour palette as packed RGB.
palette256 :: Int -> Int
palette256 n
    | n < 16 = base16 !! n
    | n < 232 =
        let i = n - 16
            lvl x = if x == 0 then 0 else 55 + x * 40
            r = lvl (i `div` 36)
            g = lvl (i `div` 6 `mod` 6)
            b = lvl (i `mod` 6)
        in r * 65536 + g * 256 + b
    | n < 256 = let v = 8 + 10 * (n - 232) in v * 65536 + v * 256 + v
    | otherwise = 0
  where
    base16 =
        [ 0x000000, 0x800000, 0x008000, 0x808000
        , 0x000080, 0x800080, 0x008080, 0xc0c0c0
        , 0x808080, 0xff0000, 0x00ff00, 0xffff00
        , 0x0000ff, 0xff00ff, 0x00ffff, 0xffffff
        ]
