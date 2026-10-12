-- | tmux colour handling: name parsing, the 256-colour palette, and SGR
-- escape rendering, mirroring tmux's
-- @colour.c@ (@colour_fromstring@, @colour_force_rgb@, @colour_toescape@).
module Hat.Colour
    ( Colour (..)
    , colourFromText
    , colourToText
    , colourToHex
    , colourToEscape
    , colourParseX11
    ) where

import Control.Applicative ((<|>))
import Data.Char (digitToInt, isHexDigit)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Numeric (showHex)

import Hat.Colour.X11 (x11Colours)

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
    | otherwise = CRGB <$> colourByName t
  where
    t = T.toLower (T.strip t0)
    indexed pre = do
        rest <- T.stripPrefix pre t
        case TR.decimal rest of
            Right (n, "") | n >= (0 :: Int) && n <= 255 -> Just n
            _ -> Nothing

-- | tmux's @colour_tostring@: the canonical spelling @show-options@ prints.
colourToText :: Colour -> Text
colourToText = \case
    CDefault -> "default"
    CTerminal -> "terminal"
    C256 n -> "colour" <> T.pack (show n)
    CRGB v -> "#" <> T.pack (pad (showHex v ""))
    CBase n
        | n >= 90 -> "bright" <> baseName (n - 90)
        | otherwise -> baseName n
  where
    pad s = replicate (6 - length s) '0' <> s
    baseName i = maybe "invalid" fst (List.find ((== i) . snd) (take 8 namedColours))

-- tmux's @colour_byname@ over a lowercased name: @grayN@\/@greyN@ is a
-- 0-100 ramp, anything else an X11 name.
-- | tmux's @colour_parseX11@: an X11 colour spec, as programs give one
-- in OSC 10\/11\/12, as a packed @0xRRGGBB@.
colourParseX11 :: Text -> Maybe Int
colourParseX11 p
    | T.length p == 12, Just [r, g, b] <- hexes "rgb:" "/" 2 = Just (join r g b)
    | T.length p == 7, Just [r, g, b] <- hexes "#" "" 2 = Just (join r g b)
    | Just [r, g, b] <- decimals = Just (join r g b)
    | T.length p == 18, Just [r, g, b] <- hexes "rgb:" "/" 4 = Just (join16 r g b)
    | T.length p == 13, Just [r, g, b] <- hexes "#" "" 4 = Just (join16 r g b)
    | Just [c, m, y, k] <- fractions "cmyk:" 4 = cmyk c m y k
    | Just [c, m, y] <- fractions "cmy:" 3 = cmyk c m y 0
    | otherwise = colourByName (T.toLower (T.dropAround (== ' ') p))
  where
    join r g b = (r `mod` 256) * 65536 + (g `mod` 256) * 256 + b `mod` 256
    join16 r g b = join (r `div` 256) (g `div` 256) (b `div` 256)
    hexes pre sep n = do
        rest <- T.stripPrefix pre p
        let parts = if T.null sep then T.chunksOf n rest else T.splitOn sep rest
        if length parts == 3 && all (\x -> T.length x == n && T.all isHexDigit x) parts
            then Just (map (T.foldl' (\acc c -> acc * 16 + digitToInt c) 0) parts)
            else Nothing
    decimals = case T.splitOn "," p of
        parts@[_, _, _] -> mapM (\x -> case TR.signed TR.decimal x of
            Right (n, _) -> Just n
            Left _ -> Nothing) parts
        _ -> Nothing
    fractions pre n = do
        rest <- T.stripPrefix pre p
        let parts = T.splitOn "/" rest
        vs <- mapM (\x -> case TR.double x of
            Right (v, _) -> Just v
            Left _ -> Nothing) parts
        if length vs == n && all (\v -> v >= 0 && v <= 1) vs then Just vs else Nothing
    cmyk :: Double -> Double -> Double -> Double -> Maybe Int
    cmyk c m y k = Just (join (channel c) (channel m) (channel y))
      where channel v = truncate ((1 - v) * (1 - k) * 255)

colourByName :: Text -> Maybe Int
colourByName t
    | Just rest <- T.stripPrefix "gray" t <|> T.stripPrefix "grey" t =
        if T.null rest then Just 0xbebebe else case TR.decimal rest of
            Right (n, "") | n <= (100 :: Int) ->
                let c = roundHalfUp (2.55 * fromIntegral n)
                in Just (c * 65536 + c * 256 + c)
            _ -> Nothing
    | otherwise = List.lookup t x11Colours
  where
    roundHalfUp :: Double -> Int
    roundHalfUp x = floor (x + 0.5)

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
