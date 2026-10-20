-- | Turn 'DrawOp's into the escape-sequence bytes the client blits to
-- its tty, in its terminal's 'Dialect'. Pure; see 'opsToAnsi'.
module Hat.Client.Draw
    ( Dialect
    , dialectOf
    , legacyDialect
    , opsToAnsi
    , sgr
    , titleBytes
    , enterAltScreen
    , featuresOn
    , leaveAltScreen
    , lockScreen
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)

import Hat.Geometry
import Hat.Term.Caps
import Hat.Term.Cell
import Hat.TextWidth (charWidth)
import Hat.Transport.Wire (DrawOp (..))

-- | What the client's terminal can be sent, as tmux's terminal model
-- ("Hat.Term.Caps") decides it: the choices the encoder makes per frame.
-- Cursor motion, clearing and SGR colour numbers assume a VT100-like
-- terminal, as tmux's do where it writes them directly.
data Dialect = Dialect
    { depth :: ColourDepth
    , attrs :: AttrCodes
    , clipCorner :: Bool  -- ^ never write the bottom-right cell (no @am@)
    , sync :: Bool        -- ^ bracket frames in DEC 2026 (@Sync@)
    , title :: Maybe (ByteString, ByteString)  -- ^ @tsl@ and @fsl@
    , altOn :: ByteString   -- ^ @smcup@
    , altOff :: ByteString  -- ^ @rmcup@
    , pasteOn :: ByteString   -- ^ @Enbp@
    , pasteOff :: ByteString  -- ^ @Dsbp@
    , focusOn :: ByteString   -- ^ @Enfcs@
    , focusOff :: ByteString  -- ^ @Dsfcs@
    , cursorColourReset :: ByteString  -- ^ @Cr@
    , cursorStyleReset :: ByteString   -- ^ @Se@, else @Ss@ with 0
    , mouseOff :: ByteString  -- ^ mouse modes off, when it has @kmous@
    }

-- | How many colours a terminal shows (tmux's @TERM_RGBCOLOURS@,
-- @TERM_256COLOURS@, else its @colors@).
data ColourDepth = DepthRGB | Depth256 | Depth Int
    deriving (Eq)

-- | How each attribute is turned on: an SGR parameter merged into the
-- run's one SGR, the capability's own bytes, or not at all.
data AttrCode = AttrSgr Int | AttrRaw ByteString | AttrNone

data AttrCodes = AttrCodes
    { bold, faint, italic, underline, blink, reverse, strike :: AttrCode }

dialectOf :: TtyTerm -> Dialect
dialectOf t = Dialect
    { depth
    , attrs = AttrCodes
        { bold = code "bold"
        , faint = code "dim"
        , italic = if hasCap "sitm" t then code "sitm" else code "smso"
        , underline = code "smul"
        , blink = code "blink"
        , reverse = code "rev"
        , strike = code "smxx"
        }
    , clipCorner = noAutoMargin t
    , sync = hasCap "Sync" t
    , title = if hasCap "tsl" t && hasCap "fsl" t
        then Just (str "tsl", str "fsl") else Nothing
    , altOn = str "smcup"
    , altOff = str "rmcup"
    , pasteOn = str "Enbp"
    , pasteOff = str "Dsbp"
    , focusOn = str "Enfcs"
    , focusOff = str "Dsfcs"
    , cursorColourReset = str "Cr"
    , cursorStyleReset
    , mouseOff = if hasCap "kmous" t then mouseModesOff else ""
    }
  where
    depth
        | rgbColours t = DepthRGB
        | colours256 t || capNumber "colors" t >= 256 = Depth256
        | otherwise = Depth (capNumber "colors" t)
    cursorStyleReset
        | hasCap "Se" t = str "Se"
        | hasCap "Ss" t = "\ESC[0 q"
        | otherwise = ""
    str n = TE.encodeUtf8 (capString n t)
    code n
        | not (hasCap n t) = AttrNone
        | otherwise = case sgrParam (capString n t) of
            Just p -> AttrSgr p
            Nothing -> AttrRaw (str n)

-- | The parameter of a plain one-parameter SGR, @\\E[Nm@.
sgrParam :: Text -> Maybe Int
sgrParam s = do
    body <- T.stripPrefix "\ESC[" s >>= T.stripSuffix "m"
    if not (T.null body) && T.all (`elem` ['0' .. '9']) body && T.length body <= 3
        then Just (read (T.unpack body))
        else Nothing

-- | The fixed xterm dialect a client draws in when its server never sent
-- the terminal it opened (one that predates it).
legacyDialect :: Dialect
legacyDialect = Dialect
    { depth = DepthRGB
    , attrs = AttrCodes
        { bold = AttrSgr 1, faint = AttrSgr 2, italic = AttrSgr 3
        , underline = AttrSgr 4, blink = AttrSgr 5, reverse = AttrSgr 7
        , strike = AttrSgr 9 }
    , clipCorner = False
    , sync = True
    , title = Just ("\ESC]0;", "\BEL")
    , altOn = "\ESC[?1049h"
    , altOff = "\ESC[?1049l"
    , pasteOn = "\ESC[?2004h"
    , pasteOff = "\ESC[?2004l"
    , focusOn = "\ESC[?1004h"
    , focusOff = "\ESC[?1004l"
    , cursorColourReset = "\ESC]112\a"
    , cursorStyleReset = "\ESC[0 q"
    , mouseOff = mouseModesOff
    }

mouseModesOff :: ByteString
mouseModesOff = "\ESC[?1006l\ESC[?1000l\ESC[?1002l\ESC[?1003l"

-- | A batch of ops for a terminal of the given size. Under 'sync' it is
-- bracketed in DEC 2026 synchronized output (BSU/ESU), so the terminal
-- presents the whole frame at once — a full redraw's clear-then-repaint
-- would otherwise flicker — and always in hide-cursor / restore-cursor so
-- the cursor never trails the paint.
opsToAnsi :: Dialect -> Size -> [DrawOp] -> ByteString
opsToAnsi d sz ops = BL.toStrict . BB.toLazyByteString $
    (if d.sync then BB.byteString "\ESC[?2026h" else mempty)
    <> BB.byteString "\ESC[?25l"
    <> foldMap (opBuilder d sz) ops
    <> restoreCursor
    <> (if d.sync then BB.byteString "\ESC[?2026l" else mempty)
  where
    -- The renderer puts CursorAt last; if a batch lacks one (pure
    -- content update), leave the cursor hidden state to the final op
    -- of the previous batch by showing it again unconditionally only
    -- when some CursorAt asked for it.
    restoreCursor = case [v | CursorAt _ v <- ops] of
        [] -> mempty
        vs | last vs -> BB.byteString "\ESC[?25h"
           | otherwise -> mempty

opBuilder :: Dialect -> Size -> DrawOp -> BB.Builder
opBuilder d sz = \case
    ClearAll -> BB.byteString "\ESC[0m\ESC[2J\ESC[H"
    Put pos st txt ->
        moveTo pos <> sgrBuilder d st <> TE.encodeUtf8Builder (clip pos txt)
    CursorAt pos _visible -> moveTo pos
  where
    -- tmux's TERM_NOAM: writing the bottom-right cell would scroll.
    clip pos txt
        | d.clipCorner, pos.row == fromIntegral sz.rows - 1 =
            fitColumns (fromIntegral sz.cols - 1 - pos.col) txt
        | otherwise = txt

-- | The longest prefix of a run that fits in the columns, keeping the
-- zero-width characters that close it.
fitColumns :: Int -> Text -> Text
fitColumns room txt = T.take (go 0 0 (T.unpack txt)) txt
  where
    go :: Int -> Int -> String -> Int
    go n _ [] = n
    go n used (c : cs)
        | used + w > room = n
        | otherwise = go (n + 1) (used + w) cs
      where
        w = charWidth c

moveTo :: Pos -> BB.Builder
moveTo p = BB.byteString "\ESC["
    <> BB.intDec (p.row + 1) <> BB.char8 ';'
    <> BB.intDec (p.col + 1) <> BB.char8 'H'

-- | Full SGR for a style, starting from reset so runs are independent.
sgr :: Dialect -> Style -> ByteString
sgr d = BL.toStrict . BB.toLazyByteString . sgrBuilder d

sgrBuilder :: Dialect -> Style -> BB.Builder
sgrBuilder d st0 =
    BB.byteString "\ESC[0"
    <> param st.bold a.bold
    <> param st.faint a.faint
    <> param st.italic a.italic
    <> param st.underline a.underline
    <> param st.blink a.blink
    <> param st.reverse a.reverse
    <> param st.strike a.strike
    <> colourCode Fg d.depth st.fg
    <> colourCode Bg d.depth st.bg
    <> BB.char8 'm'
    <> raw st.bold a.bold
    <> raw st.faint a.faint
    <> raw st.italic a.italic
    <> raw st.underline a.underline
    <> raw st.blink a.blink
    <> raw st.reverse a.reverse
    <> raw st.strike a.strike
  where
    a = d.attrs
    st = fitColours d.depth st0
    param on = \case
        AttrSgr n | on -> BB.char8 ';' <> BB.intDec n
        _ -> mempty
    raw on = \case
        AttrRaw bs | on -> BB.byteString bs
        _ -> mempty

data Layer = Fg | Bg

-- | A style's colours as a terminal of the given depth can show them
-- (tmux's @tty_check_fg@ and @tty_check_bg@): 24-bit colours to the 256
-- palette, the 256 palette to the 16 colours, and bright colours to bold
-- (foreground) or plain (background) on an 8-colour terminal.
fitColours :: ColourDepth -> Style -> Style
fitColours depth st = case depth of
    DepthRGB -> st
    Depth256 -> st { fg = to256 st.fg, bg = to256 st.bg }
    Depth n ->
        let fg16 = to16 n (to256 st.fg)
            bg16 = to16 n (to256 st.bg)
            (fg', boldUp) = case fg16 of
                Bright c | n < 16 -> (Indexed c, True)
                Converted c | n < 16 -> (Indexed (avoidSame c bg16), False)
                other -> (plain other, False)
        in st { fg = fg', bg = plainBg n bg16, bold = st.bold || boldUp }
  where
    to256 = \case
        RGB r g b -> Indexed (findRgb r g b)
        c -> c
    -- Mapping to black-on-black or white-on-white is not much use, so the
    -- foreground changes.
    avoidSame c bg16
        | c == 0, plain bg16 == Indexed 0 = 7
        | c == 7, plain bg16 == Indexed 7 = 0
        | otherwise = c
    plainBg n = \case
        Bright c | n < 16 -> Indexed c
        Converted c | n < 16 -> Indexed c
        other -> plain other

-- | A colour reduced to 16, remembering whether a bright one was bright to
-- start with (aixterm) or became so from the 256 palette.
data Colour16 = Kept Color | Bright Word8 | Converted Word8

to16 :: Int -> Color -> Colour16
to16 n = \case
    Indexed i
        | i < 8 -> Kept (Indexed i)
        | i < 16 -> if n < 16 then Bright (i - 8) else Kept (Indexed i)
        | otherwise -> case colour256to16 i of
            c | c < 8 -> Kept (Indexed c)
              | otherwise -> Converted (c - 8) `orBright` n
    c -> Kept c
  where
    orBright (Converted c) k | k >= 16 = Kept (Indexed (c + 8))
    orBright x _ = x

plain :: Colour16 -> Color
plain = \case
    Kept c -> c
    Bright c -> Indexed (c + 8)
    Converted c -> Indexed c

colourCode :: Layer -> ColourDepth -> Color -> BB.Builder
colourCode layer depth = \case
    DefaultColor -> mempty
    Indexed n
        | n < 8 -> BB.char8 ';' <> BB.intDec (base + fromIntegral n)
        | n < 16, depth /= DepthRGB, depth /= Depth256 ->
            BB.char8 ';' <> BB.intDec (base + 60 + fromIntegral n - 8)
        | otherwise -> BB.byteString (ext "5;") <> BB.intDec (fromIntegral n)
    RGB r g b -> BB.byteString (ext "2;")
        <> BB.intDec (fromIntegral r) <> BB.char8 ';'
        <> BB.intDec (fromIntegral g) <> BB.char8 ';'
        <> BB.intDec (fromIntegral b)
  where
    (base, ext) = case layer of
        Fg -> (30, (";38;" <>))
        Bg -> (40, (";48;" <>))

-- | The nearest colour of the xterm 256 palette (tmux's
-- @colour_find_rgb@): the 6x6x6 cube or the grey ramp, whichever is closer.
findRgb :: Word8 -> Word8 -> Word8 -> Word8
findRgb r8 g8 b8
    | (cr, cg, cb) == (r, g, b) = cube
    | dist (grey, grey, grey) < dist (cr, cg, cb) = fromIntegral (232 + greyIdx)
    | otherwise = cube
  where
    (r, g, b) = (fromIntegral r8, fromIntegral g8, fromIntegral b8) :: (Int, Int, Int)
    to6 v | v < 48 = 0
          | v < 114 = 1
          | otherwise = (v - 35) `div` 40
    q2c i = [0x00, 0x5f, 0x87, 0xaf, 0xd7, 0xff] !! i
    (qr, qg, qb) = (to6 r, to6 g, to6 b)
    (cr, cg, cb) = (q2c qr, q2c qg, q2c qb)
    cube = fromIntegral (16 + 36 * qr + 6 * qg + qb)
    avg = (r + g + b) `div` 3
    greyIdx = if avg > 238 then 23 else (avg - 3) `div` 10
    grey = 8 + 10 * greyIdx
    dist (x, y, z) = (x - r) * (x - r) + (y - g) * (y - g) + (z - b) * (z - b)

-- | The nearest of the 16 colours to a 256-palette one (tmux's
-- @colour_256to16@).
colour256to16 :: Word8 -> Word8
colour256to16 i = table !! fromIntegral i
  where
    table =
        [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
        , 0, 4, 4, 4, 12, 12, 2, 6, 4, 4, 12, 12, 2, 2, 6, 4
        , 12, 12, 2, 2, 2, 6, 12, 12, 10, 10, 10, 10, 14, 12, 10, 10
        , 10, 10, 10, 14, 1, 5, 4, 4, 12, 12, 3, 8, 4, 4, 12, 12
        , 2, 2, 6, 4, 12, 12, 2, 2, 2, 6, 12, 12, 10, 10, 10, 10
        , 14, 12, 10, 10, 10, 10, 10, 14, 1, 1, 5, 4, 12, 12, 1, 1
        , 5, 4, 12, 12, 3, 3, 8, 4, 12, 12, 2, 2, 2, 6, 12, 12
        , 10, 10, 10, 10, 14, 12, 10, 10, 10, 10, 10, 14, 1, 1, 1, 5
        , 12, 12, 1, 1, 1, 5, 12, 12, 1, 1, 1, 5, 12, 12, 3, 3
        , 3, 7, 12, 12, 10, 10, 10, 10, 14, 12, 10, 10, 10, 10, 10, 14
        , 9, 9, 9, 9, 13, 12, 9, 9, 9, 9, 13, 12, 9, 9, 9, 9
        , 13, 12, 9, 9, 9, 9, 13, 12, 11, 11, 11, 11, 7, 12, 10, 10
        , 10, 10, 10, 14, 9, 9, 9, 9, 9, 13, 9, 9, 9, 9, 9, 13
        , 9, 9, 9, 9, 9, 13, 9, 9, 9, 9, 9, 13, 9, 9, 9, 9
        , 9, 13, 11, 11, 11, 11, 11, 15, 0, 0, 0, 0, 0, 0, 8, 8
        , 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 15, 15, 15, 15, 15, 15 ]

-- | A window title (tmux's @tty_set_title@): nothing on a terminal
-- without @tsl@ and @fsl@.
titleBytes :: Dialect -> Text -> ByteString
titleBytes d t = case d.title of
    Just (start, end) -> start <> TE.encodeUtf8 t <> end
    Nothing -> ""

-- Also enables focus reporting so the outer terminal sends
-- @\ESC[I@/@\ESC[O@; the server forwards them to panes when
-- @focus-events@ is on. And bracketed paste, so a paste reaches the pane
-- whole rather than through the key tables.
enterAltScreen :: Dialect -> ByteString
enterAltScreen d = d.altOn <> featuresOn d <> "\ESC[2J\ESC[H"

-- | The modes 'enterAltScreen' turns on besides the alternate screen.
featuresOn :: Dialect -> ByteString
featuresOn d = d.focusOn <> d.pasteOn

-- Also resets the cursor colour and style in case a pane's were mirrored
-- onto the outer terminal, and any mouse tracking the server asked for.
leaveAltScreen :: Dialect -> ByteString
leaveAltScreen d = d.mouseOff <> d.pasteOff <> d.focusOff <> d.altOff
    <> "\ESC[0m\ESC[?25h" <> d.cursorColourReset <> "\ESC[?12l" <> d.cursorStyleReset

-- | Where a lock command runs (tmux's @server_lock_client@): a cleared
-- alternate screen, its scrollback too, under the terminal's own cursor.
lockScreen :: ByteString
lockScreen = "\ESC[ q\ESC[?1049h\ESC[H\ESC[2J\ESC[3J"
