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

import Data.Bits (testBit, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Builder.Extra qualified as BB (smallChunkSize, toLazyByteStringWith, untrimmedStrategy)
import Data.ByteString.Builder.Prim qualified as P
import Data.ByteString.Builder.Prim.Internal qualified as P (boundedPrim)
import Data.ByteString.Internal qualified as BI
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.ForeignPtr (unsafeWithForeignPtr)

import Hat.Geometry
import Hat.Term.Caps
import Hat.Term.Cell
import Hat.TextWidth (charWidth, textWidth)
import Hat.Transport.Wire (DrawOp (..))

-- | What the client's terminal can be sent, as tmux's terminal model
-- ("Hat.Term.Caps") decides it: the choices the encoder makes per frame.
-- Cursor motion, clearing and SGR colour numbers assume a VT100-like
-- terminal, as tmux's do where it writes them directly.
data Dialect = Dialect
    { attrParams :: !(V.Vector ByteString)
        -- ^ by 'attrIndex': the SGR parameters turning those attributes on
    , attrRaws :: !(V.Vector ByteString)
        -- ^ by 'attrIndex': the bytes after the SGR for those attributes
        --   whose capability is no plain SGR
    , sgrBound :: !Int  -- ^ the most bytes 'writeSgr' writes
    , fgCodes :: !(V.Vector FgCode)  -- ^ by palette index: as a foreground
    , bgCodes :: !(V.Vector BgCode)  -- ^ by palette index: as a background
    , trueColour :: !Bool  -- ^ 24-bit colours are written as they are
    , clipCorner :: !Bool  -- ^ never write the bottom-right cell (no @am@)
    , sync :: !Bool        -- ^ bracket frames in DEC 2026 (@Sync@)
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

-- | A palette colour as a terminal's foreground: SGR parameters, and
-- whatever else showing it takes.
data FgCode
    = FgCode !ByteString
    | FgBold !ByteString  -- ^ shown bright by way of bold
    | FgUnlike !Int !ByteString !ByteString
        -- ^ the second parameters over a background fitted to this index,
        --   which would otherwise hide it

-- | A palette colour as a terminal's background: SGR parameters, and the
-- index it is fitted to for 'FgUnlike' (-1 for none).
data BgCode = BgCode !ByteString !Int

-- | How each attribute is turned on: an SGR parameter merged into the
-- run's one SGR, the capability's own bytes, or not at all.
data AttrCode = AttrSgr !Int | AttrRaw !ByteString | AttrNone

data AttrCodes = AttrCodes
    { bold, faint, italic, underline, blink, reverse, strike :: !AttrCode }

-- | The attributes in 'attrIndex' order.
attrList :: AttrCodes -> [AttrCode]
attrList a = [a.bold, a.faint, a.italic, a.underline, a.blink, a.reverse, a.strike]

-- | A style's attributes as a number below 128, one bit each.
attrIndex :: Style -> Int
attrIndex st =
    bit st.bold 1 + bit st.faint 2 + bit st.italic 4 + bit st.underline 8
    + bit st.blink 16 + bit st.reverse 32 + bit st.strike 64
  where
    bit b n = if b then n else 0

-- | A table entry's few bytes, without a whole default chunk behind each.
smallBytes :: BB.Builder -> ByteString
smallBytes = BL.toStrict . BB.toLazyByteStringWith (BB.untrimmedStrategy 32 BB.smallChunkSize) BL.empty

-- | Every attribute combination's and palette colour's SGR parameters,
-- rendered once per dialect so a run only looks its own up.
withCodes :: ColourDepth -> AttrCodes -> Dialect -> Dialect
withCodes depth a d = d
    { attrParams = params
    , attrRaws = raws
    -- ESC [ 0, the attributes, two colours of at most ;38;2;R;G;B, m,
    -- and the attributes' own bytes.
    , sgrBound = 3 + longest params + 2 * 17 + 1 + longest raws
    , fgCodes = V.generate 256 (fgFit depth . fromIntegral)
    , bgCodes = V.generate 256 (bgFit depth . fromIntegral)
    , trueColour = depth == DepthRGB
    }
  where
    params = V.generate 128 (render param)
    raws = V.generate 128 (render raw)
    longest = V.maximum . V.map B.length
    render f i = smallBytes $ mconcat
        [ f code | (k, code) <- zip [0 :: Int ..] (attrList a), testBit i k ]
    param = \case
        AttrSgr n -> BB.char8 ';' <> BB.intDec n
        _ -> mempty
    raw = \case
        AttrRaw bs -> BB.byteString bs
        _ -> mempty

dialectOf :: TtyTerm -> Dialect
dialectOf t = withCodes depth attrs Dialect
    { attrParams = V.empty
    , attrRaws = V.empty
    , sgrBound = 0
    , fgCodes = V.empty
    , bgCodes = V.empty
    , trueColour = False
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
    attrs = AttrCodes
        { bold = code "bold"
        , faint = code "dim"
        , italic = if hasCap "sitm" t then code "sitm" else code "smso"
        , underline = code "smul"
        , blink = code "blink"
        , reverse = code "rev"
        , strike = code "smxx"
        }
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
legacyDialect = withCodes DepthRGB standard Dialect
    { attrParams = V.empty
    , attrRaws = V.empty
    , sgrBound = 0
    , fgCodes = V.empty
    , bgCodes = V.empty
    , trueColour = False
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
  where
    standard = AttrCodes
        { bold = AttrSgr 1, faint = AttrSgr 2, italic = AttrSgr 3
        , underline = AttrSgr 4, blink = AttrSgr 5, reverse = AttrSgr 7
        , strike = AttrSgr 9 }

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
    <> go ClearAll ops
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
    go prev = \case
        [] -> mempty
        op : rest -> opBuilder d sz prev op <> go op rest

-- | Whether a run starts with a zero-width character right where the
-- previous run ended, so it must follow on with no cursor move: a
-- terminal joins such a character to the cell before it only when it
-- comes straight after (as the bidi isolate closing a run does; see
-- 'Hat.Server.Scene.isolatedRun').
continues :: DrawOp -> DrawOp -> Bool
continues (Put p _ t) (Put q _ u)
    | Just (c, _) <- T.uncons u, c >= '\x300', charWidth c == 0 =
        q.row == p.row && q.col == p.col + textWidth t
continues _ _ = False

opBuilder :: Dialect -> Size -> DrawOp -> DrawOp -> BB.Builder
opBuilder d sz prev = \case
    ClearAll -> BB.byteString "\ESC[0m\ESC[2J\ESC[H"
    op@(Put pos st txt) ->
        let !contiguous = continues prev op
        in if d.clipCorner && pos.row == fromIntegral sz.rows - 1
            -- tmux's TERM_NOAM: writing the bottom-right cell would scroll.
            then runHead d contiguous pos st
                <> TE.encodeUtf8Builder (fitColumns (fromIntegral sz.cols - 1 - pos.col) txt)
            else runHead d contiguous pos st <> TE.encodeUtf8Builder txt
    CursorAt pos _visible -> moveTo pos

-- | A run's cursor move, unless it follows straight on, and its SGR, in
-- one write.
runHead :: Dialect -> Bool -> Pos -> Style -> BB.Builder
runHead d contiguous pos st = P.primBounded (P.boundedPrim (moveBound + d.sgrBound) write) ()
  where
    write () p
        | contiguous = writeSgr d st p
        | otherwise = writeMove pos p >>= writeSgr d st

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
moveTo = P.primBounded (P.boundedPrim moveBound writeMove)

-- | ESC [, two numbers of at most five digits, ; and H.
moveBound :: Int
moveBound = 2 + 5 + 1 + 5 + 1

writeMove :: Pos -> Ptr Word8 -> IO (Ptr Word8)
writeMove pos p0 = do
    p1 <- byte 0x1b p0 >>= byte 0x5b >>= writeDec (pos.row + 1)
    byte 0x3b p1 >>= writeDec (pos.col + 1) >>= byte 0x48

byte :: Word8 -> Ptr Word8 -> IO (Ptr Word8)
byte w p = pokeByteOff p 0 w >> pure (p `plusPtr` 1)

-- | A number from 0 to 99999 in decimal.
writeDec :: Int -> Ptr Word8 -> IO (Ptr Word8)
writeDec n p = do
    let len
            | n < 10 = 1
            | n < 100 = 2
            | n < 1000 = 3
            | n < 10000 = 4
            | otherwise = 5
        go k v = do
            pokeByteOff p k (fromIntegral (0x30 + v `rem` 10) :: Word8)
            if k == 0 then pure () else go (k - 1) (v `quot` 10)
    go (len - 1) n
    pure (p `plusPtr` len)

-- | Full SGR for a style, starting from reset so runs are independent.
sgr :: Dialect -> Style -> ByteString
sgr d = BL.toStrict . BB.toLazyByteString . sgrBuilder d

sgrBuilder :: Dialect -> Style -> BB.Builder
sgrBuilder d st = P.primBounded (P.boundedPrim d.sgrBound (\() -> writeSgr d st)) ()

-- | A run's attributes by 'attrIndex', with bold standing in for a bright
-- foreground where the terminal needs it.
runAttrs :: Dialect -> Style -> Int
runAttrs d st = case st.fg of
    Indexed n -> fgAttrs n
    RGB r g b | not d.trueColour -> fgAttrs (findRgb r g b)
    _ -> attrIndex st
  where
    fgAttrs n = case V.unsafeIndex d.fgCodes (fromIntegral n) of
        FgBold _ -> attrIndex st .|. 1
        _ -> attrIndex st

-- | A run's SGR from the dialect's tables (see 'withCodes'), then any
-- attribute's own bytes: only a 24-bit colour on a terminal that takes it
-- is rendered per run.
writeSgr :: Dialect -> Style -> Ptr Word8 -> IO (Ptr Word8)
writeSgr d st p0 = do
    p1 <- byte 0x1b p0 >>= byte 0x5b >>= byte 0x30
        >>= bytes (V.unsafeIndex d.attrParams i)
    p2 <- case st.fg of
        DefaultColor -> pure p1
        Indexed n -> fgPalette n p1
        RGB r g b
            | d.trueColour -> writeRgb 38 r g b p1
            | otherwise -> fgPalette (findRgb r g b) p1
    p3 <- case st.bg of
        DefaultColor -> pure p2
        Indexed n -> bgPalette n p2
        RGB r g b
            | d.trueColour -> writeRgb 48 r g b p2
            | otherwise -> bgPalette (findRgb r g b) p2
    byte 0x6d p3 >>= bytes (V.unsafeIndex d.attrRaws i)
  where
    i = runAttrs d st
    fgPalette n = case V.unsafeIndex d.fgCodes (fromIntegral n) of
        FgCode bs -> bytes bs
        FgBold bs -> bytes bs
        FgUnlike k bs alt -> bytes (if k == under then alt else bs)
    bgPalette n = case V.unsafeIndex d.bgCodes (fromIntegral n) of
        BgCode bs _ -> bytes bs
    under = case st.bg of
        Indexed n -> bgIndex n
        RGB r g b | not d.trueColour -> bgIndex (findRgb r g b)
        _ -> -1
    bgIndex n = case V.unsafeIndex d.bgCodes (fromIntegral n) of
        BgCode _ k -> k

bytes :: ByteString -> Ptr Word8 -> IO (Ptr Word8)
bytes (BI.BS fp n) p
    | n == 0 = pure p  -- most runs' attributes: no call out to copy nothing
    | otherwise = do
        unsafeWithForeignPtr fp $ \src -> copyBytes p src n
        pure (p `plusPtr` n)

-- | ;L;2;R;G;B for layer L (38 or 48).
writeRgb :: Int -> Word8 -> Word8 -> Word8 -> Ptr Word8 -> IO (Ptr Word8)
writeRgb layer r g b p0 = do
    p1 <- byte 0x3b p0 >>= writeDec layer >>= byte 0x3b >>= byte 0x32
    p2 <- byte 0x3b p1 >>= writeDec (fromIntegral r)
    p3 <- byte 0x3b p2 >>= writeDec (fromIntegral g)
    byte 0x3b p3 >>= writeDec (fromIntegral b)

data Layer = Fg | Bg

-- | A palette colour as a foreground on a terminal of this depth (tmux's
-- @tty_check_fg@): on fewer than 256 colours, by way of the 16, bright
-- ones becoming bold on an 8-colour terminal.
fgFit :: ColourDepth -> Word8 -> FgCode
fgFit depth i = case depth of
    Depth n -> case to16 n (Indexed i) of
        Bright c | n < 16 -> FgBold (code c)
        -- Mapping to black-on-black or white-on-white is not much use, so
        -- the foreground changes.
        Converted c | n < 16, c == 0 || c == 7 ->
            FgUnlike (fromIntegral c) (code c) (code (7 - c))
        other -> FgCode (code (index (plain other)))
    _ -> FgCode (code i)
  where
    code = paletteCode Fg depth
    index = \case
        Indexed c -> c
        _ -> i

-- | A palette colour as a background on a terminal of this depth (tmux's
-- @tty_check_bg@): on fewer than 256 colours, by way of the 16, bright
-- ones becoming plain on an 8-colour terminal.
bgFit :: ColourDepth -> Word8 -> BgCode
bgFit depth i = case depth of
    Depth n ->
        let b = to16 n (Indexed i)
            shown = case b of
                Bright c | n < 16 -> c
                Converted c | n < 16 -> c
                other -> index (plain other)
        in BgCode (code shown) (fromIntegral (index (plain b)))
    _ -> BgCode (code i) (fromIntegral i)
  where
    code = paletteCode Bg depth
    index = \case
        Indexed c -> c
        _ -> i

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

paletteCode :: Layer -> ColourDepth -> Word8 -> ByteString
paletteCode layer depth n = smallBytes code
  where
    code
        | n < 8 = BB.char8 ';' <> BB.intDec (base + fromIntegral n)
        | n < 16, depth /= DepthRGB, depth /= Depth256 =
            BB.char8 ';' <> BB.intDec (base + 60 + fromIntegral n - 8)
        | otherwise = BB.byteString ext <> BB.intDec (fromIntegral n)
    (base, ext) = case layer of
        Fg -> (30, ";38;5;")
        Bg -> (40, ";48;5;")

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
