-- | @capture-pane -R@: tmux's raw grid dump (@cmd_capture_pane_grid@), one
-- header, one line record per grid line and one cell record per column, in
-- tmux's exact byte format over the grid libghostty-vt records.
--
-- tmux prints its own grid's storage here, and libghostty-vt stores a grid
-- differently. Where the two models line up the dump is exact; where tmux
-- records something libghostty-vt does not, the field is derived from the
-- cell as it stands now:
--
--   * cellused\/cellsize are tmux allocation extents; both print the used
--     extent (through the last non-default cell).
--   * CLEARED marks a cell tmux erased. libghostty-vt keeps an erased cell
--     distinct only when the erase carried a background, so only those are
--     CLEARED; an erase under the default background reads as never written.
--   * EXTENDED on a line is sticky in tmux (once a line needed an extended
--     cell it keeps the flag); here it reflects the cells the line holds now.
--   * A palette color below 16 prints as tmux's SGR 30–37\/90–97 form:
--     libghostty-vt does not keep whether it came from @38;5;n@.
--   * linkid is the OSC 8 @id=@, which libghostty-vt does not expose; it
--     prints NONE.
--   * Tabs, charset (ACS) attributes and OSC 133 line metadata are not
--     recorded by libghostty-vt and never appear.
module Hat.Server.Command.CaptureRaw
    ( rawDump
    , lineFlags
    , flagLetters
    , cellUsed
    ) where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.List qualified as List
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.ByteString qualified as B
import Numeric (showHex, showOct)

import Hat.Model (tshow)
import Hat.Term.RawGrid

rawDump :: RawGrid -> Text
rawDump g = T.concat $
    ("G " <> tshow g.cols <> "x" <> tshow g.rows
        <> " (" <> tshow g.hsize <> "/" <> tshow g.hlimit <> ")\n")
    : concat (zipWith dumpLine [0 ..] g.lines)
  where
    dumpLine :: Int -> RawLine -> [Text]
    dumpLine yy l =
        let pos | yy < g.hsize = "-"
                | otherwise    = tshow (yy - g.hsize)
            fl = lineFlags l
            used = cellUsed l
        in ("\tL " <> tshow yy <> " (" <> pos <> ") flags="
                <> flagNames lineFlagNames fl <> "[" <> hex fl <> "] "
                <> tshow used <> "/" <> tshow used <> "\n")
           : zipWith (dumpCell yy) [0 :: Int ..] l.cells
    dumpCell yy xx c =
        let tc = tmuxCell c
        in "\t\tC " <> tshow yy <> "," <> tshow xx
            <> " data=(" <> tshow tc.dataWidth <> "," <> tshow tc.dataSize
            <> "," <> tc.dataText <> ") flags="
            <> flagNames cellFlagNames tc.flags <> "[" <> hex tc.flags
            <> "] attr=" <> flagNames attrNames tc.attr <> "[" <> hex tc.attr
            <> "] fg=" <> colour tc.fgc <> " bg=" <> colour tc.bgc
            <> " us=" <> colour tc.usc
            <> " link=" <> fromMaybe "NONE" c.link <> " linkid=NONE\n"
    colour v = colourName v <> "[" <> hex v <> "]"

-- | tmux's view of one cell: its @grid_cell@ fields as @grid_get_cell@
-- returns them.
data TmuxCell = TmuxCell
    { dataWidth, dataSize :: Int
    , dataText :: Text
    , flags, attr, fgc, bgc, usc :: Int
    }

tmuxCell :: RawCell -> TmuxCell
tmuxCell c = case c.width of
    -- tmux's padding cell, always all-default whatever sits under it.
    SpacerTail -> TmuxCell 1 1 "!" flagPadding 0 8 8 8
    _ -> TmuxCell
        { dataWidth = if c.width == RawWide then 2 else 1
        , dataSize = B.length (TE.encodeUtf8 (T.pack str))
        , dataText = T.concat (map vis str)
        , flags = (if c.bgOnly then flagCleared else 0)
              .|. (if is256 fgv then flagFg256 else 0)
              .|. (if is256 bgv then flagBg256 else 0)
        , attr = attrBits c.attrs
        , fgc = fgv, bgc = bgv, usc = usv
        }
  where
    str | null c.text || c.width == SpacerHead = " "
        | otherwise = c.text
    fgv = colourValue c.fg
    bgv = colourValue c.bg
    -- SGR 58 has only extended forms, so tmux flags every palette entry 256.
    usv = case c.us of
        RawPalette n -> flag256 .|. fromIntegral n
        other -> colourValue other
    is256 v = v .&. flag256 /= 0

-- | The line's tmux flags: WRAPPED, EXTENDED when any cell needs tmux's
-- extended storage, HYPERLINK when any cell carries a link.
lineFlags :: RawLine -> Int
lineFlags l =
        (if l.wrapped then 0x1 else 0)
    .|. (if any extended l.cells then 0x2 else 0)
    .|. (if any (isJust . (.link)) l.cells then 0x100 else 0)
  where
    -- tmux's grid_need_extended_cell, plus padding, which is always extended.
    extended c = c.width == RawWide || c.width == SpacerTail
        || B.length (TE.encodeUtf8 (T.pack c.text)) > 1
        || attrBits c.attrs > 0xff
        || isRgb c.fg || isRgb c.bg || c.us /= RawDefault
        || isJust c.link
    isRgb = \case RawRgb {} -> True; _ -> False

-- | The line-flag letters @capture-pane -F@ prints, in tmux's order, or @-@
-- for none. tmux's D (a dead pane's line) and O (OSC 133 output start) are
-- never set: libghostty-vt records neither.
flagLetters :: RawLine -> Text
flagLetters l = case [ ch | (True, ch) <- letters ] of
    [] -> "-"
    cs -> T.pack cs
  where
    fl = lineFlags l
    letters =
        [ (fl .&. 0x100 /= 0, 'H'), (l.prompt, 'P')
        , (fl .&. 0x1 /= 0, 'W'), (fl .&. 0x2 /= 0, 'X') ]

-- | Columns through the last cell that is not a default blank.
cellUsed :: RawLine -> Int
cellUsed l = length (List.dropWhileEnd defaultBlank l.cells)
  where
    defaultBlank = (== blankRawCell)

-- | tmux's colour encoding: 0–7 and 90–97 for the SGR base forms, 8 for
-- default, a 256 or RGB flag over the rest.
colourValue :: RawColor -> Int
colourValue = \case
    RawDefault -> 8
    RawPalette n
        | n < 8     -> fromIntegral n
        | n < 16    -> 90 + fromIntegral n - 8
        | otherwise -> flag256 .|. fromIntegral n
    RawRgb r g b -> flagRgb .|. (fromIntegral r `shiftL` 16)
        .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

-- | tmux's colour_tostring.
colourName :: Int -> Text
colourName v
    | v .&. flagRgb /= 0 = "#" <> T.justifyRight 6 '0' (hex (v .&. 0xffffff))
    | v .&. flag256 /= 0 = "colour" <> tshow (v .&. 0xff)
    | otherwise = fromMaybe "invalid" (lookup v names)
  where
    names = zip [0 ..] base <> [(8, "default"), (9, "terminal")]
         <> zip [90 ..] (map ("bright" <>) base)
    base = ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"]

attrBits :: RawAttrs -> Int
attrBits a = foldr (.|.) 0
    [ bit a.bold 0x1, bit a.faint 0x2, bit a.blink 0x8, bit a.inverse 0x10
    , bit a.invisible 0x20, bit a.italic 0x40, bit a.strike 0x100
    , bit a.overline 0x2000
    , case a.underline of
        NoUnderline -> 0
        Single -> 0x4
        Double -> 0x200
        Curly  -> 0x400
        Dotted -> 0x800
        Dashed -> 0x1000
    ]
  where
    bit b v = if b then v else 0

flagFg256, flagBg256, flagPadding, flagCleared, flag256, flagRgb :: Int
flagFg256 = 0x1
flagBg256 = 0x2
flagPadding = 0x4
flagCleared = 0x40
flag256 = 0x01000000
flagRgb = 0x02000000

-- Each table in tmux's print order (grid_*_string).
lineFlagNames, cellFlagNames, attrNames :: [(Int, Text)]
lineFlagNames =
    [ (0x1, "WRAPPED"), (0x2, "EXTENDED"), (0x4, "DEAD"), (0x8, "START_PROMPT")
    , (0x10, "SECOND_PROMPT"), (0x20, "START_COMMAND"), (0x40, "START_OUTPUT")
    , (0x80, "END_OUTPUT"), (0x100, "HYPERLINK") ]
cellFlagNames =
    [ (0x1, "FG256"), (0x2, "BG256"), (0x4, "PADDING"), (0x8, "EXTENDED")
    , (0x10, "SELECTED"), (0x40, "CLEARED"), (0x80, "TAB"), (0x20, "NOPALETTE") ]
attrNames =
    [ (0x80, "CHARSET"), (0x1, "BRIGHT"), (0x2, "DIM"), (0x4, "UNDERSCORE")
    , (0x8, "BLINK"), (0x10, "REVERSE"), (0x20, "HIDDEN"), (0x40, "ITALICS")
    , (0x100, "STRIKETHROUGH"), (0x200, "UNDERSCORE_2"), (0x400, "UNDERSCORE_3")
    , (0x800, "UNDERSCORE_4"), (0x1000, "UNDERSCORE_5"), (0x2000, "OVERLINE") ]

flagNames :: [(Int, Text)] -> Int -> Text
flagNames table v = case [ n | (b, n) <- table, v .&. b /= 0 ] of
    [] -> "NONE"
    ns -> T.intercalate "," ns

hex :: Int -> Text
hex v = T.pack (showHex v "")

-- | utf8_stravis(VIS_OCTAL|VIS_CSTYLE|VIS_TAB|VIS_NL) over one character.
vis :: Char -> Text
vis ch
    | ch == '\\' = "\\\\"
    | Just e <- lookup ch cstyle = e
    | ch < ' ' || ch == '\DEL' =
        "\\" <> T.justifyRight 3 '0' (T.pack (showOct (fromEnum ch) ""))
    | otherwise = T.singleton ch
  where
    cstyle = [ ('\n', "\\n"), ('\t', "\\t"), ('\r', "\\r"), ('\b', "\\b")
             , ('\a', "\\a"), ('\v', "\\v"), ('\f', "\\f"), ('\0', "\\000") ]
