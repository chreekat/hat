-- | Drawing an expanded format onto a row of cells, mirroring tmux's
-- @format-draw.c@ and @style_parse@: @#[…]@ directives restyle the text,
-- place it in the left, centre, right or absolute-centre section, or build
-- a focus-trimmed list (@list=on@ \/ @list=focus@ with @<@ \/ @>@ markers).
-- This is how tmux lays out every status line and message.
module Hat.Server.FormatDraw
    ( -- * Styles
      DrawStyle (..)
    , Align (..)
    , ListMode (..)
    , DefaultType (..)
    , Width (..)
    , defaultDrawStyle
    , styleParse
    , styleCell
      -- * Drawing
    , formatDraw
      -- * Style-aware measuring
    , formatWidth
    , formatTrimLeft
    , formatTrimRight
    ) where

import Data.Foldable (toList)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Vector qualified as V

import Hat.Server.Format.Colour (Colour (..), colourFromText)
import Hat.TextWidth (charWidth)
import Hat.Term.Cell qualified as Cell

-- | tmux's @style_align@.
data Align = AlignDefault | AlignLeft | AlignCentre | AlignRight | AlignAbsCentre
    deriving (Eq, Ord, Show)

-- | tmux's @style_list@.
data ListMode = ListOff | ListOn | ListFocus | ListLeftMarker | ListRightMarker
    deriving (Eq, Show)

-- | tmux's @style_default_type@: how a directive moves the default style.
data DefaultType = DefaultBase | DefaultPush | DefaultPop | DefaultSet
    deriving (Eq, Show)

-- | A @width=@ directive: a cell count or a percentage of the area.
data Width = WidthCells Int | WidthPercent Int
    deriving (Eq, Show)

-- | tmux's @struct style@, less the parts hat's cells cannot carry
-- (hyperlinks, underline colour, @dim=@ and ranges, which only mouse
-- clicks on the status line read).
data DrawStyle = DrawStyle
    { cell :: Cell.Style
    , fill :: Maybe Cell.Color
    , align :: Align
    , list :: ListMode
    , defaultType :: DefaultType
    , ignore :: Bool
    , width :: Maybe Width
    }
    deriving (Eq, Show)

defaultDrawStyle :: DrawStyle
defaultDrawStyle = DrawStyle
    { cell = Cell.defaultStyle, fill = Nothing, align = AlignDefault
    , list = ListOff, defaultType = DefaultBase, ignore = False
    , width = Nothing }

-- | A style option's cell attributes (tmux's @style_apply@ over the default
-- cell); an unparseable style is the default.
styleCell :: Text -> Cell.Style
styleCell t = maybe Cell.defaultStyle (.cell)
    (styleParse Cell.defaultStyle defaultDrawStyle t)

-- | tmux's @style_parse@: fold a style string onto @sy@, where @default@ and
-- a @default@ colour mean @base@'s. Any bad token rejects the whole string.
styleParse :: Cell.Style -> DrawStyle -> Text -> Maybe DrawStyle
styleParse base sy0 =
    List.foldl' (\acc tok -> acc >>= token tok) (Just sy0)
        . filter (not . T.null) . T.split (`elem` (" ,\n" :: String))
  where
    token tok0 sy
        | T.length tok0 > 255 = Nothing
        | otherwise = case T.toLower tok0 of
            "default" -> Just sy { cell = base }
            "ignore" -> Just sy { ignore = True }
            "noignore" -> Just sy { ignore = False }
            "push-default" -> Just sy { defaultType = DefaultPush }
            "pop-default" -> Just sy { defaultType = DefaultPop }
            "set-default" -> Just sy { defaultType = DefaultSet }
            "nolist" -> Just sy { list = ListOff }
            "norange" -> Just sy
            "noalign" -> Just sy { align = AlignDefault }
            "none" -> Just sy { cell = clearAttrs sy.cell }
            tok
                | Just v <- T.stripPrefix "list=" tok -> case v of
                    "on" -> Just sy { list = ListOn }
                    "focus" -> Just sy { list = ListFocus }
                    "left-marker" -> Just sy { list = ListLeftMarker }
                    "right-marker" -> Just sy { list = ListRightMarker }
                    _ -> Nothing
                | Just v <- nonEmpty "range=" tok -> sy <$ rangeOk v
                | Just v <- nonEmpty "align=" tok -> case v of
                    "left" -> Just sy { align = AlignLeft }
                    "centre" -> Just sy { align = AlignCentre }
                    "right" -> Just sy { align = AlignRight }
                    "absolute-centre" -> Just sy { align = AlignAbsCentre }
                    _ -> Nothing
                | Just v <- nonEmpty "fill=" tok -> do
                    c <- colourOf v
                    Just sy { fill = c }
                | Just v <- nonEmpty "dim=" tok ->
                    sy <$ number 0 100 (fromMaybe v (T.stripSuffix "%" v))
                | Just v <- nonEmpty "fg=" tok -> do
                    c <- colourOf v
                    Just sy { cell = sy.cell { Cell.fg = fromMaybe base.fg c } }
                | Just v <- nonEmpty "bg=" tok -> do
                    c <- colourOf v
                    Just sy { cell = sy.cell { Cell.bg = fromMaybe base.bg c } }
                | Just v <- nonEmpty "us=" tok -> sy <$ colourOf v
                | T.length tok > 2, Just v <- T.stripPrefix "no" tok ->
                    if v == "link" || v == "attr"
                        then Just sy
                        else (\f -> sy { cell = f False sy.cell }) <$> attrs v
                | Just v <- nonEmpty "width=" tok -> case T.unsnoc v of
                    Just (n, '%') | not (T.null n) ->
                        (\w -> sy { width = Just (WidthPercent (fromInteger w)) }) <$> number 0 100 n
                    _ -> (\w -> sy { width = Just (WidthCells (fromInteger w)) })
                        <$> number 0 4294967295 v
                | Just v <- nonEmpty "pad=" tok -> sy <$ number 0 4294967295 v
                | "link=" `T.isPrefixOf` tok -> Just sy
                | otherwise -> (\f -> sy { cell = f True sy.cell }) <$> attrs tok
    nonEmpty pre tok = case T.stripPrefix pre tok of
        Just v | not (T.null v) -> Just v
        _ -> Nothing
    rangeOk v = case T.breakOn "|" v of
        (kind, rest)
            | T.null rest -> if kind `elem` ["control", "pane", "window", "session", "user"]
                then Nothing else Just ()
            | otherwise -> do
                let arg = T.drop 1 rest
                if T.null arg then Nothing else case kind of
                    "left" -> Nothing
                    "right" -> Nothing
                    "control" -> () <$ number 0 9 arg
                    "pane" -> prefixed '%' arg
                    "window" -> () <$ number 0 4294967295 arg
                    "session" -> prefixed '$' arg
                    _ -> Just ()
    prefixed c arg = case T.uncons arg of
        Just (c', n) | c' == c, not (T.null n) -> () <$ number 0 4294967295 n
        _ -> Nothing

-- | @strtonum@ over a plain decimal.
number :: Integer -> Integer -> Text -> Maybe Integer
number lo hi v = case TR.signed TR.decimal v of
    Right (n, "") | n >= lo, n <= hi -> Just n
    _ -> Nothing

-- | tmux's @colour_fromstring@ as a cell colour: 'Nothing' inside is
-- @default@ (the caller's base). Theme colours take their terminal colour,
-- tmux's @theme terminal@ choice.
colourOf :: Text -> Maybe (Maybe Cell.Color)
colourOf v = case List.lookup (T.toLower v) themeColours of
    Just n -> Just (Just (Cell.Indexed n))
    Nothing -> colourFromText v >>= \case
        CDefault -> Just Nothing
        CTerminal -> Just (Just Cell.DefaultColor)
        CBase n
            | n >= 90 -> Just (Just (Cell.Indexed (fromIntegral (n - 90 + 8))))
            | otherwise -> Just (Just (Cell.Indexed (fromIntegral n)))
        C256 n -> Just (Just (Cell.Indexed (fromIntegral n)))
        CRGB rgb -> Just (Just (Cell.RGB (byte 16) (byte 8) (byte 0)))
          where byte s = fromIntegral ((rgb `div` (2 ^ (s :: Int))) `mod` 256)
  where
    themeColours =
        [ ("themeblack", 0), ("themewhite", 7), ("themelightgrey", 7)
        , ("themedarkgrey", 0), ("themegreen", 2), ("themeyellow", 3)
        , ("themered", 1), ("themeblue", 4), ("themecyan", 6)
        , ("thememagenta", 5) ]

-- | tmux's @attributes_fromstring@ as a setter for the attributes hat's
-- cells carry; @hidden@, @overline@ and @acs@ are accepted but not drawn.
attrs :: Text -> Maybe (Bool -> Cell.Style -> Cell.Style)
attrs t
    | T.null t || edge (T.take 1 t) || edge (T.takeEnd 1 t) = Nothing
    | otherwise = do
        setters <- mapM one (filter (not . T.null) (T.split (`elem` delims) t))
        Just (\on s -> List.foldl' (\acc f -> f on acc) s setters)
  where
    delims = " ,|" :: String
    edge = T.any (`elem` delims)
    one = \case
        "acs" -> Just (\_ s -> s)
        "bright" -> Just (\on s -> s { Cell.bold = on })
        "bold" -> Just (\on s -> s { Cell.bold = on })
        "dim" -> Just (\on s -> s { Cell.faint = on })
        "underscore" -> Just underline
        "double-underscore" -> Just underline
        "curly-underscore" -> Just underline
        "dotted-underscore" -> Just underline
        "dashed-underscore" -> Just underline
        "blink" -> Just (\on s -> s { Cell.blink = on })
        "reverse" -> Just (\on s -> s { Cell.reverse = on })
        "hidden" -> Just (\_ s -> s)
        "italics" -> Just (\on s -> s { Cell.italic = on })
        "strikethrough" -> Just (\on s -> s { Cell.strike = on })
        "overline" -> Just (\_ s -> s)
        _ -> Nothing
    underline on s = s { Cell.underline = on }

clearAttrs :: Cell.Style -> Cell.Style
clearAttrs s = s
    { Cell.bold = False, Cell.underline = False, Cell.italic = False
    , Cell.reverse = False, Cell.strike = False, Cell.blink = False
    , Cell.faint = False }

-- Drawing ----------------------------------------------------------------

data Section
    = SLeft | SCentre | SRight | SAbsCentre | SList | SListLeft | SListRight
    | SAfter
    deriving (Eq, Ord, Show)

-- | The walk's state (the locals of tmux's @format_draw@).
data Walk = Walk
    { sections :: Map Section (Seq Cell.Cell)
    , current :: Section
    , sy :: DrawStyle
    , currentDefault :: Cell.Style
    , baseDefault :: Cell.Style
    , focusStart :: Maybe Int
    , focusEnd :: Maybe Int
    , listState :: ListState
    , listAlign :: Align
    , fillColour :: Maybe Cell.Color
    , alignMap :: Map Align Section
    }

data ListState = NoList | InList | AfterList
    deriving (Eq)

-- | tmux's @format_draw@: draw an expanded format over a row (the area,
-- already holding whatever lies beneath), in @base@ style.
formatDraw :: Cell.Style -> Text -> V.Vector Cell.Cell -> V.Vector Cell.Cell
formatDraw base expanded area = case walk start (T.unpack expanded) of
    Nothing -> area  -- an unterminated #[ draws nothing
    Just w -> arrange w (filled w)
  where
    available = V.length area
    start = Walk
        { sections = Map.empty, current = SLeft
        , sy = defaultDrawStyle { cell = base }
        , currentDefault = base, baseDefault = base
        , focusStart = Nothing, focusEnd = Nothing
        , listState = NoList, listAlign = AlignDefault
        , fillColour = Nothing
        , alignMap = Map.fromList
            [ (AlignDefault, SLeft), (AlignLeft, SLeft), (AlignCentre, SCentre)
            , (AlignRight, SRight), (AlignAbsCentre, SAbsCentre) ] }
    filled w = case w.fillColour of
        Just c -> V.replicate available
            (Cell.glyphCell ' ' Cell.defaultStyle { Cell.bg = c })
        Nothing -> area
    arrange w row0 =
        let sec s = Map.findWithDefault Seq.empty s w.sections
        in layout available (sec SLeft) (sec SCentre) (sec SRight)
            (sec SAbsCentre) (sec SList) (sec SListLeft) (sec SListRight)
            (sec SAfter) w.listAlign w.focusStart w.focusEnd row0

-- | Walk the expanded text into sections; 'Nothing' for an unterminated
-- style.
walk :: Walk -> String -> Maybe Walk
walk w = \case
    [] -> Just w
    s@('#' : c1 : _) | c1 /= '[' ->
        let (hashes, rest) = span (== '#') s
            n = length hashes
            even' = even n
        in case rest of
            ('[' : rest')
                | even' ->
                    if w.sy.ignore then walk w rest'
                    else walk (putChars (replicate (n `div` 2) '#' <> "[") w) rest'
                | otherwise ->
                    let w' = if w.sy.ignore then w
                             else putChars (replicate (n `div` 2) '#') w
                    in walk w' ('#' : '[' : rest')
            _ -> walk (putChars (replicate ((n + 1) `div` 2) '#') w) rest
    ('#' : '[' : rest) | not w.sy.ignore -> case skipStyle rest of
        Nothing -> Nothing
        Just (body, rest') -> walk (directive (T.pack body) w) rest'
    (c : rest) -> walk (putChars [c] w) rest

-- | tmux's @format_skip@ for @]@: the style body, honouring nested @#[@
-- and #-escapes, and the text after the @]@.
skipStyle :: String -> Maybe (String, String)
skipStyle = go (0 :: Int) []
  where
    go _ _ [] = Nothing
    go depth acc ('#' : c : rest)
        | c `elem` (",#{}:" :: String) =
            go (if c == '{' then depth + 1 else depth) (c : '#' : acc) rest
    go depth acc (c : rest)
        | c == ']' && depth == 0 = Just (reverse acc, rest)
        | c == '}' = go (depth - 1) (c : acc) rest
        | otherwise = go depth (c : acc) rest

putChars :: String -> Walk -> Walk
putChars cs w = w { sections = Map.alter (Just . add . fromMaybe Seq.empty)
                                    w.current w.sections }
  where
    sty = w.sy.cell
    add sq = List.foldl' put sq cs
    put sq c = case charWidth c of
        0 | c < ' ' || c == '\DEL' -> sq
          | otherwise -> attach c sq
        2 -> sq |> Cell.Cell (Cell.Glyph c [] Cell.Wide) sty
                |> Cell.Cell Cell.Continuation sty
        _ -> sq |> Cell.glyphCell c sty
    isGlyph cl = Cell.cellWidth cl > 0
    attach c sq = case lastGlyph sq of
        Just i -> Seq.adjust' (addMark c) i sq
        Nothing -> sq
    lastGlyph sq = Seq.findIndexR isGlyph sq
    addMark c cl = case cl.content of
        Cell.Glyph b ms wd -> cl { Cell.content = Cell.Glyph b (ms <> [c]) wd }
        Cell.Continuation -> cl

-- | Apply one @#[…]@ directive (the style half of @format_draw@'s loop).
directive :: Text -> Walk -> Walk
directive body w = case styleParse w.currentDefault w.sy body of
    Nothing -> w
    Just parsed ->
        let saved = w.sy.cell
            w1 = w { fillColour = maybe w.fillColour Just parsed.fill
                   , sy = parsed { defaultType = DefaultBase } }
            w2 = case parsed.defaultType of
                DefaultPush -> w1 { currentDefault = saved }
                DefaultPop -> w1 { currentDefault = w1.baseDefault }
                DefaultSet -> w1 { baseDefault = saved, currentDefault = saved }
                DefaultBase -> w1
        in listStep parsed w2

listStep :: DrawStyle -> Walk -> Walk
listStep parsed w = case parsed.list of
    ListOn ->
        let w1 | w.listState /= InList =
                    w { listState = InList, listAlign = parsed.align }
               | otherwise = w
            w2 = endFocus w1
        in w2 { current = SList }
    ListFocus
        | w.listState /= InList -> w
        | Nothing <- w.focusStart -> w { focusStart = Just (listWidth w) }
        | otherwise -> w
    ListOff ->
        let w1 | w.listState == InList =
                    let w' = endFocus w
                        m = Map.insert w.listAlign SAfter w'.alignMap
                        m' | w.listAlign == AlignLeft = Map.insert AlignDefault SAfter m
                           | otherwise = m
                    in w' { alignMap = m', listState = AfterList }
               | otherwise = w
        in w1 { current = Map.findWithDefault SLeft parsed.align w1.alignMap }
    ListLeftMarker -> marker SListLeft
    ListRightMarker -> marker SListRight
  where
    marker s
        | w.listState /= InList = w
        | not (Seq.null (Map.findWithDefault Seq.empty s w.sections)) = w
        | otherwise =
            let w' = case (w.focusStart, w.focusEnd) of
                    (Just _, Nothing) -> w { focusStart = Nothing }
                    _ -> w
            in w' { current = s }
    endFocus x = case (x.focusStart, x.focusEnd) of
        (Just _, Nothing) -> x { focusEnd = Just (listWidth x) }
        _ -> x
    listWidth x = Seq.length (Map.findWithDefault Seq.empty SList x.sections)

-- | Place the sections (tmux's @format_draw_none@ \/ @_left@ \/ @_centre@ \/
-- @_right@ \/ @_absolute_centre@, picked by where the list sits).
layout
    :: Int -> Seq Cell.Cell -> Seq Cell.Cell -> Seq Cell.Cell -> Seq Cell.Cell
    -> Seq Cell.Cell -> Seq Cell.Cell -> Seq Cell.Cell -> Seq Cell.Cell
    -> Align -> Maybe Int -> Maybe Int -> V.Vector Cell.Cell
    -> V.Vector Cell.Cell
layout available left centre right absC list lMark rMark after listAlign fs fe =
    case listAlign of
        AlignDefault -> drawNone left centre right
        AlignLeft -> drawLeft
        AlignCentre -> drawCentre
        AlignRight -> drawRight
        AlignAbsCentre -> drawAbs
  where
    len = Seq.length
    focus dflt = case (fs, fe) of
        (Just a, Just b) -> (a, b)
        _ -> (dflt, dflt)

    drawNone l c r row =
        let (wl, wc, wr) = trim3 (len l) (len c) (len r)
            wa = min available (len absC)
        in put ((available - wa) `div` 2) 0 wa absC
            $ put (wl + ((available - wr) - wl) `div` 2 - wc `div` 2)
                (len c `div` 2 - wc `div` 2) wc c
            $ put (available - wr) (len r - wr) wr r
            $ put 0 0 wl l row
    trim3 wl0 wc0 wr0 = go wl0 wc0 wr0
      where
        go wl wc wr
            | wl + wc + wr <= available = (wl, wc, wr)
            | wc > 0 = go wl (wc - 1) wr
            | wr > 0 = go wl wc (wr - 1)
            | otherwise = go (wl - 1) wc wr

    -- Trim in the given priority order until the five widths fit.
    trim5 order ws0 = go ws0
      where
        go ws
            | sum (Map.elems ws) <= available = ws
            | otherwise = case [ k | k <- order, Map.findWithDefault 0 k ws > 0 ] of
                (k : _) -> go (Map.adjust (subtract 1) k ws)
                [] -> ws
    widths = Map.fromList
        [ (SLeft, len left), (SCentre, len centre), (SRight, len right)
        , (SList, len list), (SAfter, len after) ]
    widths5 ws =
        let wOf k = Map.findWithDefault 0 k ws
        in (wOf SLeft, wOf SCentre, wOf SRight, wOf SList, wOf SAfter)

    drawLeft row =
        let ws = trim5 [SCentre, SList, SRight, SAfter, SLeft] widths
            (wl, wc, wr, wli, wa) = widths5 ws
            (f0, f1) = focus 0
            wab = min available (len absC)
        in if wli == 0
            then drawNone (left <> Seq.take wa after) centre right row
            else put ((available - wab) `div` 2) 0 wab absC
                $ putList wl wli f0 f1
                $ put ((wl + wli + wa) + ((available - wr) - (wl + wli + wa)) `div` 2
                        - wc `div` 2) (len centre `div` 2 - wc `div` 2) wc centre
                $ put (wl + wli) 0 wa after
                $ put (available - wr) (len right - wr) wr right
                $ put 0 0 wl left row

    drawCentre row =
        let ws = trim5 [SList, SAfter, SCentre, SRight, SLeft] widths
            (wl, wc, wr, wli, wa) = widths5 ws
            (f0, f1) = focus (len list `div` 2)
            middle = wl + ((available - wr) - wl) `div` 2
            wab = min available (len absC)
        in if wli == 0
            then drawNone left (centre <> Seq.take wa after) right row
            else put ((available - wab) `div` 2) 0 wab absC
                $ putList (middle - wli `div` 2) wli f0 f1
                $ put (middle - wli `div` 2 + wli) 0 wa after
                $ put (middle - wli `div` 2 - wc) 0 wc centre
                $ put (available - wr) (len right - wr) wr right
                $ put 0 0 wl left row

    drawRight row =
        let ws = trim5 [SCentre, SList, SRight, SAfter, SLeft] widths
            (wl, wc, wr, wli, wa) = widths5 ws
            (f0, f1) = focus 0
            wab = min available (len absC)
        in if wli == 0
            then drawNone left centre (right <> Seq.take wa after) row
            else put ((available - wab) `div` 2) 0 wab absC
                $ putList (available - wli - wa) wli f0 f1
                $ put (wl + ((available - wr - wli - wa) - wl) `div` 2 - wc `div` 2)
                    (len centre `div` 2 - wc `div` 2) wc centre
                $ put (available - wr - wli - wa) 0 wr right
                $ put (available - wa) (len after - wa) wa after
                $ put 0 0 wl left row

    drawAbs row =
        let (wl, wc, wr) = trim3 (len left) (len centre) (len right)
            (wli, wa, wab) = trimAbs (len list) (len after) (len absC)
            middle = wl + ((available - wr) - wl) `div` 2
            (f0, f1) = focus (len list `div` 2)
            off0 = (available - wli - wab) `div` 2
        in put (off0 + wab + wli) 0 wa after
            $ putList (off0 + wab) wli f0 f1
            $ put off0 0 wab absC
            $ put (middle - wc) 0 wc centre
            $ put (available - wr) (len right - wr) wr right
            $ put 0 0 wl left row
    trimAbs wli wa wab
        | wli + wa + wab <= available = (wli, wa, wab)
        | wli > 0 = trimAbs (wli - 1) wa wab
        | wa > 0 = trimAbs wli (wa - 1) wab
        | otherwise = trimAbs wli wa (wab - 1)

    -- tmux's format_draw_put_list: draw the list whole, or trimmed to keep
    -- the focus visible with markers at a cut side.
    putList offset width f0 f1 row
        | width >= len list = put offset 0 width list row
        | otherwise =
            let centreF = f0 + (f1 - f0) `div` 2
                s0 | centreF < width `div` 2 = 0
                   | otherwise = centreF - width `div` 2
                s1 = if s0 + width > len list then len list - width else s0
                (row1, offset1, s2, width1)
                    | s1 /= 0 && width > len lMark =
                        ( put offset 0 (len lMark) lMark row
                        , offset + len lMark, s1 + len lMark
                        , width - len lMark )
                    | otherwise = (row, offset, s1, width)
                (row2, width2)
                    | s2 + width1 < len list && width1 > len rMark =
                        ( put (offset1 + width1 - len rMark) 0 (len rMark) rMark row1
                        , width1 - len rMark )
                    | otherwise = (row1, width1)
            in put offset1 s2 width2 list row2

    -- Copy @width@ cells of a section from @from@ to column @offset@,
    -- clipped to the row; a wide glyph cut in half becomes a blank.
    put offset from width sec row
        | width <= 0 = row
        | otherwise =
            let cells = toList (Seq.take width (Seq.drop from sec))
                n = length cells
                fixed = zipWith (fixWide n) [0 ..] cells
                updates = [ (offset + i, c) | (i, c) <- zip [0 ..] fixed
                          , offset + i >= 0, offset + i < V.length row ]
            in row V.// updates
    fixWide n i c = case c.content of
        Cell.Continuation | i == 0 -> Cell.glyphCell ' ' c.style
        Cell.Glyph _ _ Cell.Wide | i == n - 1 -> Cell.glyphCell ' ' c.style
        _ -> c

-- Style-aware measuring ---------------------------------------------------

-- | A run of hashes before a character: their count, the width they draw,
-- and whether they open a style (tmux's @format_leading_hashes@).
leadingHashes :: String -> (Int, Int, Bool, String)
leadingHashes s =
    let (hs, rest) = span (== '#') s
        n = length hs
    in case rest of
        ('[' : _)
            | odd n -> (n, n `div` 2, True, '#' : rest)
            | otherwise -> (n, n `div` 2, False, rest)
        _ -> (n, (n + 1) `div` 2, False, rest)

-- | tmux's @format_width@: display width ignoring @#[…]@ styles.
formatWidth :: Text -> Int
formatWidth = go 0 . T.unpack
  where
    go !acc = \case
        [] -> acc
        s@('#' : _) ->
            let (_, w, isStyle, rest) = leadingHashes s
            in if isStyle
                then case skipStyle (drop 2 rest) of
                    Nothing -> 0
                    Just (_, rest') -> go (acc + w) rest'
                else go (acc + w) rest
        (c : rest) -> go (acc + charWidth c) rest

-- | tmux's @format_trim_left@: keep the first @limit@ columns, copying
-- every style.
formatTrimLeft :: Int -> Text -> Text
formatTrimLeft limit = T.pack . go 0 . T.unpack
  where
    go w s
        | w >= limit = []
        | otherwise = case s of
            [] -> []
            ('#' : _) ->
                let (n, lw0, isStyle, rest) = leadingHashes s
                    lw = min lw0 (limit - w)
                    hashes
                        | lw == 0 = []
                        | n == 1 = "#"
                        | otherwise = replicate (2 * lw) '#'
                in hashes <> styleThen (w + lw) isStyle rest
            (c : rest) ->
                let cw = charWidth c
                in (if w + cw <= limit && cw > 0 || cw == 0 && c >= ' ' && c /= '\DEL'
                        then [c] else []) <> go (w + cw) rest
    styleThen w isStyle rest
        | isStyle = case skipStyle (drop 2 rest) of
            Nothing -> []
            Just (body, rest') -> "#[" <> body <> "]" <> go w rest'
        | otherwise = go w rest

-- | tmux's @format_trim_right@: keep the last @limit@ columns, copying
-- every style.
formatTrimRight :: Int -> Text -> Text
formatTrimRight limit t
    | total <= limit = t
    | otherwise = T.pack (go 0 (T.unpack t))
  where
    total = formatWidth t
    skip = total - limit
    go w = \case
        [] -> []
        s@('#' : _) ->
            let (n, lw, isStyle, rest) = leadingHashes s
                copyW
                    | w <= skip = if skip - w >= lw then 0 else lw - (skip - w)
                    | otherwise = lw
                hashes
                    | copyW == 0 = []
                    | n == 1 = "#"
                    | otherwise = replicate (2 * copyW) '#'
            in hashes <> styleThen (w + lw) isStyle rest
        (c : rest) ->
            let cw = charWidth c
            in (if w >= skip && (cw > 0 || c >= ' ' && c /= '\DEL') then [c] else [])
                <> go (w + cw) rest
    styleThen w isStyle rest
        | isStyle = case skipStyle (drop 2 rest) of
            Nothing -> []
            Just (body, rest') -> "#[" <> body <> "]" <> go w rest'
        | otherwise = go w rest

