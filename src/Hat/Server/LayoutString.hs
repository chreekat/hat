-- | tmux's @window_layout@ string codec (@layout-custom.c@):
-- @<checksum>,WxH,x,y{…}@ with @{…}@ for left-right nodes, @[…]@ for
-- top-bottom ones and a trailing @,id@ on each pane. tmux-resurrect saves
-- these ('emitLayout') and replays them via @select-layout@
-- ('layoutFromString').
module Hat.Server.LayoutString
    ( emitLayout
    , layoutFromString
    , layoutSize
    , capturedArea
    , layoutChecksum
    ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.Char (isDigit, ord)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Numeric (showHex)

import Hat.Geometry (Size (..))
import Hat.Model.Ids (PaneId (..))
import Hat.Server.Layout
    ( Cell (..), Layout (..), Orientation (..), destroyBottomRight, fixOffsets
    , layoutPanes )

-- Emit -----------------------------------------------------------------

-- | Serialize a layout to a tmux layout string, checksum included
-- (@layout_dump@).
emitLayout :: Layout -> Text
emitLayout lay =
    let body = layoutBody lay
    in hex4 (layoutChecksum body) <> "," <> body

layoutBody :: Layout -> Text
layoutBody l = dims <> case l.cell of
    LeafCell (PaneId n) -> "," <> tshow n
    NodeCell LeftRight cs -> "{" <> T.intercalate "," (map layoutBody cs) <> "}"
    NodeCell TopBottom cs -> "[" <> T.intercalate "," (map layoutBody cs) <> "]"
  where
    dims = tshow l.sx <> "x" <> tshow l.sy <> "," <> tshow l.xoff <> "," <> tshow l.yoff

-- | tmux's 16-bit rolling checksum over the (checksum-less) body.
layoutChecksum :: Text -> Int
layoutChecksum = T.foldl' step 0
  where
    step csum c =
        let rolled = ((csum `shiftR` 1) + ((csum .&. 1) `shiftL` 15)) .&. 0xffff
        in (rolled + ord c) .&. 0xffff

hex4 :: Int -> Text
hex4 n = T.pack (replicate (4 - T.length h') '0') <> h'
  where h' = T.pack (showHex n "")

tshow :: Show a => a -> Text
tshow = T.pack . show

-- Parse ----------------------------------------------------------------

-- | Parse a layout string onto the given panes, in order (@layout_parse@):
-- a checksum that disagrees with the body, trailing garbage, more panes than
-- cells or a cell whose children do not add up to it fails with tmux's
-- message; surplus cells are closed bottom-right first. The pane ids in the
-- string are ignored — geometry is what is restored.
layoutFromString :: Text -> [PaneId] -> Either Text Layout
layoutFromString str pids = do
    body <- maybe (Left "invalid layout") Right (checkedBody str)
    (lay0, rest) <- maybe (Left "invalid layout") Right (construct body)
    if not (T.null rest) then Left "invalid layout" else pure ()
    lay1 <- trimCells (length pids) lay0
    let lay2 = correctRoot lay1
    if checkLayout lay2 then pure () else Left "size mismatch after applying layout"
    pure (fixOffsets (assign pids lay2))
  where
    trimCells npanes lay
        | npanes > ncells = Left ("have " <> tshow npanes <> " panes but need " <> tshow ncells)
        | npanes == ncells = Right lay
        | otherwise = case destroyBottomRight lay of
            Just lay' -> trimCells npanes lay'
            Nothing -> Left ("have " <> tshow npanes <> " panes but need " <> tshow ncells)
      where
        ncells = length (layoutPanes lay)

-- | The body after a well-formed @xxxx,@ checksum prefix that matches it.
checkedBody :: Text -> Maybe Text
checkedBody str = do
    let (hexPart, rest) = T.splitAt 4 str
    body <- T.stripPrefix "," rest
    (csum, leftover) <- either (const Nothing) Just (TR.hexadecimal hexPart)
    if T.null leftover && T.length hexPart == 4 && csum == layoutChecksum body
        then Just body else Nothing

-- | One cell and what follows it (@layout_construct@).
construct :: Text -> Maybe (Layout, Text)
construct t0 = do
    (w, t1) <- number t0
    t2 <- T.stripPrefix "x" t1
    (h, t3) <- number t2
    t4 <- T.stripPrefix "," t3
    (x, t5) <- number t4
    t6 <- T.stripPrefix "," t5
    (y, t7) <- number t6
    -- A pane's trailing @,id@, told apart from the next sibling's @,WxH@.
    let t8 = case T.stripPrefix "," t7 of
            Just after | (_, more) <- T.span isDigit after
                       , not ("x" `T.isPrefixOf` more) -> more
            _ -> t7
        base = Layout w h x y (LeafCell (PaneId 0))
    case T.uncons t8 of
        Just ('{', more) -> children LeftRight base '}' more
        Just ('[', more) -> children TopBottom base ']' more
        Just (c, _) | c `elem` (",}]>" :: String) -> Just (base, t8)
        Nothing -> Just (base, t8)
        _ -> Nothing
  where
    number t = case TR.decimal t of
        Right (n, rest) -> Just (n, rest)
        Left _ -> Nothing
    children o base close t = go [] t
      where
        go acc s = do
            (c, rest) <- construct s
            case T.uncons rest of
                Just (',', more) -> go (c : acc) more
                Just (ch, more) | ch == close ->
                    Just (base { cell = NodeCell o (reverse (c : acc)) }, more)
                _ -> Nothing

-- | Older tmux could write a root larger than its children; trust the
-- children (as @layout_parse@ does).
correctRoot :: Layout -> Layout
correctRoot l = case l.cell of
    LeafCell _ -> l
    NodeCell LeftRight cs ->
        let w = sum [ c.sx + 1 | c <- cs ] - 1
            h = maybe l.sy (.sy) (listToMaybe (reverse cs))
        in l { sx = w, sy = h }
    NodeCell TopBottom cs ->
        let h = sum [ c.sy + 1 | c <- cs ] - 1
            w = maybe l.sx (.sx) (listToMaybe (reverse cs))
        in l { sx = w, sy = h }

-- | Whether every node's children exactly fill it (@layout_check@).
checkLayout :: Layout -> Bool
checkLayout l = case l.cell of
    LeafCell _ -> True
    NodeCell LeftRight cs ->
        all (\c -> c.sy == l.sy && checkLayout c) cs
            && sum [ c.sx + 1 | c <- cs ] - 1 == l.sx
    NodeCell TopBottom cs ->
        all (\c -> c.sx == l.sx && checkLayout c) cs
            && sum [ c.sy + 1 | c <- cs ] - 1 == l.sy

-- | Fill the cells with panes in order (@layout_assign@).
assign :: [PaneId] -> Layout -> Layout
assign pids0 lay = fst (go pids0 lay)
  where
    go pids l = case l.cell of
        LeafCell _ -> case pids of
            p : rest -> (l { cell = LeafCell p }, rest)
            [] -> (l, [])
        NodeCell o cs ->
            let step (acc, ps) c = let (c', ps') = go ps c in (c' : acc, ps')
                (cs', left) = foldl step ([], pids) cs
            in (l { cell = NodeCell o (reverse cs') }, left)

-- | The window area a layout string was laid out in: its root cell's
-- dimensions, clamped to sane emulator bounds against a corrupt string.
layoutSize :: Text -> Maybe Size
layoutSize str = do
    body <- T.stripPrefix "," (T.drop 4 str)
    (lay, _) <- construct body
    pure Size { rows = clamp lay.sy, cols = clamp lay.sx }
  where
    clamp n = fromIntegral (max 1 (min 1000 n))

-- | The window area a captured tree was laid out in: the first parsable
-- window's 'layoutSize' (every window is captured at the same effective
-- size), defaulting to 24x80. Seeding a rebuilt session's size from this
-- keeps the pre-attach reconcile from reflowing every pane through a
-- fabricated geometry.
capturedArea :: [Text] -> Size
capturedArea lays = fromMaybe Size { rows = 24, cols = 80 }
    (listToMaybe (mapMaybe layoutSize lays))
