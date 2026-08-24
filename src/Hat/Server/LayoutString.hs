-- | tmux's @window_layout@ string codec: @<checksum>,WxH,x,y{…}@ with
-- @{…}@ for left-right splits and @[…]@ for top-bottom. tmux-resurrect
-- saves these ('emitLayout') and replays them via @select-layout@
-- ('parseLayout' + 'layoutFromString').
module Hat.Server.LayoutString
    ( emitLayout
    , layoutFromString
    , layoutSize
    , capturedArea
    , layoutChecksum
    ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.Char (ord)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Numeric (showHex)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

import Hat.Geometry (Rect (..), Size (..))
import Hat.Model.Ids (PaneId (..))
import Hat.Server.Layout (Layout (..), Orientation (..), childRects)

-- Emit -----------------------------------------------------------------

-- | Serialize a layout laid out in @rect@ to a tmux layout string,
-- checksum included.
emitLayout :: Rect -> Layout -> Text
emitLayout rect lay =
    let body = layoutBody rect lay
    in hex4 (layoutChecksum body) <> "," <> body

layoutBody :: Rect -> Layout -> Text
layoutBody r = \case
    Leaf (PaneId n) -> dims r <> "," <> tshow n
    Split LeftRight ratio a b ->
        let (ra, rb) = childRects r LeftRight ratio
        in dims r <> "{" <> layoutBody ra a <> "," <> layoutBody rb b <> "}"
    Split TopBottom ratio a b ->
        let (ra, rb) = childRects r TopBottom ratio
        in dims r <> "[" <> layoutBody ra a <> "," <> layoutBody rb b <> "]"
  where
    dims rr = tshow (rr.endCol - rr.startCol) <> "x" <> tshow (rr.endRow - rr.startRow)
        <> "," <> tshow rr.startCol <> "," <> tshow rr.startRow

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

-- Parsed geometry tree: each node carries its cell size along both axes;
-- ratios are recovered from child sizes when mapping onto a 'Layout'.
data LNode = LNode { w :: Int, h :: Int, kind :: LKind }

data LKind = KLeaf | KLR [LNode] | KTB [LNode]

type P = Parsec Void Text

-- | Parse a layout string and map its geometry onto the given panes (in
-- order). Extra panes are dropped; too few yields 'Nothing'. The saved
-- pane ids in the string are ignored — geometry is what restores.
layoutFromString :: Text -> [PaneId] -> Maybe Layout
layoutFromString str pids =
    case parse (layoutP <* eof) "<layout>" str of
        Left _ -> Nothing
        Right node -> case toLayout node pids of
            Just (lay, _) -> Just lay
            Nothing -> Nothing

-- | The window area a layout string was laid out in: its root node's
-- dimensions, clamped to sane emulator bounds against a corrupt string.
layoutSize :: Text -> Maybe Size
layoutSize str = case parse (layoutP <* eof) "<layout>" str of
    Left _ -> Nothing
    Right node -> Just Size { rows = clamp node.h, cols = clamp node.w }
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

layoutP :: P LNode
layoutP = do
    _ <- L.hexadecimal :: P Int  -- checksum (hex, ignored on restore)
    _ <- char ','
    nodeP

nodeP :: P LNode
nodeP = do
    wv <- L.decimal
    _ <- char 'x'
    hv <- L.decimal
    _ <- char ','
    _ <- L.decimal :: P Int  -- x
    _ <- char ','
    _ <- L.decimal :: P Int  -- y
    k <- choice
        [ char ',' *> (L.decimal :: P Int) *> pure KLeaf   -- leaf: pane id
        , KLR <$> between (char '{') (char '}') (nodeP `sepBy1` char ',')
        , KTB <$> between (char '[') (char ']') (nodeP `sepBy1` char ',')
        ]
    pure LNode { w = wv, h = hv, kind = k }

-- Consume pane ids in order, building a binary 'Layout' whose ratios come
-- from the parsed child sizes.
toLayout :: LNode -> [PaneId] -> Maybe (Layout, [PaneId])
toLayout node pids = case node.kind of
    KLeaf -> case pids of
        (p : rest) -> Just (Leaf p, rest)
        []         -> Nothing
    KLR kids -> binary LeftRight [ (k.w, k) | k <- kids ] pids
    KTB kids -> binary TopBottom [ (k.h, k) | k <- kids ] pids

binary :: Orientation -> [(Int, LNode)] -> [PaneId] -> Maybe (Layout, [PaneId])
binary _ [] _ = Nothing
binary _ [(_, k)] pids = toLayout k pids
binary o ((s, k) : rest) pids = do
    (a, pids') <- toLayout k pids
    (b, pids'') <- binary o rest pids'
    let restTotal = sum (map fst rest) + (length rest - 1)  -- sizes + borders
        ratio = toInteger s % toInteger (max 1 (s + restTotal))
    Just (Split o ratio a b, pids'')
