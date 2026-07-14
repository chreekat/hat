-- | The chooser overlay's pure logic: flattening the session/window/pane
-- tree into visible rows (honoring each node's expansion and the search
-- query), mapping one key at a time onto navigation/expansion/search
-- edits, and laying out the visible rows. The server owns the
-- 'PickerState' (per client), feeds keys here, and runs the chosen node's
-- command.
module Hat.Server.Picker
    ( PickerEdit (..)
    , Row (..)
    , leaf
    , windowChildren
    , visibleRows
    , selectedPreview
    , pickerSplit
    , stackThumbnails
    , pickerRegion
    , editPicker
    , pickerLines
    ) where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Hat.Geometry (Rect (..), Size (..))
import Hat.Model (PickerNode (..), PickerState (..), PreviewTarget)
import Hat.Server.Keys (Key (..))

-- | What one key does to an open picker.
data PickerEdit
    = PickerStay PickerState  -- ^ keep it open with this new state
    | PickerRun Text          -- ^ run this command line and close
    | PickerCancel            -- ^ close, running nothing
    deriving (Eq, Show)

-- | A flattened, on-screen row: the node, its depth in the tree, and the
-- index path locating it in 'roots' (so a key can edit that exact node).
data Row = Row
    { depth :: !Int
    , node  :: !PickerNode
    , path  :: ![Int]
    }
    deriving (Eq, Show)

-- | A childless node: used for @choose-window@ and for building leaves.
leaf :: Text -> Text -> PickerNode
leaf lbl cmd = PickerNode
    { label = lbl, command = cmd, preview = Nothing
    , children = [], expanded = False }

-- | A window's pane rows for the tree. A window with a single pane
-- collapses to just its window row (that lone pane is redundant), so it
-- gets no pane children; a window with two or more keeps them all.
windowChildren :: [PickerNode] -> [PickerNode]
windowChildren [_] = []
windowChildren panes = panes

-- | The rows to display: the tree filtered by the current query, then
-- flattened depth-first, descending into a node only when it is expanded.
visibleRows :: PickerState -> [Row]
visibleRows p = go 0 [] (filterTree p.query p.roots)
  where
    go d prefix nodes = concat
        [ let here = prefix <> [i]
              rest = if n.expanded then go (d + 1) here n.children else []
          in Row d n here : rest
        | (i, n) <- zip [0 ..] nodes ]

-- | What should preview the row under the cursor (a pane, window, or
-- session), if anything.
selectedPreview :: PickerState -> Maybe PreviewTarget
selectedPreview p = do
    r <- listToMaybe (drop p.cursor (visibleRows p))
    r.node.preview

-- | How wide to make the list column when a preview is shown beside it,
-- or 'Nothing' when the overlay is too narrow to split usefully.
pickerSplit :: Int -> Maybe Int
pickerSplit total
    | total >= 40 = Just (max 20 (total `div` 3))
    | otherwise   = Nothing

-- | Vertical placement of stacked window thumbnails in a preview @height@
-- rows tall: each window gets a one-row label then a thumbnail body, and
-- the bodies divide the available rows evenly. Returns, per shown window
-- (in order), @(labelRow, bodyTopRow, bodyHeight)@. Windows past what fits
-- (at least two rows each) are dropped — the list column still names them.
stackThumbnails :: Int -> Int -> [(Int, Int, Int)]
stackThumbnails height n
    | height <= 1 || n <= 0 = []
    | otherwise =
        [ (top, top + 1, bodyH) | k <- [0 .. shown - 1], let top = k * perBlock ]
  where
    shown    = min n (height `div` 2)   -- ≥2 rows each: label + ≥1 body row
    perBlock = height `div` shown
    bodyH    = perBlock - 1

-- | The rectangle the picker paints into: the whole content area (every
-- row but the status row) when zoomed, otherwise the active pane's
-- rectangle, falling back to the content area when there is no active
-- pane. @rowOff@ is where content starts (1 under a top status line).
pickerRegion :: Bool -> Size -> Int -> Maybe Rect -> Rect
pickerRegion isZoomed csize rowOff mActive
    | isZoomed  = full
    | otherwise = fromMaybe full mActive
  where
    full = Rect
        { startRow = rowOff
        , endRow = rowOff + max 0 (fromIntegral csize.rows - 1)
        , startCol = 0
        , endCol = fromIntegral csize.cols }

-- | Keep nodes matching the query (case-insensitive substring) along with
-- every ancestor of a match, force-expanding so matches are revealed.
filterTree :: Text -> [PickerNode] -> [PickerNode]
filterTree q nodes
    | T.null q = nodes
    | otherwise = concatMap keep nodes
  where
    ql = T.toLower q
    keep n
        | ql `T.isInfixOf` T.toLower n.label =
            [ n { expanded = not (null n.children) } ]
        | otherwise = case filterTree q n.children of
            []   -> []
            kids -> [ n { children = kids, expanded = True } ]

-- | Apply one key. In menu mode @j@/@k@ navigate, @l@/@h@ expand/collapse
-- and @/@ enters search; in search mode keys type into the query. Enter
-- runs the node under the cursor.
editPicker :: PickerState -> Key -> PickerEdit
editPicker p key
    | p.searching = searchKey
    | otherwise   = menuKey
  where
    rows = visibleRows p
    n = length rows
    clampC c = max 0 (min (max 0 (n - 1)) c)
    up   = PickerStay p { cursor = clampC (p.cursor - 1) }
    down = PickerStay p { cursor = clampC (p.cursor + 1) }
    cur = listToMaybe (drop p.cursor rows)
    runSel = case cur of
        Just r  -> PickerRun r.node.command
        Nothing -> PickerCancel
    -- Expansion edits address the node by its path; they are only allowed
    -- with no active query, where the path indexes 'roots' directly.
    setExp b r = PickerStay p { roots = modifyAt r.path (\nd -> nd { expanded = b }) p.roots }
    expand = case cur of
        Just r | T.null p.query, not (null r.node.children), not r.node.expanded -> setExp True r
        _ -> PickerStay p
    collapse = case cur of
        Just r | T.null p.query, r.node.expanded -> setExp False r
        _ -> up
    toggle = case cur of
        Just r | T.null p.query, not (null r.node.children) -> setExp (not r.node.expanded) r
        _ -> PickerStay p
    menuKey = case key.name of
        "Enter"  -> runSel
        "Escape" -> PickerCancel
        "q"      -> PickerCancel
        "C-c"    -> PickerCancel
        "j"      -> down
        "Down"   -> down
        "C-n"    -> down
        "k"      -> up
        "Up"     -> up
        "C-p"    -> up
        "l"      -> expand
        "Right"  -> expand
        "h"      -> collapse
        "Left"   -> collapse
        "O"      -> toggle
        "Space"  -> toggle
        "g"      -> PickerStay p { cursor = 0 }
        "G"      -> PickerStay p { cursor = max 0 (n - 1) }
        "/"      -> PickerStay p { searching = True }
        _        -> PickerStay p
    searchKey = case key.name of
        -- Commit the filter and drop back to menu mode with the cursor on
        -- the first match; a second Enter there activates it. This keeps a
        -- pre-typed search (@choose-tree ... ; send-keys /@) from firing the
        -- first hit the instant you finish typing.
        "Enter"  -> PickerStay p { searching = False }
        "Escape" -> PickerStay p { searching = False, query = "", cursor = 0 }
        "Up"     -> up
        "C-p"    -> up
        "Down"   -> down
        "C-n"    -> down
        "BSpace" -> PickerStay (reQuery (T.dropEnd 1 p.query))
        _        -> case insertText key of
            Just t  -> PickerStay (reQuery (p.query <> t))
            Nothing -> PickerStay p
    -- After editing the query, land the cursor on the first row that
    -- actually matches (a leaf), not on an ancestor kept only for context.
    reQuery q =
        let p' = p { query = q }
            matches r = T.toLower q `T.isInfixOf` T.toLower r.node.label
        in p' { cursor = case [ i | (i, r) <- zip [0 ..] (visibleRows p'), matches r ] of
                    (i : _) -> i
                    []      -> 0 }

-- | Apply @f@ to the node at the given index path within a forest.
modifyAt :: [Int] -> (PickerNode -> PickerNode) -> [PickerNode] -> [PickerNode]
modifyAt [] _ nodes = nodes
modifyAt (i : is) f nodes =
    [ if j == i then adjust nd else nd | (j, nd) <- zip [0 ..] nodes ]
  where
    adjust nd
        | null is   = f nd
        | otherwise = nd { children = modifyAt is f nd.children }

-- | The text a self-inserting key contributes to the query.
insertText :: Key -> Maybe Text
insertText key
    | key.name == "Space" = Just " "
    | T.length key.name == 1, c <- T.head key.name, c >= ' ' = Just key.name
    | otherwise = Nothing

-- | Render the picker into at most @height@ lines: a title/search line
-- then the visible rows, scrolled to keep the cursor in view. Each row is
-- indented by depth and prefixed with an expansion arrow; the returned
-- 'Bool' flags the cursor row so the caller can highlight it.
pickerLines :: Int -> PickerState -> [(Bool, Text)]
pickerLines height p = take height ((False, titleLine) : itemLines)
  where
    rows = visibleRows p
    titleLine = p.title <> (if p.searching then "  /" <> p.query else "")
    bodyH = max 1 (height - 1)
    start = max 0 (min (length rows - bodyH) (p.cursor - bodyH `div` 2))
    windowed = take bodyH (drop start rows)
    itemLines = [ (i == p.cursor, rowText r) | (i, r) <- zip [start ..] windowed ]
    rowText r = T.replicate (2 * r.depth) " " <> arrow r <> " " <> r.node.label
    arrow r
        | null r.node.children = " "
        | r.node.expanded      = "\x25be"   -- ▾
        | otherwise            = "\x25b8"    -- ▸
