-- | The chooser overlay's pure logic: flattening the session/window/pane
-- tree into visible rows (honoring each node's expansion and the search
-- query), mapping one key at a time onto navigation/expansion/search
-- edits, and laying out the visible rows. The server owns the
-- 'PickerState' (per client), feeds keys here, and runs the chosen node's
-- command.
module Hat.Server.Picker
    ( PickerEdit (..)
    , RowSelected (..)
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

import Data.List (findIndex)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Hat.Geometry (Rect (..), Size (..))
import Hat.Model
    ( Expansion (..), PickerFill (..), PickerMode (..), PickerNode (..)
    , PickerState (..), PreviewTarget )
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
    , children = [], expanded = Collapsed }

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
              rest = case n.expanded of
                  Expanded  -> go (d + 1) here n.children
                  Collapsed -> []
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
pickerRegion :: PickerFill -> Size -> Int -> Maybe Rect -> Rect
pickerRegion fill csize rowOff mActive = case fill of
    FillWindow -> full
    PaneRegion -> fromMaybe full mActive
  where
    full = Rect
        { startRow = rowOff
        , endRow = rowOff + max 0 (fromIntegral csize.rows - 1)
        , startCol = 0
        , endCol = fromIntegral csize.cols }

-- | A fuzzy match: is the query a subsequence of the target (its characters
-- appearing in order, not necessarily contiguously)? Both are compared
-- lower-cased by the callers. This is what lets @projhat@ match the @hat@
-- window under the @projects@ session, matched against the joined path.
fuzzySubsequence :: Text -> Text -> Bool
fuzzySubsequence query target = go (T.unpack query) (T.unpack target)
  where
    go [] _ = True
    go _ [] = False
    go (c : cs) (d : ds)
        | c == d    = go cs ds
        | otherwise = go (c : cs) ds

-- | Whether a node matches the query: the query is a fuzzy subsequence of the
-- node's path — its ancestor labels joined with its own label. So a query can
-- span the session\/window\/pane levels (@projhat@ → @projects hat@).
nodeMatches :: Text -> Text -> PickerNode -> Bool
nodeMatches ql prefix n = fuzzySubsequence ql (prefix <> T.toLower n.label)

-- | The path prefix a node hands its children: its own path plus a separator.
childPrefix :: Text -> PickerNode -> Text
childPrefix prefix n = prefix <> T.toLower n.label <> " "

-- | Keep nodes matching the query (a fuzzy subsequence of the node's path)
-- along with every ancestor of a match, force-expanding so matches are
-- revealed.
filterTree :: Text -> [PickerNode] -> [PickerNode]
filterTree q = go ""
  where
    ql = T.toLower q
    go prefix nodes
        | T.null q = nodes
        | otherwise = concatMap (keep prefix) nodes
    keep prefix n
        | nodeMatches ql prefix n =
            [ n { expanded = if null n.children then Collapsed else Expanded } ]
        | otherwise = case go (childPrefix prefix n) n.children of
            []   -> []
            kids -> [ n { children = kids, expanded = Expanded } ]

-- | Apply one key. In menu mode @j@/@k@ navigate, @l@/@h@ expand/collapse
-- and @/@ enters search; in search mode keys type into the query. Enter
-- runs the node under the cursor.
editPicker :: PickerState -> Key -> PickerEdit
editPicker p key = case p.mode of
    Searching -> searchKey
    Browsing  -> menuKey
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
    setExp e r = PickerStay p { roots = modifyAt r.path (\nd -> nd { expanded = e }) p.roots }
    flipExp e = case e of Expanded -> Collapsed; Collapsed -> Expanded
    expand = case cur of
        Just r | T.null p.query, not (null r.node.children), r.node.expanded == Collapsed -> setExp Expanded r
        _ -> PickerStay p
    collapse = case cur of
        Just r | T.null p.query, r.node.expanded == Expanded -> setExp Collapsed r
        _ -> up
    toggle = case cur of
        Just r | T.null p.query, not (null r.node.children) -> setExp (flipExp r.node.expanded) r
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
        "/"      -> PickerStay p { mode = Searching }
        "n"      -> nextMatch
        "b"      -> prevMatch
        _        -> PickerStay p
    searchKey = case key.name of
        -- Commit the search: drop back to menu mode with the filter cleared
        -- so the full tree shows again, the cursor on the first match (its
        -- collapsed ancestors expanded to reveal it). A second Enter there
        -- activates it, which keeps a pre-typed search (@choose-tree ... ;
        -- send-keys /@) from firing the first hit the instant you finish
        -- typing.
        "Enter"  -> PickerStay commitSearch
        "Escape" -> PickerStay p { mode = Browsing, query = "", search = "", cursor = 0 }
        "Up"     -> up
        "C-p"    -> up
        "Down"   -> down
        "C-n"    -> down
        "BSpace" -> PickerStay (reQuery (T.dropEnd 1 p.query))
        _        -> case insertText key of
            Just t  -> PickerStay (reQuery (p.query <> t))
            Nothing -> PickerStay p
    -- In menu mode, jump the cursor to the next/previous node matching the
    -- committed search (over the whole tree, not just the visible rows),
    -- revealing any collapsed ancestors and wrapping around the ends; with
    -- no active search or no match the cursor stays put. Depth-first order
    -- coincides with the paths' lexicographic order, so a match is "next"
    -- when its path sorts after the cursor's.
    curPath = maybe [] (.path) cur
    menuMatches = allMatchPaths p.search p.roots
    jumpTo mp =
        let p' = p { roots = revealPath mp p.roots }
        in PickerStay p' { cursor = fromMaybe p.cursor (findIndex ((== mp) . (.path)) (visibleRows p')) }
    nextMatch = case filter (> curPath) menuMatches <> menuMatches of
        (mp : _) -> jumpTo mp
        []       -> PickerStay p
    prevMatch = case reverse (filter (< curPath) menuMatches) <> reverse menuMatches of
        (mp : _) -> jumpTo mp
        []       -> PickerStay p
    commitSearch
        | T.null p.query = p { mode = Browsing, search = "" }
        | otherwise = case firstMatchPath p.query p.roots of
            Nothing -> cleared { cursor = min p.cursor (max 0 (length (visibleRows cleared) - 1)) }
            Just mp ->
                let p' = cleared { roots = revealPath mp p.roots }
                in p' { cursor = fromMaybe 0 (findIndex ((== mp) . (.path)) (visibleRows p')) }
      where
        cleared = p { mode = Browsing, query = "", search = p.query }
    -- After editing the query, land the cursor on the first row that
    -- actually matches, not on an ancestor kept only for context.
    reQuery q =
        let p' = p { query = q }
        in p' { cursor = case firstMatchPath q p'.roots of
                    Just mp -> fromMaybe 0 (findIndex ((== mp) . (.path)) (visibleRows p'))
                    Nothing -> 0 }

-- | The index path of the first node, depth-first, that matches the query
-- (a fuzzy subsequence of its path) — the same node the filtered view puts
-- first, since filtering preserves depth-first order.
firstMatchPath :: Text -> [PickerNode] -> Maybe [Int]
firstMatchPath q = listToMaybe . go ""
  where
    ql = T.toLower q
    go prefix nodes = concat
        [ if nodeMatches ql prefix n
              then [[i]]
              else map (i :) (go (childPrefix prefix n) n.children)
        | (i, n) <- zip [0 ..] nodes ]

-- | The index paths of every node, depth-first, that matches the query (a
-- fuzzy subsequence of its path), descending into matches too so a matching
-- child of a matching parent is its own stop; empty for an empty query.
allMatchPaths :: Text -> [PickerNode] -> [[Int]]
allMatchPaths q nodes
    | T.null q  = []
    | otherwise = go "" [] nodes
  where
    ql = T.toLower q
    go lprefix iprefix ns = concat
        [ let here = iprefix <> [i]
              this = [ here | nodeMatches ql lprefix n ]
          in this <> go (childPrefix lprefix n) here n.children
        | (i, n) <- zip [0 ..] ns ]

-- | Expand every ancestor along the index path (not the node itself), so
-- the node at the path is visible.
revealPath :: [Int] -> [PickerNode] -> [PickerNode]
revealPath (i : is@(_ : _)) nodes =
    [ if j == i
          then nd { expanded = Expanded, children = revealPath is nd.children }
          else nd
    | (j, nd) <- zip [0 ..] nodes ]
revealPath _ nodes = nodes

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

-- | Whether a rendered picker row is the one under the cursor, so the
-- caller can highlight it.
data RowSelected = SelectedRow | UnselectedRow
    deriving (Eq, Show)

-- | Render the picker into at most @height@ lines: a title/search line
-- then the visible rows, scrolled to keep the cursor in view. Each row is
-- indented by depth and prefixed with an expansion arrow; the flag marks
-- the cursor row.
pickerLines :: Int -> PickerState -> [(RowSelected, Text)]
pickerLines height p = take height ((UnselectedRow, titleLine) : itemLines)
  where
    rows = visibleRows p
    titleLine = p.title <> (case p.mode of Searching -> "  /" <> p.query; Browsing -> "")
    bodyH = max 1 (height - 1)
    start = max 0 (min (length rows - bodyH) (p.cursor - bodyH `div` 2))
    windowed = take bodyH (drop start rows)
    itemLines =
        [ (if i == p.cursor then SelectedRow else UnselectedRow, rowText r)
        | (i, r) <- zip [start ..] windowed ]
    rowText r = T.replicate (2 * r.depth) " " <> arrow r <> " " <> r.node.label
    arrow r = case r.node.expanded of
        _ | null r.node.children -> " "
        Expanded                 -> "\x25be"   -- ▾
        Collapsed                -> "\x25b8"    -- ▸
