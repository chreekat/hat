-- | The pure core of tmux's @mode-tree.c@, the list screen behind
-- choose-tree, choose-buffer and choose-client: flattening a rebuilt item
-- tree into lines under the mode's expansion state, moving the selection,
-- the generic keys every such mode shares, and drawing the list with its
-- preview box and prompt.
module Hat.Server.ModeTree
    ( Item (..)
    , NameAlign (..)
    , Line (..)
    , buildLines
    , listHeight
    , settle
    , KeyResult (..)
    , treeKey
    , searchSet
    , FuzzyView (..)
    , fuzzyView
    , fuzzyQuery
    , shownItems
    , taggedLines
    , SortSequence
    , sortOrderFromText
    , sortOrderText
    , defaultLineKey
    , HelpText (..)
    , TreeDraw (..)
    , DrawInput (..)
    , drawTree
    , drawFormat
    ) where

import Data.List (findIndex, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import Hat.FuzzyMatch (score)
import Hat.Geometry (Pos (..), Rect (..), Size (..))
import Hat.Model
import Hat.Server.FormatDraw (FormatColours (..), formatDrawIn, formatWidth)
import Hat.Server.Prompt (promptFor)
import Hat.Server.Theme (Theme)
import Hat.Term.Cell qualified as Cell

-- | One list item as a mode's builder produced it.
data Item = Item
    { tag           :: !TreeTag
    , name          :: !Text
    , text          :: !(Maybe Text)  -- ^ drawn after @name: @
    , children      :: ![Item]
    , startExpanded :: !Bool          -- ^ the expansion of an item not seen before
    , align         :: !NameAlign
    , searchText    :: ![Text]        -- ^ what @n@ and @N@ match against
    , fuzzyName     :: !(Maybe Text)
        -- ^ its part of the path @/@ fuzzily matches; 'Nothing' for an item
        --   shown only around its matching neighbours
    }
    deriving (Eq, Show)

-- | How an item's name lines up with its siblings' (tmux's @mode_tree_align@).
data NameAlign = AlignNone | AlignLeft | AlignRight
    deriving (Eq, Show)

-- | One visible line.
data Line = Line
    { item        :: !Item
    , depth       :: !Int
    , lastSibling :: !Bool
    , flat        :: !Bool        -- ^ no sibling has children
    , ancestors   :: ![TreeTag]   -- ^ nearest first
    , parentLast  :: !Bool        -- ^ the parent is the last of its siblings
    , open        :: !Bool        -- ^ the item is expanded
    , key         :: !(Maybe Text)  -- ^ the key that chooses this line
    }
    deriving (Eq, Show)

isExpanded :: TreeState -> Item -> Bool
isExpanded st i = Map.findWithDefault i.startExpanded i.tag st.expanded

-- | Flatten the item tree into lines, descending into expanded items. Keys
-- are left for the caller (they may need the mode's key format).
buildLines :: TreeState -> [Item] -> [Line]
buildLines st = go 0 [] False
  where
    go d parents pLast items =
        let flatHere = all (null . (.children)) items
            n = length items
        in concat
            [ let open = isExpanded st i
                  here = Line
                    { item = i, depth = d, lastSibling = k == n - 1
                    , flat = flatHere, ancestors = parents
                    , parentLast = pLast, open = open, key = Nothing }
              in here : if open
                    then go (d + 1) (i.tag : parents) (k == n - 1) i.children
                    else []
            | (k, i) <- zip [0 :: Int ..] items ]

-- | The list's height on a screen @sy@ rows tall holding @n@ lines (tmux's
-- @mode_tree_set_height@): the rest goes to the preview.
listHeight :: Int -> Int -> PreviewSize -> Int
listHeight sy n preview = keepTwo $ case preview of
    PreviewOff -> sy
    PreviewNormal ->
        let h0 = (sy `div` 3) * 2
            h1 = if h0 > n then sy `div` 2 else h0
        in if h1 < 10 then sy else h1
    PreviewBig ->
        let h0 = sy `div` 4
        in max 2 (if h0 > n then n else h0)
  where
    keepTwo h = if sy - h < 2 then sy else h

-- | Pin the selection after a rebuild: onto the selected item's line, or —
-- when it vanished — the line it was on, clamped to the list; then scroll
-- just enough to show it.
settle :: Int -> [Line] -> TreeState -> TreeState
settle height ls st = case st.current >>= \t -> findIndex ((== t) . (.item.tag)) ls of
    Just i -> place i
    Nothing
        | null ls -> st { currentIx = 0, offset = 0, current = Nothing }
        | otherwise -> place (min st.currentIx (length ls - 1))
  where
    place i = st
        { currentIx = i
        , current = Just (ls !! i).item.tag
        , offset = showing height i st.offset }

-- | The offset that shows line @i@, moving the window as little as possible.
showing :: Int -> Int -> Int -> Int
showing height i o
    | i < o = i
    | i > o + height - 1 = i - height + 1
    | otherwise = o

-- | What a key did to a tree mode.
data KeyResult = KeyResult
    { state     :: !TreeState
    , finished  :: !Bool          -- ^ leave the mode
    , unhandled :: !(Maybe Text)  -- ^ a key for the mode's own handler
    }
    deriving (Eq, Show)

-- | The sort orders the @O@ key cycles through, first the default.
type SortSequence = [SortOrder]

-- | One key against a settled tree mode (tmux's @mode_tree_key@): the
-- generic moves, tags, sorts, expansion, search and filter. A key a line
-- answers to selects it and becomes Enter; anything else is the mode's.
treeKey
    :: Int -> SortSequence -> [Item] -> [Line] -> TreeState -> Text -> KeyResult
treeKey height sortSeq roots ls st key
    | null ls = done st
    | st.showHelp = stay st { showHelp = False }
    | Just i <- findIndex ((== Just key) . (.key)) ls =
        KeyResult (select i st) False (Just "Enter")
    | otherwise = case key of
        _ | key `elem` ["q", "Escape", "C-[", "C-g"] -> done st
          | key `elem` ["F1", "C-h"] -> stay st { showHelp = True }
          | key `elem` ["Up", "k", "C-p"] -> stay (up True st)
          | key `elem` ["Down", "j", "C-n"] -> stay (down True st)
          | key `elem` ["PgUp", "C-b"] -> stay (pageUp st)
          | key `elem` ["PgDn", "C-f"] -> stay (pageDown st)
          | key `elem` ["g", "Home"] -> stay (select 0 st) { offset = 0 }
          | key `elem` ["G", "End"] ->
              let c = n - 1
              in stay (select c st) { offset = if c > height - 1 then c - height + 1 else 0 }
          | key `elem` ["Left", "h", "-"] -> stay collapse
          | key `elem` ["Right", "l", "+"] -> stay expand
          | key `elem` ["?", "/", "C-s"] ->
              stay st { modePrompt = Just (linePrompt "(search) " "" (PromptSearch st.current)) }
          | key `elem` ["K", "S-Up", "J", "S-Down"] -> pass
        "t" -> stay toggleTag
        "T" -> stay st { tagged = filter (`notElem` visibleTags) st.tagged }
        "C-t" -> stay st
            { tagged = [ l.item.tag | l <- ls, l.depth == 0 ]
                <> filter (`notElem` visibleTags) st.tagged }
        "O" -> stay st { sort = st.sort { order = nextOrder st.sort.order } }
        "r" -> stay st { sort = st.sort { reversed = not st.sort.reversed } }
        "M--" -> stay st { expanded = foldr (\i -> Map.insert i.tag False) st.expanded roots }
        "M-+" -> stay st { expanded = foldr (\i -> Map.insert i.tag True) st.expanded roots }
        "n" -> stay (searchSet roots Forward st)
        "N" -> stay (searchSet roots Backward st)
        "f" -> stay st { modePrompt = Just (linePrompt "(filter) " (fromMaybe "" st.filter) PromptFilter) }
        "c" -> stay st { modePrompt = Nothing, filter = Nothing }
        "v" -> stay st { preview = case st.preview of
            PreviewOff -> PreviewBig
            PreviewNormal -> PreviewOff
            PreviewBig -> PreviewNormal }
        _ -> pass
  where
    n = length ls
    cur = ls !! st.currentIx
    visibleTags = map (.item.tag) ls
    done s = KeyResult s True Nothing
    stay s = KeyResult s False Nothing
    pass = KeyResult st False (Just key)
    select i s = s { currentIx = i, current = Just (ls !! i).item.tag
                   , offset = showing height i s.offset }
    up wrap s
        | s.currentIx == 0 =
            if wrap then (select (n - 1) s) { offset = if n >= height then n - height else s.offset }
                    else s
        | otherwise = select (s.currentIx - 1) s
    down wrap s
        | s.currentIx == n - 1 = if wrap then (select 0 s) { offset = 0 } else s
        | otherwise = select (s.currentIx + 1) s
    pageUp s = iterate (up False) s !! min height s.currentIx
    pageDown s = iterate (down False) s !! min height (n - 1 - s.currentIx)
    collapse
        | cur.flat || not cur.open = case cur.ancestors of
            [] -> up False st
            (p : _) -> case findIndex ((== p) . (.item.tag)) ls of
                Just i -> (select i st) { expanded = Map.insert p False st.expanded }
                Nothing -> st
        | otherwise = st { expanded = Map.insert cur.item.tag False st.expanded }
    expand
        | cur.flat || cur.open = down False st
        | otherwise = st { expanded = Map.insert cur.item.tag True st.expanded }
    toggleTag
        | cur.item.tag `elem` st.tagged = st { tagged = filter (/= cur.item.tag) st.tagged }
        | otherwise = st
            { tagged = cur.item.tag : filter (`notElem` (cur.ancestors <> subtree cur.item)) st.tagged }
    subtree i = concatMap (\c -> c.tag : subtree c) i.children
    nextOrder o = case dropWhile (/= o) sortSeq of
        _ : o' : _ -> o'
        _ -> fromMaybe o (listToMaybe sortSeq)
    linePrompt label initial use = TreePrompt
        { edit = promptFor label initial ""
        , use = use
        , entry = PromptLine
        }

-- | Select the next item (in @dir@, wrapping, through collapsed items too)
-- whose search text holds the current search term, expanding its parents
-- (tmux's @mode_tree_search_set@). Case-insensitive when the term is all
-- lowercase.
searchSet :: [Item] -> SearchDirection -> TreeState -> TreeState
searchSet roots dir st = case st.search of
    Nothing -> st
    Just term -> case matchAfter term of
        Nothing -> st
        Just (hit, parents) -> st
            { expanded = foldr (`Map.insert` True) st.expanded parents
            , current = Just hit }
  where
    -- Every item in depth-first order with its ancestors.
    flat = walk [] roots
    walk ps = concatMap (\i -> (i, ps) : walk (i.tag : ps) i.children)
    ordered = case dir of
        Forward -> flat
        Backward -> reverse flat
    rotated = case break (\(i, _) -> Just i.tag == st.current) ordered of
        (before, here : after) -> after <> before <> [here]
        (before, []) -> before
    matchAfter term = listToMaybe
        [ (i.tag, ps) | (i, ps) <- rotated, any (matches term) i.searchText ]
    matches term t
        | T.toLower term == term = term `T.isInfixOf` T.toLower t
        | otherwise = term `T.isInfixOf` t

-- Note [Fuzzy tree search]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- Hat's tree modes search as its old chooser did, not as tmux's mode-tree
-- does (an incremental jump to the next substring match). While the @/@
-- prompt holds a query, the list shows only the items whose path -- their
-- ancestors' fuzzy names and their own, space-joined -- the query fuzzily
-- matches ('Hat.FuzzyMatch.score'), under their ancestors for context,
-- with siblings ordered by the best score in their subtree so the best
-- match comes first, selected. Each key typed narrows and re-ranks the list
-- at once; Up and Down move through it, Enter chooses the selected item, and
-- Escape returns to the whole tree, selected where it was before the search.
-- An item with no fuzzy name (a pane) is context only, never a match; a
-- matching item keeps its other children.

-- | What a fuzzy search keeps of a mode's items. See Note [Fuzzy tree search].
data FuzzyView = FuzzyView
    { kept   :: ![Item]           -- ^ the matches under their ancestors, best first
    , opened :: ![TreeTag]        -- ^ the ancestors opened to show a match
    , best   :: !(Maybe TreeTag)  -- ^ the best match
    }

-- | An item with its search scores: its own, and its subtree's best.
data Ranked = Ranked
    { item    :: !Item
    , own     :: !(Maybe Int)
    , subtree :: !Int
    , matched :: ![Ranked]  -- ^ the children with a match in their subtree, ranked
    }

-- | The items a fuzzy query keeps. See Note [Fuzzy tree search].
fuzzyView :: Text -> [Item] -> FuzzyView
fuzzyView q items
    | T.null q = FuzzyView { kept = items, opened = [], best = Nothing }
    | otherwise = FuzzyView
        { kept = map keep ranked
        , opened = concatMap ancestors ranked
        , best = listToMaybe
            [ r.item.tag | r <- concatMap flatten ranked, r.own == Just top ]
        }
  where
    ranked = rankAll "" items
    top = maximum (minBound : map (.subtree) ranked)
    rankAll prefix = sortOn (Down . (.subtree)) . concatMap (rank prefix)
    rank prefix i =
        let ownScore = i.fuzzyName >>= \n -> score q (prefix <> n)
            kids = rankAll (maybe prefix (\n -> prefix <> n <> " ") i.fuzzyName) i.children
            scores = maybe [] pure ownScore <> map (.subtree) kids
        in [ Ranked { item = i, own = ownScore, subtree = maximum scores, matched = kids }
           | not (null scores) ]
    keep r = r.item
        { children = map keep r.matched <> case r.own of
            Just _ -> [ c | c <- r.item.children, c.tag `notElem` map (.item.tag) r.matched ]
            Nothing -> [] }
    ancestors r = [ r.item.tag | not (null r.matched) ] <> concatMap ancestors r.matched
    flatten r = r : concatMap flatten r.matched

-- | The query a mode's open search prompt holds, if any.
fuzzyQuery :: TreeState -> Maybe Text
fuzzyQuery ts = case ts.modePrompt of
    Just p | PromptSearch _ <- p.use, not (T.null p.edit.input) -> Just p.edit.input
    _ -> Nothing

-- | The items a mode lays out, and the state whose expansion it lays them
-- out under: while a fuzzy search holds a query, its matches with their
-- ancestors opened; else every item.
shownItems :: TreeState -> [Item] -> ([Item], TreeState)
shownItems ts items = case fuzzyQuery ts of
    Nothing -> (items, ts)
    Just q ->
        let v = fuzzyView q items
        in (v.kept, ts { expanded = foldr (`Map.insert` True) ts.expanded v.opened })

-- | The tagged items among the visible lines (tmux only acts on those).
taggedLines :: TreeState -> [Line] -> [Line]
taggedLines st = filter ((`elem` st.tagged) . (.item.tag))

-- | tmux's @sort_order_from_string@.
sortOrderFromText :: Text -> Maybe SortOrder
sortOrderFromText t = case T.toLower t of
    "activity" -> Just SortActivity
    "creation" -> Just SortCreation
    "index" -> Just SortIndex
    "key" -> Just SortIndex
    "modifier" -> Just SortModifier
    "name" -> Just SortName
    "title" -> Just SortName
    "order" -> Just SortNatural
    "size" -> Just SortSize
    "z" -> Just SortZ
    _ -> Nothing

-- | tmux's @sort_order_to_string@.
sortOrderText :: SortOrder -> Text
sortOrderText = \case
    SortActivity -> "activity"
    SortCreation -> "creation"
    SortIndex -> "index"
    SortModifier -> "modifier"
    SortName -> "name"
    SortNatural -> "order"
    SortSize -> "size"
    SortZ -> "z"

-- | The key tmux's default key format gives line @i@: its number up to 9,
-- then @M-a@ to @M-z@.
defaultLineKey :: Int -> Maybe Text
defaultLineKey i
    | i < 10 = Just (T.pack (show i))
    | i < 36 = Just ("M-" <> T.singleton (toEnum (97 + i - 10)))
    | otherwise = Nothing

-- | A mode's key help: what @%1@ names in the lines, and its own lines.
data HelpText = HelpText
    { itemName :: !Text
    , width    :: !Int
    , lines    :: ![Text]
    }

-- | Everything 'drawTree' draws from besides the mode state.
data DrawInput = DrawInput
    { size        :: !Size
    , lines       :: ![Line]
    , noMatches   :: !Bool
    , theme       :: !Theme
    , selectStyle :: !Cell.Style   -- ^ @tree-mode-selection-style@
    , borderStyle :: !Cell.Style   -- ^ @tree-mode-border-style@
    , promptStyle :: !Cell.Style   -- ^ @message-style@
    , promptTop   :: !Bool         -- ^ the prompt sits on the top row
    , sortShown   :: !Bool         -- ^ the mode sorts (shows its order)
    , viewName    :: !(Maybe Text)
    , help        :: !HelpText
    }

-- | A drawn tree mode: its screen, where the preview of which item goes,
-- and where the cursor shows (only while a prompt is open).
data TreeDraw = TreeDraw
    { cells       :: !(V.Vector (V.Vector Cell.Cell))
    , previewArea :: !(Maybe (Rect, TreeTag))
    , cursor      :: !(Maybe Pos)
    }

-- | The cells a format draws at its own width, from the @base@ style.
drawFormat :: Theme -> FormatColours -> Cell.Style -> Text -> [Cell.Cell]
drawFormat th use base t = V.toList
    (formatDrawIn use th base t (V.replicate (formatWidth t) (Cell.glyphCell ' ' base)))

-- | Draw the mode's screen (tmux's @mode_tree_draw@).
drawTree :: DrawInput -> TreeState -> TreeDraw
drawTree inp st = TreeDraw
    { cells = withPrompt (withHelp (withBox listed))
    , previewArea = previewRect
    , cursor = promptCursor
    }
  where
    w = fromIntegral inp.size.cols
    sy = fromIntegral inp.size.rows
    ls = inp.lines
    h = listHeight sy (length ls) st.preview
    blankRow = V.replicate w Cell.blankCell
    grid0 = V.replicate sy blankRow
    keylen = maximum (0 : [ T.length k + 3 | Just k <- map (.key) ls ])
    alignLen d = maximum (0 : [ T.length l.item.name | l <- ls, l.depth == d
                                                  , l.item.align /= AlignNone ])
    listed
        | null ls || w == 0 || h == 0 = grid0
        | otherwise = grid0 V.//
            [ (i - st.offset, lineRow i l)
            | (i, l) <- zip [0 ..] ls, i >= st.offset, i <= st.offset + h - 1 ]
    lineRow i l =
        let selected = i == st.currentIx
            tagged = l.item.tag `elem` st.tagged
            tint s = if tagged then s { Cell.fg = Cell.Indexed 6 } else s
            base = tint (if selected then inp.selectStyle else Cell.defaultStyle)
            use = if selected then BaseColours else OwnColours
            prefix = take w (drawFormat inp.theme use base (prefixFormat l))
            left = w - length prefix
            nm = padName l
            label = nm <> (if tagged then "*" else "")
                <> (if isJust l.item.text then "#[fg=themelightgrey]: #[default]" else "")
            labelCells = take left (drawFormat inp.theme use base label)
            textCells = case l.item.text of
                Just t -> take (left - length labelCells) (drawFormat inp.theme use base t)
                Nothing -> []
            body = prefix <> labelCells <> textCells
            fill = Cell.glyphCell ' ' (if selected then base else Cell.defaultStyle)
        in V.fromList (take w (body <> repeat fill))
    padName l =
        let nmT = l.item.name
            pad = alignLen l.depth - T.length nmT
        in case l.item.align of
            AlignRight -> T.replicate pad " " <> nmT
            AlignLeft -> nmT <> T.replicate pad " "
            AlignNone -> nmT
    prefixStyle = "#[fg=themelightgrey]#[bg=default]#[noacs]"
    prefixFormat l = prefixStyle
        <> padRight keylen (maybe "" (\k -> "(" <> k <> ")") l.key)
        <> T.concat (replicate (max 0 (l.depth - 1))
            (if l.parentLast then "    " else "#[acs]x" <> prefixStyle <> "   "))
        <> (if l.depth > 0
                then "#[acs]" <> (if l.lastSibling then "mq" else "tq") <> prefixStyle <> "> "
                else "")
        <> (if not (null l.item.children)
                then if l.open
                    then "#[fg=themered]-" <> prefixStyle <> " "
                    else "#[fg=themegreen]+" <> prefixStyle <> " "
                else if l.flat then "" else "  ")
    padRight n t = t <> T.replicate (n - T.length t) " "

    -- The preview box under the list.
    boxRows = sy - h
    showBox = st.preview /= PreviewOff && not (null ls)
        && sy > 4 && h >= 2 && boxRows > 4 && w > 4
    selectedItem = (ls !! st.currentIx).item
    withBox g
        | not showBox = g
        | otherwise = stamp h 1 titleCells
            (g V.// [ (r, boxRow (r - h)) | r <- [h .. sy - 1] ])
    boxRow k
        | k == 0 = edge '┌' '─' '┐'
        | k == boxRows - 1 = edge '└' '─' '┘'
        | otherwise = V.fromList ([side '│'] <> replicate (w - 2) Cell.blankCell <> [side '│'])
    edge a b c = V.fromList ([side a] <> replicate (w - 2) (side b) <> [side c])
    side c = Cell.glyphCell c inp.borderStyle
    title = " " <> selectedItem.name
        <> (if inp.sortShown
                then " (sort: " <> sortOrderText st.sort.order
                    <> (if st.sort.reversed then ", reversed" else "") <> ")"
                    <> maybe "" (\v -> " (view: " <> v <> ")") inp.viewName
                else "")
    status = if inp.noMatches then "no matches" else "active"
    titleCells
        | w - 2 < T.length title = []
        | isJust st.filter, w - 2 >= T.length title + 10 + T.length status + 2 =
            styled (title <> " (filter: " <> status <> ") ")
        | otherwise = styled (title <> " ")
    styled t = [ Cell.glyphCell c inp.borderStyle | c <- T.unpack t ]
    previewRect
        | showBox, w - 4 > 0, boxRows - 2 > 0 = Just
            ( Rect { startRow = h + 1, endRow = h + 1 + boxRows - 2
                   , startCol = 2, endCol = 2 + w - 4 }
            , selectedItem.tag )
        | otherwise = Nothing

    -- The key help, boxed over the middle of the screen.
    helpLines = map (T.replace "%1" inp.help.itemName . T.replace
            "#{E:tree-mode-border-style}" "bg=themedarkgrey,fg=themelightgrey")
        (helpStart <> inp.help.lines <> helpEnd)
    helpW = max 39 inp.help.width
    withHelp g
        | not st.showHelp || w < helpW + 2 || sy < length helpLines + 2 = g
        | otherwise =
            let bw = helpW + 2
                bh = length helpLines + 2
                x = (w - bw) `div` 2
                y = (sy - bh) `div` 2
                top = [side '┌'] <> replicate helpW (side '─') <> [side '┐']
                bottom = [side '└'] <> replicate helpW (side '─') <> [side '┘']
                mid l = [side '│']
                    <> take helpW (drawFormat inp.theme OwnColours Cell.defaultStyle l
                        <> repeat Cell.blankCell)
                    <> [side '│']
                rows = [top] <> map mid helpLines <> [bottom]
            in foldl (\acc (k, r) -> stamp (y + k) x r acc) g (zip [0 ..] rows)

    -- The prompt, on the top or bottom row.
    promptRow = if inp.promptTop then 0 else sy - 1
    withPrompt g = case st.modePrompt of
        Just p | sy > 0 ->
            let txt = p.edit.promptLabel <> p.edit.input
                row = V.fromList (take w ([ Cell.glyphCell c inp.promptStyle | c <- T.unpack txt ]
                    <> repeat (Cell.glyphCell ' ' inp.promptStyle)))
            in g V.// [(promptRow, row)]
        _ -> g
    promptCursor = case st.modePrompt of
        Just p | sy > 0 -> Just Pos
            { row = promptRow
            , col = min (w - 1) (T.length p.edit.promptLabel + p.edit.cursor) }
        _ -> Nothing

-- | Overwrite the cells of @g@ from @(row, col)@ rightwards with @cs@,
-- clipped to the grid.
stamp :: Int -> Int -> [Cell.Cell] -> V.Vector (V.Vector Cell.Cell)
      -> V.Vector (V.Vector Cell.Cell)
stamp r c cs g = case g V.!? r of
    Nothing -> g
    Just row -> g V.// [(r, row V.//
        [ (c + k, x) | (k, x) <- zip [0 ..] cs, c + k < V.length row, c + k >= 0 ])]

helpStart :: [Text]
helpStart = map ("#[fg=themelightgrey]" <>)
    [ "      Up, k #[#{E:tree-mode-border-style},acs]x#[default] Move cursor up"
    , "    Down, j #[#{E:tree-mode-border-style},acs]x#[default] Move cursor down"
    , "          g #[#{E:tree-mode-border-style},acs]x#[default] Go to top"
    , "          G #[#{E:tree-mode-border-style},acs]x#[default] Go to bottom"
    , " PPage, C-b #[#{E:tree-mode-border-style},acs]x#[default] Page up"
    , " NPage, C-f #[#{E:tree-mode-border-style},acs]x#[default] Page down"
    , "    Left, h #[#{E:tree-mode-border-style},acs]x#[default] Collapse %1"
    , "   Right, l #[#{E:tree-mode-border-style},acs]x#[default] Expand %1"
    , "        M-- #[#{E:tree-mode-border-style},acs]x#[default] Collapse all %1s"
    , "        M-+ #[#{E:tree-mode-border-style},acs]x#[default] Expand all %1s"
    , "          t #[#{E:tree-mode-border-style},acs]x#[default] Toggle %1 tag"
    , "          T #[#{E:tree-mode-border-style},acs]x#[default] Untag all %1s"
    , "        C-t #[#{E:tree-mode-border-style},acs]x#[default] Tag all %1s"
    , "        C-s #[#{E:tree-mode-border-style},acs]x#[default] Search forward"
    , "          n #[#{E:tree-mode-border-style},acs]x#[default] Repeat search forward"
    , "          N #[#{E:tree-mode-border-style},acs]x#[default] Repeat search backward"
    , "          f #[#{E:tree-mode-border-style},acs]x#[default] Filter %1s"
    , "          O #[#{E:tree-mode-border-style},acs]x#[default] Change sort order"
    , "          r #[#{E:tree-mode-border-style},acs]x#[default] Reverse sort order"
    , "          v #[#{E:tree-mode-border-style},acs]x#[default] Toggle preview"
    ]

helpEnd :: [Text]
helpEnd = map ("#[fg=themelightgrey]" <>)
    [ "  q, Escape #[#{E:tree-mode-border-style},acs]x#[default] Exit mode" ]
