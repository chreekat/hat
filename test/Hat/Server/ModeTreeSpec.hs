module Hat.Server.ModeTreeSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Hspec

import Hat.Geometry (Size (..))
import Hat.Model
import Hat.Server.Command.Types (templateReplace)
import Hat.Server.FormatDraw (styleCell)
import Hat.Server.ModeTree
import Hat.Server.Prompt (promptFor)
import Hat.Server.Theme (terminalTheme)
import Hat.Term.Cell qualified as Cell

-- | Session @s@ holding window @0@ with panes 1 and 2, and window @1@ with
-- pane 3; then session @t@ with window @0@ holding pane 4.
items :: [Item]
items =
    [ node (TagSession (SessionId 0)) "s"
        [ node (TagWinlink (SessionId 0) 0) "0" [leaf (TagPane (PaneId 1)) "0", leaf (TagPane (PaneId 2)) "1"]
        , node (TagWinlink (SessionId 0) 1) "1" [leaf (TagPane (PaneId 3)) "0"] ]
    , node (TagSession (SessionId 1)) "t"
        [ node (TagWinlink (SessionId 1) 0) "0" [leaf (TagPane (PaneId 4)) "0"] ]
    ]
  where
    node t n cs = Item
        { tag = t, name = n, text = Just "x", children = cs, startExpanded = True
        , align = AlignNone, searchText = [n], fuzzyName = Just n
        , tagging = Taggable, previewFrom = OwnPreview }
    leaf t n = (node t n []) { fuzzyName = Nothing }

-- | Session @work@ holding windows @chat@ and @mail@, then session
-- @projects@ holding windows @notes@ and @hat@; one pane in each window.
named :: [Item]
named =
    [ session 0 "work" [window 0 0 "chat" 1, window 0 1 "mail" 2]
    , session 1 "projects" [window 1 0 "notes" 3, window 1 1 "hat" 4] ]
  where
    session i n = item (TagSession (SessionId i)) n (Just n)
    window s i n p = item (TagWinlink (SessionId s) i) (T.pack (show i)) (Just n)
        [item (TagPane (PaneId p)) "0" Nothing []]
    item t n fz cs = Item
        { tag = t, name = n, text = Nothing, children = cs, startExpanded = True
        , align = AlignNone, searchText = [n], fuzzyName = fz
        , tagging = Taggable, previewFrom = OwnPreview }

-- | An item tree's tags, depth-first.
tags :: [Item] -> [TreeTag]
tags = concatMap (\i -> i.tag : tags i.children)

-- | 'fresh' with its search prompt open on a query.
searching :: Text -> TreeState
searching q = fresh
    { modePrompt = Just TreePrompt
        { edit = promptFor "(search) " q "", use = PromptSearch Nothing, entry = PromptLine } }

fresh :: TreeState
fresh = TreeState
    { kind = SessionTree
    , origin = TreeOrigin { fromSession = SessionId 0, fromWinlink = 0, fromPane = PaneId 1 }
    , itemFormat = "", keyFormat = Nothing, command = "", filter = Nothing
    , sort = TreeSort { order = SortIndex, reversed = False }
    , preview = PreviewNormal, view = ViewPreview, expanded = Map.empty
    , tagged = [], current = Nothing, currentIx = 0, offset = 0
    , modePrompt = Nothing, search = Nothing, hidePane = Nothing
    , groups = SquashGroups, confirm = AskBeforeKill, zoom = ModeLeftZoom
    , showHelp = False, editor = Nothing
    }

-- | The lines of a state, with tmux's default keys, settled onto them.
laid :: TreeState -> ([Line], TreeState)
laid ts =
    let ls = [ l { key = defaultLineKey i } | (i, l) <- zip [0 ..] (buildLines ts items) ]
    in (ls, settle 20 ls ts)

press :: TreeState -> Text -> KeyResult
press ts k = let (ls, ts') = laid ts in treeKey 20 [SortIndex, SortName] items ls ts' k

names :: TreeState -> [Text]
names ts = map (.item.name) (fst (laid ts))

rowText :: V.Vector Cell.Cell -> Text
rowText = T.stripEnd . T.pack . map Cell.baseChar . V.toList

spec :: Spec
spec = do
    describe "buildLines" $ do
        it "flattens every expanded item depth-first" $
            names fresh `shouldBe` ["s", "0", "0", "1", "1", "0", "t", "0", "0"]
        it "hides a collapsed item's children" $
            names fresh { expanded = Map.fromList [(TagSession (SessionId 0), False)] }
                `shouldBe` ["s", "t", "0", "0"]

    describe "treeKey" $ do
        it "h collapses the selected expanded item and l expands it again" $ do
            let collapsed = (press fresh "h").state
            names collapsed `shouldBe` ["s", "t", "0", "0"]
            names (press collapsed "l").state `shouldBe` names fresh
        it "h on a leaf selects and collapses its parent" $ do
            let onPane = fresh { current = Just (TagPane (PaneId 2)) }
                r = press onPane "h"
            r.state.current `shouldBe` Just (TagWinlink (SessionId 0) 0)
            names r.state `shouldBe` ["s", "0", "1", "0", "t", "0", "0"]
        it "a line's key selects it and chooses it" $ do
            let r = press fresh "6"
            r.state.current `shouldBe` Just (TagSession (SessionId 1))
            r.unhandled `shouldBe` Just "Enter"
        it "q finishes the mode" $
            (press fresh "q").finished `shouldBe` True
        it "t tags an item, clearing its ancestors' and children's tags" $ do
            let start = fresh { current = Just (TagWinlink (SessionId 0) 0)
                              , tagged = [TagSession (SessionId 0), TagPane (PaneId 1)] }
            (press start "t").state.tagged `shouldBe` [TagWinlink (SessionId 0) 0]
        it "O moves to the next sort order and r reverses it" $ do
            (press fresh "O").state.sort.order `shouldBe` SortName
            (press fresh "r").state.sort.reversed `shouldBe` True
        it "a search selects the next match and opens its parents" $ do
            let hidden = fresh { expanded = Map.fromList [(TagSession (SessionId 1), False)]
                               , search = Just "t" }
                found = searchSet items Forward hidden
            found.current `shouldBe` Just (TagSession (SessionId 1))

    describe "item kinds" $ do
        -- Heading @h@ (untaggable) over items @a@ and @b@, whose child
        -- @c@ (untaggable) previews as @b@.
        let heading = Item
                { tag = TagSession (SessionId 9), name = "h", text = Nothing
                , children = [leafI "a", (leafI "bb") { children = [part] }]
                , startExpanded = True, align = AlignNone, searchText = []
                , fuzzyName = Nothing, tagging = Untaggable, previewFrom = OwnPreview }
            leafI n = heading
                { tag = TagWinlink (SessionId 9) (T.length n), name = n, children = []
                , tagging = Taggable }
            part = (leafI "ccc")
                { tag = TagPane (PaneId 9), tagging = Untaggable
                , previewFrom = ParentPreview }
            linesOf ts = [ l { key = defaultLineKey i } | (i, l) <- zip [0 ..] (buildLines ts [heading]) ]
            at i = fresh { current = Just ((linesOf fresh !! i).item.tag), currentIx = i }
            pressAt i k = treeKey 20 [] [heading] (linesOf fresh) (at i) k
        it "t leaves an untaggable item untagged" $
            (pressAt 0 "t").state.tagged `shouldBe` []
        it "C-t tags the items under an untaggable heading" $
            (pressAt 0 "C-t").state.tagged
                `shouldBe` [TagWinlink (SessionId 9) 1, TagWinlink (SessionId 9) 2]
        it "titles and previews a parent-previewed item as its parent" $ do
            let drawn = drawTree DrawInput
                    { size = Size { rows = 24, cols = 60 }, lines = linesOf fresh
                    , noMatches = False, theme = terminalTheme
                    , selectStyle = Cell.defaultStyle, borderStyle = Cell.defaultStyle
                    , promptStyle = Cell.defaultStyle, promptTop = False
                    , previewHeight = FixedPreview 12, sortShown = False
                    , viewName = Nothing, help = [], helpWidth = 0 } (at 3)
            fmap snd drawn.previewArea `shouldBe` Just (TagWinlink (SessionId 9) 2)
            any ("┌ bb " `T.isPrefixOf`) (map rowText (V.toList drawn.cells)) `shouldBe` True
        it "gives a fixed preview its rows and the list the rest" $ do
            listHeight (FixedPreview 12) 24 100 PreviewNormal `shouldBe` 12
            listHeight (FixedPreview 12) 24 100 PreviewOff `shouldBe` 24

    describe "drawTree" $ do
        let draw ts noMatches =
                let (ls, ts') = laid ts
                in drawTree DrawInput
                    { size = Size { rows = 24, cols = 60 }, lines = ls
                    , noMatches = noMatches, theme = terminalTheme
                    , selectStyle = Cell.defaultStyle
                    , borderStyle = styleCell terminalTheme "bg=themedarkgrey,fg=themelightgrey"
                    , promptStyle = Cell.defaultStyle, promptTop = False
                    , previewHeight = ProportionalPreview
                    , sortShown = True, viewName = Just "preview"
                    , help = [], helpWidth = 0 } ts'
        it "draws each line as key, tree branch, name and text" $ do
            let rows = map rowText (V.toList (draw fresh False).cells)
            take 3 rows `shouldBe` ["(0) - s: x", "(1) ├─> - 0: x", "(2) │   ├─> 0: x"]
        it "titles the preview box with the selection, sort and filter state" $ do
            let rows = map rowText (V.toList (draw fresh { filter = Just "f" } True).cells)
            any ("┌ s (sort: index) (view: preview) (filter: no matches) " `T.isPrefixOf`) rows
                `shouldBe` True

        it "aligns an item's text within the rest of its line" $ do
            let item = (items !! 0) { children = [], text = Just "#[align=right]R" }
                ls = [ l { key = defaultLineKey i } | (i, l) <- zip [0 ..] (buildLines fresh [item]) ]
                drawn = drawTree DrawInput
                    { size = Size { rows = 24, cols = 60 }, lines = ls
                    , noMatches = False, theme = terminalTheme
                    , selectStyle = Cell.defaultStyle, borderStyle = Cell.defaultStyle
                    , promptStyle = Cell.defaultStyle, promptTop = False
                    , previewHeight = ProportionalPreview, sortShown = False
                    , viewName = Nothing, help = [], helpWidth = 0 } fresh
            T.takeEnd 2 (T.stripEnd (rowText (V.head drawn.cells))) `shouldBe` " R"

    describe "fuzzyView" $ do
        it "keeps the items whose path fuzzily matches, under their ancestors, the best first" $ do
            let v = fuzzyView "hat" named
            tags v.kept `shouldBe`
                [ TagSession (SessionId 1), TagWinlink (SessionId 1) 1, TagPane (PaneId 4)
                , TagSession (SessionId 0), TagWinlink (SessionId 0) 0, TagPane (PaneId 1) ]
            v.best `shouldBe` Just (TagWinlink (SessionId 1) 1)
            v.opened `shouldMatchList` [TagSession (SessionId 1), TagSession (SessionId 0)]
        it "matches a query across an item's ancestors" $
            (fuzzyView "projhat" named).best `shouldBe` Just (TagWinlink (SessionId 1) 1)
        it "never matches an item without a fuzzy name" $
            tags (fuzzyView "0" named).kept `shouldBe` []
        it "keeps every item for an empty query, matching none" $ do
            let v = fuzzyView "" named
            (tags v.kept, v.best) `shouldBe` (tags named, Nothing)

    describe "shownItems" $ do
        it "lays out a live search's matches with their ancestors opened" $ do
            let closed = (searching "hat")
                    { expanded = Map.fromList [(TagSession (SessionId 1), False)] }
                (shown, lts) = shownItems closed named
            map (.item.tag) (buildLines lts shown) `shouldBe` tags (fuzzyView "hat" named).kept
        it "lays out every item while no query is typed" $
            tags (fst (shownItems (searching "") named)) `shouldBe` tags named

    describe "templateReplace" $ do
        it "puts the item in for %% with single quotes escaped" $
            templateReplace "switch-client -t '%%'" "it's" 1
                `shouldBe` "switch-client -t 'it'\\''s'"
        it "escapes for double quotes after %%%" $
            templateReplace "display \"%%%\"" "a$b" 1 `shouldBe` "display \"a\\$b\""
