module Hat.Server.ModeTreeSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Hspec

import Hat.Geometry (Size (..))
import Hat.Model
import Hat.Server.Command.Types (templateReplace)
import Hat.Server.ModeTree
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
        , align = AlignNone, searchText = [n] }
    leaf t n = node t n []

fresh :: TreeState
fresh = TreeState
    { kind = SessionTree
    , origin = TreeOrigin { fromSession = SessionId 0, fromWinlink = 0, fromPane = PaneId 1 }
    , format = "", keyFormat = Nothing, command = "", filter = Nothing
    , sort = TreeSort { order = SortIndex, reversed = False }
    , preview = PreviewNormal, view = ViewPreview, expanded = Map.empty
    , tagged = [], current = Nothing, currentIx = 0, offset = 0
    , modePrompt = Nothing, search = Nothing, hidePane = Nothing
    , groups = SquashGroups, confirm = AskBeforeKill, zoom = ModeLeftZoom
    , showHelp = False
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

    describe "drawTree" $ do
        let draw ts noMatches =
                let (ls, ts') = laid ts
                in drawTree DrawInput
                    { size = Size { rows = 24, cols = 60 }, lines = ls
                    , noMatches = noMatches, selectStyle = Cell.defaultStyle
                    , promptStyle = Cell.defaultStyle, promptTop = False
                    , sortShown = True, viewName = Just "preview"
                    , help = HelpText "item" 0 [] } ts'
        it "draws each line as key, tree branch, name and text" $ do
            let rows = map rowText (V.toList (draw fresh False).cells)
            take 3 rows `shouldBe` ["(0) - s: x", "(1) ├─> - 0: x", "(2) │   ├─> 0: x"]
        it "titles the preview box with the selection, sort and filter state" $ do
            let rows = map rowText (V.toList (draw fresh { filter = Just "f" } True).cells)
            any ("┌ s (sort: index) (view: preview) (filter: no matches) " `T.isPrefixOf`) rows
                `shouldBe` True

    describe "templateReplace" $ do
        it "puts the item in for %% with single quotes escaped" $
            templateReplace "switch-client -t '%%'" "it's" 1
                `shouldBe` "switch-client -t 'it'\\''s'"
        it "escapes for double quotes after %%%" $
            templateReplace "display \"%%%\"" "a$b" 1 `shouldBe` "display \"a\\$b\""
