module Hat.Server.PickerSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Hat.Geometry (Rect (..), Size (..))
import Hat.Model
    ( PaneId (..), WindowId (..), SessionId (..), Expansion (..), PickerFill (..)
    , PickerMode (..), PickerNode (..), PickerState (..), PreviewTarget (..) )
import Hat.Server.Keys (Key, parseKeyName)
import Hat.Server.Picker

-- Build the key a name denotes (all names here are valid).
key :: T.Text -> Key
key n = maybe (error ("bad key: " <> T.unpack n)) id (parseKeyName n)

-- A collapsible parent node, initially expanded.
node :: T.Text -> T.Text -> [PickerNode] -> PickerNode
node lbl cmd kids = PickerNode lbl cmd Nothing kids Expanded

demo :: PickerState
demo = PickerState
    { title = "tree"
    , roots =
        [ node "work" "switch-client -t work"
            [ leaf "0:editor" "select-window -t work:0"
            , leaf "1:shell"  "select-window -t work:1" ]
        , node "play" "switch-client -t play"
            [ leaf "0:game" "select-window -t play:0" ]
        ]
    , cursor = 0
    , query = ""
    , search = ""
    , mode = Browsing
    , fill = PaneRegion
    }

stay :: PickerState -> Key -> PickerState
stay p k = case editPicker p k of
    PickerStay p' -> p'
    _             -> error "expected the picker to stay open"

labels :: PickerState -> [T.Text]
labels = map ((.label) . (.node)) . visibleRows

spec :: Spec
spec = do
    describe "visibleRows" $ do
        it "flattens the fully expanded tree depth-first" $
            labels demo `shouldBe` ["work", "0:editor", "1:shell", "play", "0:game"]

        it "records the depth of each row" $
            map (.depth) (visibleRows demo) `shouldBe` [0, 1, 1, 0, 1]

    describe "editPicker (menu mode)" $ do
        it "moves the cursor down with j, clamped to the last row" $ do
            let p = iterate (`stay` key "j") demo !! 9
            p.cursor `shouldBe` 4

        it "collapses the node under the cursor with h, hiding its children" $
            labels (stay demo (key "h")) `shouldBe` ["work", "play", "0:game"]

        it "re-expands a collapsed node with l" $ do
            let collapsed = stay demo (key "h")
            labels (stay collapsed (key "l")) `shouldBe` labels demo

        it "toggles the node under the cursor with O" $
            labels (stay demo (key "O")) `shouldBe` ["work", "play", "0:game"]

        it "runs a leaf's command on Enter" $
            editPicker demo { cursor = 2 } (key "Enter")
                `shouldBe` PickerRun "select-window -t work:1"

        it "runs a parent's own command on Enter" $
            editPicker demo (key "Enter")
                `shouldBe` PickerRun "switch-client -t work"

        it "cancels on q and Escape" $ do
            editPicker demo (key "q") `shouldBe` PickerCancel
            editPicker demo (key "Escape") `shouldBe` PickerCancel

        it "enters search mode on /" $
            (stay demo (key "/")).mode `shouldBe` Searching

    describe "search mode" $ do
        let searching = demo { mode = Searching }
        it "filters to matches and keeps their ancestors" $ do
            let p = foldl stay searching (map key ["s", "h", "e", "l", "l"])
            p.query `shouldBe` "shell"
            labels p `shouldBe` ["work", "1:shell"]

        -- Enter while typing a search commits it: the filter clears so the
        -- full tree shows again, with the cursor on the first match, rather
        -- than immediately activating it; activation takes a deliberate
        -- second Enter.
        it "commits the search on Enter: clears the filter, cursor on the first match" $ do
            let p = foldl stay searching (map key ["s", "h", "e", "l", "l"])
                committed = stay p (key "Enter")
            committed.mode `shouldBe` Browsing
            committed.query `shouldBe` ""
            labels committed `shouldBe` labels demo
            committed.cursor `shouldBe` 2

        it "runs the match only on a second Enter, now in menu mode" $ do
            let p = foldl stay searching (map key ["s", "h", "e", "l", "l"])
                committed = stay p (key "Enter")
            editPicker committed (key "Enter")
                `shouldBe` PickerRun "select-window -t work:1"

        it "reveals a first match hidden inside a collapsed subtree" $ do
            let collapsed = stay demo { cursor = 3 } (key "h")  -- collapse "play"
                p = foldl stay (stay collapsed (key "/")) (map key ["g", "a", "m", "e"])
                committed = stay p (key "Enter")
            labels committed `shouldBe` labels demo
            committed.cursor `shouldBe` 4

        it "clears the filter on Enter even with no matches" $ do
            let p = foldl stay searching (map key ["z", "z"])
                committed = stay p (key "Enter")
            committed.mode `shouldBe` Browsing
            committed.query `shouldBe` ""
            labels committed `shouldBe` labels demo
            committed.cursor `shouldBe` 0

        it "types n and b into the query instead of navigating" $ do
            let p = foldl stay searching (map key ["n", "b"])
            p.query `shouldBe` "nb"
            p.mode `shouldBe` Searching

        it "keeps the cursor on the best match while typing, not at the top" $ do
            -- "game" drops the whole "work" subtree, so the match sits under
            -- "play"; the highlight must follow it, not stick to the top row.
            let p = foldl stay searching (map key ["g", "a", "m", "e"])
            labels p `shouldBe` ["play", "0:game"]
            p.cursor `shouldBe` 1

        it "shows a matched session's windows, never collapsing past them" $ do
            -- matching a session by name reveals its windows (the tree never
            -- collapses shallower than the window level).
            let p = foldl stay searching (map key ["w", "o", "r", "k"])
            labels p `shouldBe` ["work", "0:editor", "1:shell"]

        it "shows panes for context but never matches them" $ do
            -- panes display under a matched window, yet a query aimed at a
            -- pane's generic label finds nothing (they are not targets).
            let paneRow lbl = PickerNode lbl "c"
                    (Just (PreviewPane (PaneId 0))) [] Collapsed
                win = PickerNode "1:hat" "c" (Just (PreviewWindow (WindowId 0)))
                    [ paneRow "pane 0", paneRow "pane 1" ] Expanded
                sess = PickerNode "projects" "c"
                    (Just (PreviewSession (SessionId 0))) [win] Expanded
                tree = demo { mode = Searching, roots = [sess] }
                onHat = foldl stay tree (map key ["h", "a", "t"])
                onPane = foldl stay tree (map key ["p", "a", "n", "e"])
            labels onHat `shouldBe` ["projects", "1:hat", "pane 0", "pane 1"]
            labels onPane `shouldBe` []

    describe "menu-mode search stepping (n/b after committing a search)" $ do
        -- Committing a search remembers its term; back in menu mode n/b then
        -- step the cursor between the whole tree's matching rows, revealing
        -- collapsed subtrees and wrapping. "0:" matches "0:editor" (row 1)
        -- and "0:game" (row 4); Enter lands on the first, n advances.
        let committedOn q =
                let typed = foldl stay (stay demo (key "/")) (map key q)
                in stay typed (key "Enter")

        it "steps to the next match with n after the search is committed" $ do
            let committed = committedOn ["0", ":"]
            committed.mode `shouldBe` Browsing
            committed.cursor `shouldBe` 1        -- 0:editor
            let next = stay committed (key "n")
            next.cursor `shouldBe` 4             -- 0:game
            (stay next (key "n")).cursor `shouldBe` 1  -- wraps to the first

        it "steps to the previous match with b, wrapping" $ do
            let committed = committedOn ["0", ":"]
            let prev = stay committed (key "b")
            prev.cursor `shouldBe` 4             -- wraps back to the last match
            (stay prev (key "b")).cursor `shouldBe` 1

        it "reveals a match hidden in a collapsed subtree when stepping" $ do
            let collapsed = stay demo { cursor = 3 } (key "h")   -- collapse "play"
                typed = foldl stay (stay collapsed (key "/")) (map key ["0", ":"])
                committed = stay typed (key "Enter")
            committed.cursor `shouldBe` 1
            labels committed `shouldBe` ["work", "0:editor", "1:shell", "play"]
            let next = stay committed (key "n")
            labels next `shouldBe` ["work", "0:editor", "1:shell", "play", "0:game"]
            next.cursor `shouldBe` 4

        it "does nothing on n/b with no committed search" $ do
            (stay demo (key "n")).cursor `shouldBe` demo.cursor
            (stay demo (key "b")).cursor `shouldBe` demo.cursor

    describe "fuzzy path search" $ do
        -- A query can span the session and window names: "projhat" reaches
        -- the "hat" window inside the "projects" session (bug ef).
        let tree = demo
                { roots =
                    [ node "projects" "switch-client -t projects"
                        [ leaf "hat" "select-window -t projects:0"
                        , leaf "editor" "select-window -t projects:1" ]
                    , node "misc" "switch-client -t misc"
                        [ leaf "notes" "select-window -t misc:0" ] ]
                , query = "projhat" }
        it "matches a window by a query spanning its session and window names" $
            labels tree `shouldBe` ["projects", "hat"]

        it "still matches a plain contiguous substring within a label" $
            labels (tree { query = "edit" }) `shouldBe` ["projects", "editor"]

        it "matches non-contiguous characters in order within a label" $
            labels (tree { query = "etr" }) `shouldBe` ["projects", "editor"]

        it "lands the cursor on the highest-scoring match, not the first" $ do
            -- both leaves contain "b"; "x back" matches at a word boundary and
            -- so outscores the mid-word "xback", even though it comes second.
            let flat = demo { roots = [ leaf "xback" "c1", leaf "x back" "c2" ] }
                typed = foldl stay (stay flat (key "/")) [key "b"]
                committed = stay typed (key "Enter")
            committed.cursor `shouldBe` 1

    describe "selectedPreview" $ do
        let previewTree = demo
                { roots =
                    [ (node "work" "switch-client -t work"
                        [ (leaf "0:editor" "c")
                            { preview = Just (PreviewPane (PaneId 7)) } ])
                        { preview = Just (PreviewWindow (WindowId 3)) } ] }
        it "previews the target of the row under the cursor" $ do
            -- the parent row previews its whole window (splits and all);
            -- the child row previews just its pane.
            selectedPreview previewTree { cursor = 0 }
                `shouldBe` Just (PreviewWindow (WindowId 3))
            selectedPreview previewTree { cursor = 1 }
                `shouldBe` Just (PreviewPane (PaneId 7))

    describe "pickerSplit" $ do
        it "gives the list a third of a wide overlay" $
            pickerSplit 90 `shouldBe` Just 30
        it "declines to split a narrow overlay" $
            pickerSplit 30 `shouldBe` Nothing

    describe "stackThumbnails" $ do
        it "splits the height evenly, a label row above each body" $
            -- two windows in 20 rows: 10 each, 1 label + 9 body.
            stackThumbnails 20 2 `shouldBe` [(0, 1, 9), (10, 11, 9)]

        it "packs more windows into thinner blocks" $
            -- eight windows in 20 rows: 2 each, 1 label + 1 body.
            stackThumbnails 20 8
                `shouldBe` [ (r, r + 1, 1) | r <- [0, 2 .. 14] ]

        it "drops windows that cannot fit two rows each" $
            -- five windows but only room for one block.
            stackThumbnails 3 5 `shouldBe` [(0, 1, 2)]

        it "shows nothing when there is no room or no windows" $ do
            stackThumbnails 1 4 `shouldBe` []
            stackThumbnails 20 0 `shouldBe` []

    describe "pickerRegion" $ do
        let csize = Size { rows = 24, cols = 80 }
            paneRect = Rect { startRow = 0, endRow = 23, startCol = 41, endCol = 80 }
        it "uses the active pane's rect when not zoomed" $
            pickerRegion PaneRegion csize 0 (Just paneRect) `shouldBe` paneRect
        it "fills the content area (minus the status row) when zoomed" $
            pickerRegion FillWindow csize 0 (Just paneRect)
                `shouldBe` Rect { startRow = 0, endRow = 23, startCol = 0, endCol = 80 }
        it "falls back to the content area with no active pane" $
            pickerRegion PaneRegion csize 1 Nothing
                `shouldBe` Rect { startRow = 1, endRow = 24, startCol = 0, endCol = 80 }

    describe "windowChildren" $ do
        it "collapses a single-pane window to no pane rows" $
            windowChildren [leaf "pane 0*" "select-pane -t 0"] `shouldBe` []

        it "keeps the pane rows of a multi-pane window" $ do
            let panes = [ leaf "pane 0*" "select-pane -t 0"
                        , leaf "pane 1" "select-pane -t 1" ]
            windowChildren panes `shouldBe` panes

    describe "pickerLines" $
        it "shows the title, marks the cursor row, and draws arrows" $ do
            let ls = pickerLines 10 demo { cursor = 1 }
            map snd (take 1 ls) `shouldBe` ["tree"]
            [ t | (SelectedRow, t) <- ls ] `shouldBe` ["    0:editor"]
            [ t | (_, t) <- ls, "\x25be" `T.isInfixOf` t ] `shouldBe` ["\x25be work", "\x25be play"]
