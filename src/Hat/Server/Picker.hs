-- | The chooser overlay's pure logic: filtering items by the query,
-- mapping one key at a time onto navigation/search edits, and laying out
-- the visible rows. The server owns the 'PickerState' (per client),
-- feeds keys here, and runs the chosen item's command.
module Hat.Server.Picker
    ( PickerEdit (..)
    , visibleItems
    , editPicker
    , pickerLines
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Hat.Model (PickerItem (..), PickerState (..))
import Hat.Server.Keys (Key (..))

-- | What one key does to an open picker.
data PickerEdit
    = PickerStay PickerState  -- ^ keep it open with this new state
    | PickerRun Text          -- ^ run this command line and close
    | PickerCancel            -- ^ close, running nothing
    deriving (Eq, Show)

-- | The items matching the current query (case-insensitive substring).
visibleItems :: PickerState -> [PickerItem]
visibleItems p
    | T.null p.query = p.items
    | otherwise =
        filter (T.isInfixOf (T.toLower p.query) . T.toLower . (.label)) p.items

-- | Apply one key. In menu mode @j@/@k@ navigate and @/@ enters search;
-- in search mode keys type into the query. Enter runs the selection.
editPicker :: PickerState -> Key -> PickerEdit
editPicker p key
    | p.searching = searchKey
    | otherwise = menuKey
  where
    n = length (visibleItems p)
    clampC c = max 0 (min (max 0 (n - 1)) c)
    up = PickerStay p { cursor = clampC (p.cursor - 1) }
    down = PickerStay p { cursor = clampC (p.cursor + 1) }
    runSel = case drop p.cursor (visibleItems p) of
        (it : _) -> PickerRun it.command
        []       -> PickerCancel
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
        "g"      -> PickerStay p { cursor = 0 }
        "G"      -> PickerStay p { cursor = max 0 (n - 1) }
        "/"      -> PickerStay p { searching = True }
        _        -> PickerStay p
    searchKey = case key.name of
        "Enter"  -> runSel
        "Escape" -> PickerStay p { searching = False, query = "", cursor = 0 }
        "Up"     -> up
        "C-p"    -> up
        "Down"   -> down
        "C-n"    -> down
        "BSpace" -> PickerStay (reQuery (T.dropEnd 1 p.query))
        _        -> case insertText key of
            Just t  -> PickerStay (reQuery (p.query <> t))
            Nothing -> PickerStay p
    reQuery q = p { query = q, cursor = 0 }

-- | The text a self-inserting key contributes to the query.
insertText :: Key -> Maybe Text
insertText key
    | key.name == "Space" = Just " "
    | T.length key.name == 1, c <- T.head key.name, c >= ' ' = Just key.name
    | otherwise = Nothing

-- | Render the picker into at most @height@ lines: a title/search line
-- then the visible items, scrolled to keep the cursor in view and marked
-- with @▸@.
pickerLines :: Int -> PickerState -> [Text]
pickerLines height p = take height (titleLine : itemLines)
  where
    vis = visibleItems p
    titleLine = p.title <> (if p.searching then "  /" <> p.query else "")
    bodyH = max 1 (height - 1)
    start = max 0 (min (length vis - bodyH) (p.cursor - bodyH `div` 2))
    windowed = take bodyH (drop start vis)
    itemLines =
        [ (if i == p.cursor then "\x25b8 " else "  ") <> it.label
        | (i, it) <- zip [start ..] windowed ]
