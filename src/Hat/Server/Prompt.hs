-- | The command prompt's line editor: a pure state machine mapping one
-- key at a time onto an edit of the prompt buffer. The server owns the
-- 'PromptState' (per client) and feeds keys here; rendering and command
-- execution live in "Hat.Server".
module Hat.Server.Prompt
    ( PromptEdit (..)
    , emptyPrompt
    , editPrompt
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Hat.Model (PromptState (..))
import Hat.Server.Keys (Key (..))

-- | What one key does to the prompt.
data PromptEdit
    = Editing PromptState  -- ^ keep the prompt open with this new state
    | Submit Text          -- ^ run this line and close the prompt
    | Cancel               -- ^ close the prompt, running nothing
    deriving (Eq, Show)

-- | A fresh, empty prompt.
emptyPrompt :: PromptState
emptyPrompt = PromptState
    { input = ""
    , cursor = 0
    , histIx = Nothing
    , pending = ""
    }

-- | Apply one key to the prompt. @history@ is the command history,
-- most-recent first, used by the Up/Down keys.
editPrompt :: [Text] -> PromptState -> Key -> PromptEdit
editPrompt _history st key = case key.name of
    "Enter"  -> Submit st.input
    "Escape" -> Cancel
    "C-c"    -> Cancel
    "C-g"    -> Cancel
    "BSpace" -> Editing (backspace st)
    "Delete" -> Editing (deleteAt st)
    "C-d"    -> Editing (deleteAt st)
    "Left"   -> Editing (moveTo (st.cursor - 1) st)
    "Right"  -> Editing (moveTo (st.cursor + 1) st)
    "Home"   -> Editing (moveTo 0 st)
    "C-a"    -> Editing (moveTo 0 st)
    "End"    -> Editing (moveTo (T.length st.input) st)
    "C-e"    -> Editing (moveTo (T.length st.input) st)
    _        -> case insertText key of
        Just t  -> Editing (insert t st)
        Nothing -> Editing st  -- unhandled key: swallowed, no change

-- | Clamp the cursor to @0..length@.
moveTo :: Int -> PromptState -> PromptState
moveTo n st = st { cursor = max 0 (min (T.length st.input) n) }

-- | Delete the character at the cursor (forward delete).
deleteAt :: PromptState -> PromptState
deleteAt st
    | st.cursor >= T.length st.input = st
    | otherwise = st { input = before <> T.drop 1 after }
  where
    (before, after) = T.splitAt st.cursor st.input

-- | The text a key inserts, if it is a self-inserting character. Named
-- keys (Up, Enter, C-a, …) insert nothing.
insertText :: Key -> Maybe Text
insertText key
    | key.name == "Space" = Just " "
    | T.length key.name == 1, c <- T.head key.name, c >= ' ' = Just key.name
    | otherwise = Nothing

insert :: Text -> PromptState -> PromptState
insert t st = st
    { input = before <> t <> after
    , cursor = st.cursor + T.length t
    }
  where
    (before, after) = T.splitAt st.cursor st.input

backspace :: PromptState -> PromptState
backspace st
    | st.cursor <= 0 = st
    | otherwise = st
        { input = T.dropEnd 1 before <> after
        , cursor = st.cursor - 1
        }
  where
    (before, after) = T.splitAt st.cursor st.input
