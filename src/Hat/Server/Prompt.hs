-- | The command prompt's line editor: a pure state machine mapping one
-- key at a time onto an edit of the prompt buffer. The server owns the
-- 'PromptState' (per client) and feeds keys here; rendering and command
-- execution live in "Hat.Server".
module Hat.Server.Prompt
    ( PromptEdit (..)
    , emptyPrompt
    , promptFor
    , applyTemplate
    , editPrompt
    , pushHistory
    ) where

import Data.Char (isAlphaNum, isSpace)
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

-- | A fresh, empty prompt: the bare @:@ command line.
emptyPrompt :: PromptState
emptyPrompt = PromptState
    { input = ""
    , cursor = 0
    , histIx = Nothing
    , pending = ""
    , template = ""
    , promptLabel = ":"
    , killed = ""
    }

-- | A prompt pre-filled with @initial@ and shown behind @prefix@. On
-- submit the typed line is spliced into @tmpl@ (see 'applyTemplate').
-- Backs @command-prompt -I initial tmpl@, e.g. the @,@ rename binding.
promptFor :: Text -> Text -> Text -> PromptState
promptFor label initial tmpl = emptyPrompt
    { input = initial
    , cursor = T.length initial
    , template = tmpl
    , promptLabel = label
    }

-- | Splice a submitted line into a @command-prompt@ template, replacing
-- every @%%@ or @%1@ with the line. An empty template runs the line as-is.
applyTemplate :: Text -> Text -> Text
applyTemplate tmpl line
    | T.null tmpl = line
    | otherwise = T.replace "%1" line (T.replace "%%" line tmpl)

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
    "C-b"    -> Editing (moveTo (st.cursor - 1) st)
    "Right"  -> Editing (moveTo (st.cursor + 1) st)
    "C-f"    -> Editing (moveTo (st.cursor + 1) st)
    "Home"   -> Editing (moveTo 0 st)
    "C-a"    -> Editing (moveTo 0 st)
    "End"    -> Editing (moveTo (T.length st.input) st)
    "C-e"    -> Editing (moveTo (T.length st.input) st)
    "M-b"    -> Editing (moveTo (backwardWordIx st.input st.cursor) st)
    "M-f"    -> Editing (moveTo (forwardWordIx st.input st.cursor) st)
    "C-k"    -> Editing (killToEnd st)
    "C-u"    -> Editing (killToStart st)
    "C-w"    -> Editing (killWordBack st)
    "M-d"    -> Editing (killWordForward st)
    "C-y"    -> Editing (insert st.killed st)
    "Up"     -> Editing (historyBack _history st)
    "Down"   -> Editing (historyForward _history st)
    _        -> case insertText key of
        Just t  -> Editing (insert t st)
        Nothing -> Editing st  -- unhandled key: swallowed, no change

-- | Recall an older command. On first step the in-progress line is
-- stashed in 'pending' so 'historyForward' can restore it.
historyBack :: [Text] -> PromptState -> PromptState
historyBack history st = case history !? next of
    Nothing -> st  -- already at the oldest (or empty history)
    Just line -> setLine line st
        { histIx = Just next
        , pending = maybe st.input (const st.pending) st.histIx
        }
  where
    next = maybe 0 (+ 1) st.histIx

-- | Step back toward the newest command, then to the stashed line.
historyForward :: [Text] -> PromptState -> PromptState
historyForward history st = case st.histIx of
    Nothing -> st  -- not browsing
    Just 0  -> setLine st.pending st { histIx = Nothing }
    Just n  -> case history !? (n - 1) of
        Just line -> setLine line st { histIx = Just (n - 1) }
        Nothing -> st

-- | Replace the buffer and drop the cursor at its end.
setLine :: Text -> PromptState -> PromptState
setLine line st = st { input = line, cursor = T.length line }

(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n < 0 = Nothing
    | otherwise = case drop n xs of
        (x : _) -> Just x
        []      -> Nothing

-- | Add a submitted line to the front of the history, most-recent
-- first. Blank lines and consecutive duplicates are dropped.
pushHistory :: Text -> [Text] -> [Text]
pushHistory line history
    | T.null (T.strip line) = history
    | otherwise = case history of
        (recent : _) | recent == line -> history
        _ -> line : history

-- | Clamp the cursor to @0..length@.
moveTo :: Int -> PromptState -> PromptState
moveTo n st = st { cursor = max 0 (min (T.length st.input) n) }

-- | The index reached by moving forward over one word: skip any leading
-- non-word characters, then the word characters (alphanumeric) themselves.
forwardWordIx :: Text -> Int -> Int
forwardWordIx s i = skip isAlphaNum (skip (not . isAlphaNum) i)
  where
    n = T.length s
    skip p k
        | k < n, p (T.index s k) = skip p (k + 1)
        | otherwise = k

-- | The index reached by moving backward over one word.
backwardWordIx :: Text -> Int -> Int
backwardWordIx s i = skipBack isAlphaNum (skipBack (not . isAlphaNum) i)
  where
    skipBack p k
        | k > 0, p (T.index s (k - 1)) = skipBack p (k - 1)
        | otherwise = k

-- | The start of the previous whitespace-delimited word (readline @C-w@).
backwardWSWordIx :: Text -> Int -> Int
backwardWSWordIx s i = skipBack (not . isSpace) (skipBack isSpace i)
  where
    skipBack p k
        | k > 0, p (T.index s (k - 1)) = skipBack p (k - 1)
        | otherwise = k

-- | Delete from the cursor to the end of the line, saving the kill.
killToEnd :: PromptState -> PromptState
killToEnd st
    | st.cursor >= T.length st.input = st
    | otherwise = st { input = before, killed = after }
  where
    (before, after) = T.splitAt st.cursor st.input

-- | Delete from the start of the line to the cursor, saving the kill.
killToStart :: PromptState -> PromptState
killToStart st
    | st.cursor <= 0 = st
    | otherwise = st { input = after, killed = before, cursor = 0 }
  where
    (before, after) = T.splitAt st.cursor st.input

-- | Kill from the previous word boundary to the cursor.
killWordBack :: PromptState -> PromptState
killWordBack st
    | j >= st.cursor = st
    | otherwise = st
        { input = T.take j st.input <> T.drop st.cursor st.input
        , killed = T.take (st.cursor - j) (T.drop j st.input)
        , cursor = j
        }
  where
    j = backwardWSWordIx st.input st.cursor

-- | Kill from the cursor to the next word boundary.
killWordForward :: PromptState -> PromptState
killWordForward st
    | j <= st.cursor = st
    | otherwise = st
        { input = before <> T.drop (j - st.cursor) after
        , killed = T.take (j - st.cursor) after
        }
  where
    j = forwardWordIx st.input st.cursor
    (before, after) = T.splitAt st.cursor st.input

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
