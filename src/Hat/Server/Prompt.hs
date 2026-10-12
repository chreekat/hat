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
    , addHistory
    ) where

import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Model
    (CursorShape (..), PromptCursor (..), PromptKeys (..), PromptMode (..), PromptState (..))
import Hat.Model.Options (ModeKeys (..), Options (..), defaultOptions)
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
    , promptKeys = PromptKeys
        { statusKeys = KeysEmacs
        , wordSeparators = defaultOptions.wordSeparators
        , insertCursor = PromptCursor "" CursorDefault
        , commandCursor = PromptCursor "" CursorDefault }
    , promptMode = PromptInsert
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

-- | Splice a submitted line into a @command-prompt@ template (tmux's
-- @cmd_template_replace@): the first @%%@ and every @%1@ become the line,
-- escaped for the single quotes @%%@ sits in, or for the double quotes
-- @%%%@ and @%1%@ sit in. An empty template runs the line as-is.
applyTemplate :: Text -> Text -> Text
applyTemplate tmpl line
    | T.null tmpl = line
    | otherwise = go False tmpl
  where
    go replaced t = case T.uncons t of
        Nothing -> ""
        Just ('%', r)
            | Just r1 <- T.stripPrefix "1" r -> splice replaced r1
            | not replaced, Just r1 <- T.stripPrefix "%" r -> spliceSingle r1
        Just (c, r) -> T.cons c (go replaced r)
    splice replaced r = case T.stripPrefix "%" r of
        Just r' -> T.concatMap doubleQuoted line <> go replaced r'
        Nothing -> line <> go replaced r
    spliceSingle r = case T.stripPrefix "%" r of
        Just r' -> T.concatMap doubleQuoted line <> go True r'
        Nothing -> T.concatMap singleQuoted line <> go True r
    singleQuoted '\'' = "'\\''"
    singleQuoted c = T.singleton c
    doubleQuoted c
        | c `elem` ("\"\\$;~" :: String) = T.pack ['\\', c]
        | otherwise = T.singleton c

-- | Apply one key to the prompt, read as its @status-keys@ say. @history@
-- is the command history, most-recent first, used by the Up/Down keys.
editPrompt :: [Text] -> PromptState -> Key -> PromptEdit
editPrompt history st key = case st.promptKeys.statusKeys of
    KeysEmacs -> emacsKey history st key
    KeysVi -> case st.promptMode of
        PromptInsert -> viInsertKey history st key
        PromptCommand -> viCommandKey history st key

-- | A vi-keys prompt in insert mode (tmux's @prompt_translate_key@):
-- Escape enters command mode, the line-editing keys they share act as in
-- emacs mode, and any other key inserts itself if it is a character.
viInsertKey :: [Text] -> PromptState -> Key -> PromptEdit
viInsertKey history st key
    | key.name `elem` ["Escape", "C-["] = Editing
        (moveTo (st.cursor - 1) st) { promptMode = PromptCommand }
    | key.name `elem` shared = emacsKey history st key
    | otherwise = Editing (maybe st (`insert` st) (insertText key))
  where
    shared =
        [ "C-a", "C-c", "C-e", "C-g", "C-h", "Tab", "C-k", "C-n", "C-p"
        , "C-t", "C-u", "C-v", "C-w", "C-y", "Enter", "C-Left", "C-Right"
        , "BSpace", "Delete", "Down", "End", "Home", "Left", "Right", "Up" ]

-- | A vi-keys prompt in command mode (tmux's @prompt_translate_key@).
viCommandKey :: [Text] -> PromptState -> Key -> PromptEdit
viCommandKey history st key = case key.name of
    "BSpace" -> as "Left"
    "A" -> inserting (as "End")
    "I" -> inserting (as "Home")
    "C" -> inserting (as "C-k")
    "s" -> inserting (as "Delete")
    "a" -> inserting (as "Right")
    "S" -> inserting (Editing (clearLine st))
    "i" -> inserting (Editing st)
    "$" -> as "End"
    "0" -> as "Home"
    "^" -> as "Home"
    "D" -> as "C-k"
    "X" -> as "BSpace"
    "b" -> move (backwardWordTmux seps)
    "B" -> move (backwardWordTmux "")
    "d" -> Editing (clearLine st)
    "e" -> move (endWordTmux seps)
    "E" -> move (endWordTmux "")
    "w" -> move (forwardWordVi seps)
    "W" -> move (forwardWordVi "")
    "p" -> as "C-y"
    "q" -> Cancel
    "x" -> as "Delete"
    "Delete" -> as "Delete"
    "j" -> as "Down"
    "Down" -> as "Down"
    "h" -> as "Left"
    "Left" -> as "Left"
    "l" -> as "Right"
    "Right" -> as "Right"
    "k" -> as "Up"
    "Up" -> as "Up"
    "C-h" -> as "C-h"
    "C-c" -> as "C-c"
    "Enter" -> as "Enter"
    _ -> Editing st
  where
    seps = st.promptKeys.wordSeparators
    as name = emacsKey history st key { name = name }
    move f = Editing (moveTo (f st.input st.cursor) st)
    inserting = \case
        Editing st' -> Editing st' { promptMode = PromptInsert }
        done -> done

-- | tmux's @C-u@: empty the line.
clearLine :: PromptState -> PromptState
clearLine st = st { input = "", cursor = 0 }

-- | The emacs-keys prompt (and the keys vi mode shares with it).
emacsKey :: [Text] -> PromptState -> Key -> PromptEdit
emacsKey _history st key = case key.name of
    "Enter"  -> Submit st.input
    "Escape" -> Cancel
    "C-c"    -> Cancel
    "C-g"    -> Cancel
    "BSpace" -> Editing (backspace st)
    "C-h"    -> Editing (backspace st)
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
-- first, keeping at most @limit@ lines. Blank lines are dropped.
pushHistory :: Int -> Text -> [Text] -> [Text]
pushHistory limit line history
    | T.null (T.strip line) = history
    | otherwise = addHistory limit line history

-- | tmux's @prompt_add_history@: the line becomes the newest entry unless it
-- repeats the newest already, and only the newest @limit@ are kept.
addHistory :: Int -> Text -> [Text] -> [Text]
addHistory limit line history = take limit $ case history of
    recent : _ | recent == line -> history
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

-- | tmux's @prompt_forward_word@ in vi mode: past the rest of this word
-- (a run of separators or of other characters) and any spaces after it.
forwardWordVi :: Text -> Text -> Int -> Int
forwardWordVi seps s i0
    | i0 >= n = n
    | otherwise = go (i0 + 1)
  where
    n = T.length s
    cls = isSep seps (T.index s i0) && T.index s i0 /= ' '
    go i
        | i < n && T.index s i == ' ' = skipSpaces i
        | i < n && isSep seps (T.index s i) == cls = go (i + 1)
        | otherwise = i
    skipSpaces i
        | i < n && T.index s i == ' ' = skipSpaces (i + 1)
        | otherwise = i

-- | tmux's @prompt_end_word@: to the last character of the next word.
endWordTmux :: Text -> Text -> Int -> Int
endWordTmux seps s i0
    | i0 >= n = i0
    | otherwise = case firstWord (i0 + 1) of
        Nothing -> n
        Just j -> runEnd (isSep seps (T.index s j)) (j + 1) - 1
  where
    n = T.length s
    firstWord i
        | i >= n = Nothing
        | T.index s i == ' ' = firstWord (i + 1)
        | otherwise = Just i
    runEnd cls i
        | i < n, T.index s i /= ' ', isSep seps (T.index s i) == cls = runEnd cls (i + 1)
        | otherwise = i

-- | tmux's @prompt_backward_word@: to the start of this or the previous
-- word.
backwardWordTmux :: Text -> Text -> Int -> Int
backwardWordTmux seps s i0 = case nonSpace i0 of
    Nothing -> 0
    Just j -> start (isSep seps (T.index s j)) j
  where
    nonSpace i
        | i <= 0 = Nothing
        | T.index s (i - 1) /= ' ' = Just (i - 1)
        | i - 1 == 0 = Just 0
        | otherwise = nonSpace (i - 1)
    start cls j
        | j <= 0 = 0
        | T.index s (j - 1) == ' ' || isSep seps (T.index s (j - 1)) /= cls = j
        | otherwise = start cls (j - 1)

isSep :: Text -> Char -> Bool
isSep seps c = T.elem c seps

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
