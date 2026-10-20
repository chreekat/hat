-- | The command prompt's line editor: a pure state machine mapping one
-- key at a time onto an edit of the prompt buffer. The server owns the
-- 'PromptState' (per client) and feeds keys here; rendering and command
-- execution live in "Hat.Server".
module Hat.Server.Prompt
    ( PromptEdit (..)
    , PromptEnv (..)
    , noPromptEnv
    , promptTypeName
    , parsePromptType
    , promptFlagsText
    , emptyPrompt
    , promptFor
    , PromptAnswered (..)
    , answerPrompt
    , applyTemplate
    , templateReplace
    , editPrompt
    , pushHistory
    , addHistory
    ) where

import Data.Char (chr, digitToInt, isAlphaNum, isDigit, isSpace, ord, toUpper)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Model
    ( CursorShape (..), PromptCursor (..), PromptFreeze (..), PromptKeys (..), PromptKind (..)
    , PromptMode (..), PromptState (..), PromptType (..) )
import Hat.Model.Options (ModeKeys (..), Options (..), defaultOptions)
import Hat.Server.Keys (Key (..))

-- | What one key does to the prompt.
data PromptEdit
    = Editing PromptState  -- ^ keep the prompt open with this new state
    | Submit Text          -- ^ run this line and close the prompt
    | Cancel               -- ^ close the prompt, running nothing
    | SubmitPassing Text
        -- ^ run this line and close the prompt, the key then handled as if
        --   no prompt were open (a @-N@ prompt's non-digit)
    | Answering PromptState Text
        -- ^ keep the prompt open with this new state, running this answer
        --   now (an incremental prompt's edit)
    deriving (Eq, Show)

-- | What a prompt's keys draw on: its type's history, most-recent first,
-- and the command names @Tab@ completes.
data PromptEnv = PromptEnv
    { history :: [Text]
    , commands :: [Text]
    }

-- | No history and nothing to complete.
noPromptEnv :: PromptEnv
noPromptEnv = PromptEnv { history = [], commands = [] }

-- | A prompt type's name (tmux's @prompt_type_string@).
promptTypeName :: PromptType -> Text
promptTypeName = \case
    CommandPrompt -> "command"
    SearchPrompt -> "search"

-- | A prompt type by its name (tmux's @prompt_type@).
parsePromptType :: Text -> Maybe PromptType
parsePromptType t = lookup t [ (promptTypeName p, p) | p <- [minBound .. maxBound] ]

-- | @#{prompt_flags}@ (tmux's @prompt_flags_to_string@).
promptFlagsText :: Bool -> PromptState -> Text
promptFlagsText inPane pr = T.intercalate "," $ concat
    [ [ "SINGLE" | pr.promptKind == SingleEntry ]
    , [ "NUMERIC" | pr.promptKind == NumericEntry ]
    , [ "INCREMENTAL" | pr.promptKind == IncrementalEntry ]
    , [ "KEY" | pr.promptKind == KeyEntry ]
    , [ "QUOTENEXT" | pr.quoteNext ]
    , [ "BSPACE_EXIT" | pr.promptKind == BackspaceExitEntry ]
    , [ "NOFREEZE" | pr.freeze == NoFreeze ]
    , [ "COMMANDMODE" | pr.promptMode == PromptCommand ]
    , [ "ISPANE" | inPane ] ]

-- | A fresh, empty prompt: the bare @:@ command line.
emptyPrompt :: PromptState
emptyPrompt = PromptState
    { input = ""
    , cursor = 0
    , histIx = Nothing
    , pending = ""
    , template = ""
    , answers = []
    , queued = []
    , promptLabel = ":"
    , killed = ""
    , promptKeys = PromptKeys
        { statusKeys = KeysEmacs
        , wordSeparators = defaultOptions.wordSeparators
        , insertCursor = PromptCursor "" CursorDefault
        , commandCursor = PromptCursor "" CursorDefault }
    , promptMode = PromptInsert
    , promptKind = LineEntry
    , promptType = CommandPrompt
    , freeze = FreezeScreen
    , quoteNext = False
    , lastInput = ""
    , completions = []
    , waiter = Nothing
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

-- | Where submitting a line to a prompt leads.
data PromptAnswered
    = NextPrompt PromptState  -- ^ a multi-prompt's next question
    | AnswersDone Text        -- ^ the command every answer makes
    deriving (Eq, Show)

-- | Submit @line@ to a prompt: ask the next queued prompt, or splice every
-- answer into the template.
answerPrompt :: PromptState -> Text -> PromptAnswered
answerPrompt pr line = case pr.queued of
    (label, initial) : rest -> NextPrompt (promptFor label initial pr.template)
        { answers = answers', queued = rest, promptKeys = pr.promptKeys
        , promptKind = pr.promptKind, promptType = pr.promptType, freeze = pr.freeze
        , waiter = pr.waiter }
    [] -> AnswersDone (applyTemplate pr.template answers')
  where
    answers' = pr.answers <> [line]

-- | Splice a @command-prompt@'s answers into its template, the @n@th answer
-- through 'templateReplace' at index @n@ (tmux's @args_make_commands@). An
-- empty template is tmux's default, @%1@: the answer runs as typed.
applyTemplate :: Text -> [Text] -> Text
applyTemplate tmpl answers =
    List.foldl' (\t (i, a) -> templateReplace t a i)
        (if T.null tmpl then "%1" else tmpl) (zip [1 ..] answers)

-- | tmux's @cmd_template_replace@: put @s@ in for every @%idx@ (raw) or
-- @%idx%@ (double-quote escaped), and for the first @%%@ (escaped for single
-- quotes) or @%%%@ (double-quote escaped).
templateReplace :: Text -> Text -> Int -> Text
templateReplace tmpl s idx
    | not (T.any (== '%') tmpl) = tmpl
    | otherwise = T.pack (go False (T.unpack tmpl))
  where
    go _ [] = []
    go replaced ('%' : rest)
        | d : rest' <- rest, isDigit d, d /= '0', digitToInt d == idx =
            marker replaced NoQuote rest'
        | not replaced, '%' : rest' <- rest = marker True SingleQuoted rest'
    go replaced (c : rest) = c : go replaced rest
    marker replaced q = \case
        '%' : rest -> escape DoubleQuoted <> go replaced rest
        rest -> escape q <> go replaced rest
    escape = \case
        NoQuote -> T.unpack s
        SingleQuoted -> concatMap (\c -> if c == '\'' then "'\\''" else [c]) (T.unpack s)
        DoubleQuoted -> concatMap (\c -> if c `elem` ("\"\\$;~" :: String)
            then ['\\', c] else [c]) (T.unpack s)

-- | How 'templateReplace' quotes a replacement: for the quotes around the
-- marker in the template.
data Quoting = NoQuote | SingleQuoted | DoubleQuoted

-- | Apply one key to the prompt (tmux's @prompt_key@): a one-key, one
-- character or digits-only prompt takes it its own way, a quoted key goes
-- in as it is, and any other is an edit read as the prompt's
-- @status-keys@ say.
editPrompt :: PromptEnv -> PromptState -> Key -> PromptEdit
editPrompt env st0 key = case st.promptKind of
    -- A mouse report, or bytes no key is named for, is not a key press.
    KeyEntry
        | key.name `elem` ["Mouse", "Unknown"] -> Editing st
        | otherwise -> Submit key.name
    NumericEntry -> case T.unpack key.name of
        [c] | isDigit c -> Editing (insert (T.singleton c) st)
        _ -> SubmitPassing st.input
    SingleEntry -> case literalChar key of
        Nothing -> Editing st
        Just c
            | T.length (insert (T.singleton c) st).input == 1 -> Submit (T.singleton c)
            | otherwise -> Cancel
    _ | st.quoteNext -> case literalChar key of
        Nothing -> Editing st
        Just c -> Editing (insert (T.singleton c) st) { quoteNext = False }
    IncrementalEntry
        | key.name `elem` ["Up", "Down", "PPage", "NPage", "Left", "Right", "Enter"] -> Cancel
        | key.name `elem` ["C-r", "C-s"] -> if T.null st.input
            then Answering (setLine st.lastInput st) ("=" <> st.lastInput)
            else Answering st ((if key.name == "C-r" then "-" else "+") <> st.input)
        | otherwise -> case lineEdit env st key of
            Editing st' | st'.input /= st.input -> Answering st' ("=" <> st'.input)
            edit -> edit
    BackspaceExitEntry | key.name `elem` ["BSpace", "C-h"], T.null st.input -> Cancel
    _ -> lineEdit env st key
  where
    -- The matches a Tab showed go with the next key.
    st = st0 { completions = [] }

-- | A key as the character it puts in a one-character prompt or after
-- @C-v@: a control key as its control character, @BSpace@ as DEL; a key
-- that is no character puts in nothing.
literalChar :: Key -> Maybe Char
literalChar key = case T.unpack key.name of
    [c] -> Just c
    "Space" -> Just ' '
    "BSpace" -> Just '\DEL'
    "Enter" -> Just '\r'
    "Tab" -> Just '\t'
    "Escape" -> Just '\ESC'
    ['C', '-', c]
        | c == '?' -> Just '\DEL'
        | toUpper c >= '@' && toUpper c <= '_' -> Just (chr (ord (toUpper c) - 64))
    _ -> Nothing

-- | An edit to the line, read as its @status-keys@ say.
lineEdit :: PromptEnv -> PromptState -> Key -> PromptEdit
lineEdit env st key = case st.promptKeys.statusKeys of
    KeysEmacs -> emacsKey env st key
    KeysVi -> case st.promptMode of
        PromptInsert -> viInsertKey env st key
        PromptCommand -> viCommandKey env st key

-- | A vi-keys prompt in insert mode (tmux's @prompt_translate_key@):
-- Escape enters command mode, the line-editing keys they share act as in
-- emacs mode, and any other key inserts itself if it is a character.
viInsertKey :: PromptEnv -> PromptState -> Key -> PromptEdit
viInsertKey env st key
    | key.name `elem` ["Escape", "C-["] = Editing
        (moveTo (st.cursor - 1) st) { promptMode = PromptCommand }
    | key.name `elem` shared = emacsKey env st key
    | otherwise = Editing (maybe st (`insert` st) (insertText key))
  where
    shared =
        [ "C-a", "C-c", "C-e", "C-g", "C-h", "Tab", "C-k", "C-n", "C-p"
        , "C-t", "C-u", "C-v", "C-w", "C-y", "Enter", "C-Left", "C-Right"
        , "BSpace", "Delete", "Down", "End", "Home", "Left", "Right", "Up" ]

-- | A vi-keys prompt in command mode (tmux's @prompt_translate_key@).
viCommandKey :: PromptEnv -> PromptState -> Key -> PromptEdit
viCommandKey env st key = case key.name of
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
    as name = emacsKey env st key { name = name }
    move f = Editing (moveTo (f st.input st.cursor) st)
    inserting = \case
        Editing st' -> Editing st' { promptMode = PromptInsert }
        done -> done

-- | tmux's @C-u@: empty the line.
clearLine :: PromptState -> PromptState
clearLine st = st { input = "", cursor = 0 }

-- | The emacs-keys prompt (and the keys vi mode shares with it).
emacsKey :: PromptEnv -> PromptState -> Key -> PromptEdit
emacsKey env st key = case key.name of
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
    "C-u"    -> Editing st { input = "", cursor = 0 }
    "C-w"    -> Editing (killWordBack st)
    "M-d"    -> Editing (killWordForward st)
    "C-y"    -> Editing (insert st.killed st)
    "Up"     -> Editing (historyBack env.history st)
    "Down"   -> Editing (historyForward env.history st)
    "C-v"    -> Editing st { quoteNext = True }
    "Tab"    -> Editing (complete env.commands st)
    _        -> case insertText key of
        Just t  -> Editing (insert t st)
        Nothing -> Editing st  -- unhandled key: swallowed, no change

-- | tmux's @prompt_replace_complete@: a command prompt's first word, the
-- cursor in or just after it, becomes the one command it begins, a space
-- after it, else the longest start the commands it begins share; with
-- nothing longer to put in, those commands are shown instead.
complete :: [Text] -> PromptState -> PromptState
complete commands st
    | st.promptType /= CommandPrompt || start /= 0 || T.null word = st
    | otherwise = case matches of
        [] -> st
        [one] -> replaceWith (one <> " ")
        m : ms
            | common m ms /= word -> replaceWith (common m ms)
            | otherwise -> st { completions = matches }
  where
    n = T.length st.input
    at = max 0 (min (n - 1) (if st.cursor == 0 then 0 else st.cursor - 1))
    spaceAt i = i < n && isSpace (T.index st.input i)
    back i = if i > 0 && not (spaceAt i) then back (i - 1) else i
    fwd i = if spaceAt i then fwd (i + 1) else i
    start = fwd (back at)
    end = let go i = if i < n && not (spaceAt i) then go (i + 1) else i in go at
    word = T.take (end - start) (T.drop start st.input)
    matches = List.sort (List.nub (filter (word `T.isPrefixOf`) commands))
    common m ms = List.foldl' commonPrefix m ms
    commonPrefix a b = maybe "" (\(p, _, _) -> p) (T.commonPrefixes a b)
    replaceWith t = st
        { input = T.take start st.input <> t <> T.drop end st.input
        , cursor = start + T.length t }

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
