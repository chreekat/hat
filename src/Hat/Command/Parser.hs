{-# LANGUAGE RoleAnnotations #-}

-- | The command language: one megaparsec tokenizer shared by the
-- config file, the command prompt, and @hat <command>@ from a shell.
--
-- A line holds one or more commands separated by unescaped semicolons.
-- Words quote with single quotes (literal), double quotes (backslash
-- escapes), or balanced brace blocks (verbatim, for command blocks).
module Hat.Command.Parser
    ( parseCommandLine
    , parseScript
    , parseArgv
    ) where

import Data.Char (isAlphaNum, isAscii, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Hat.Command.Script (Script (..), ScriptItem (..))
import Hat.Server.Environ (EnvVisibility (..))

type Parser = Parsec Void Text

-- | Split argv (from @getArgs@) into a list of commands, using tmux's
-- convention: a trailing unescaped @;@ on a token is a command boundary,
-- a trailing @\;@ is a literal @;@, and semicolons in the middle of a
-- token are literal. Empty commands are dropped.
parseArgv :: [String] -> [[Text]]
parseArgv = finish . foldl step ([], [])
  where
    step (cmds, cur) arg = case classify arg of
        (arg', True)
            | null arg' -> (reverse cur : cmds, [])
            | otherwise -> (reverse (T.pack arg' : cur) : cmds, [])
        (arg', False) -> (cmds, T.pack arg' : cur)
    finish (cmds, cur) =
        filter (not . null) . reverse $ reverse cur : cmds
    classify s = case reverse s of
        ';' : '\\' : rest -> (reverse rest ++ ";", False)
        ';' : rest        -> (reverse rest, True)
        _                 -> (s, False)

-- | Parse a single command line (possibly @;@-separated).
parseCommandLine :: Text -> Either Text [[Text]]
parseCommandLine input =
    case parse (commandsP <* eof) "<command>" input of
        Left err -> Left (T.pack (errorBundlePretty err))
        Right cmds -> Right (filter (not . null) cmds)

-- | Parse a whole file of commands (tmux's @cmd-parse.y@): comments, blank
-- lines, continuations, assignments and conditionals, each item with the
-- line its statement starts on. An error stops the parse at tmux's line
-- and in its words, keeping the items before it.
parseScript :: Text -> Script
parseScript input = case runScriptP statementsP stream of
    (Left err, items) -> Script items (Just err)
    (Right (), items) -> Script items Nothing
  where
    stream = concatMap lexChunk chunks <> [(T.count "\n" input + 1, LEnd)]
    chunks = joinBraces
        $ stripComments
        $ joinContinuations (zip [1 ..] (T.lines input))
    lexChunk (n, text) = case parse (tokensP <* eof) "<command>" text of
        Left err -> [(n, LBad (T.pack (errorBundlePretty err)))]
        Right toks -> map ((n,) . LTok) toks <> [(n, LNewline)]

-- The grammar --------------------------------------------------------------

-- | A word, a separator, or a directive, as tmux's lexer gives them.
data ScriptToken
    = TWord Text
    | TBrace Text
    | TSemi
    | TFormat Text  -- ^ a @#{...}@ right after a directive
    | TIf
    | TElif
    | TElse
    | TEndif
    | THidden
    | TInvalid      -- ^ an unknown directive, or a format that never closes
    deriving (Eq)

-- | A token, the end of a line, the end of the file, or a chunk that
-- would not tokenize (and why).
data Lexeme = LTok ScriptToken | LNewline | LEnd | LBad Text

-- | A parser over a file's lexemes that keeps the items it has emitted
-- when it fails.
type role ScriptP representational
newtype ScriptP a = ScriptP
    { stepP :: [(Int, Lexeme)] -> [(Int, ScriptItem)]
            -> (Either (Int, Text) a, [(Int, Lexeme)], [(Int, ScriptItem)]) }

instance Functor ScriptP where
    fmap f p = ScriptP $ \s o -> case p.stepP s o of
        (r, s', o') -> (fmap f r, s', o')

instance Applicative ScriptP where
    pure a = ScriptP (\s o -> (Right a, s, o))
    pf <*> pa = pf >>= \f -> fmap f pa

instance Monad ScriptP where
    p >>= k = ScriptP $ \s o -> case p.stepP s o of
        (Left e, s', o') -> (Left e, s', o')
        (Right a, s', o') -> (k a).stepP s' o'

runScriptP :: ScriptP () -> [(Int, Lexeme)] -> (Either (Int, Text) (), [(Int, ScriptItem)])
runScriptP p s = case p.stepP s [] of
    (r, _, o) -> (r, reverse o)

-- | The next lexeme, with its line.
peekP :: ScriptP (Int, Lexeme)
peekP = ScriptP $ \s o -> case s of
    l : _ -> (Right l, s, o)
    [] -> (Right (0, LEnd), s, o)

advanceP :: ScriptP ()
advanceP = ScriptP (\s o -> (Right (), drop 1 s, o))

emitP :: Int -> ScriptItem -> ScriptP ()
emitP n item = ScriptP (\s o -> (Right (), s, (n, item) : o))

-- | Fail at the next lexeme: tmux's @syntax error@, or why its chunk would
-- not tokenize.
unexpectedP :: ScriptP a
unexpectedP = do
    (n, l) <- peekP
    ScriptP $ \s o -> case l of
        LBad e -> (Left (n, e), s, o)
        _ -> (Left (n, "syntax error"), s, o)

-- | The file: statements to its end.
statementsP :: ScriptP ()
statementsP = peekP >>= \case
    (_, LEnd) -> pure ()
    _ -> statementP *> newlineP *> statementsP

newlineP :: ScriptP ()
newlineP = peekP >>= \case
    (_, LNewline) -> advanceP
    _ -> unexpectedP

-- | One line's statement: empty, a hidden assignment, a conditional over
-- the lines that follow, or commands.
statementP :: ScriptP ()
statementP = peekP >>= \case
    (_, LNewline) -> pure ()
    (n, LTok THidden) -> advanceP *> peekP >>= \case
        (_, LTok (TWord w)) | Just (name, value) <- assignment w ->
            advanceP *> emitP n (ScriptAssign EnvHidden name value)
        _ -> unexpectedP
    (n, LTok TIf) -> do
        advanceP
        fmt <- conditionP
        peekP >>= \case
            (_, LNewline) -> emitP n (ScriptIf fmt) *> newlineP *> linesConditionP
            _ -> emitP n (ScriptIf fmt) *> inlineConditionP *> commandListTailP
    _ -> commandListP

-- | The format a directive tests: a @#{...}@ or a word that is not an
-- assignment.
conditionP :: ScriptP Text
conditionP = peekP >>= \case
    (_, LTok (TFormat f)) -> f <$ advanceP
    (_, LTok (TWord w)) | Nothing <- assignment w -> w <$ advanceP
    _ -> unexpectedP

-- | A conditional's branches, each one or more lines, after its @%if@ line.
linesConditionP :: ScriptP ()
linesConditionP = branchLinesP *> peekP >>= \case
    (n, LTok TElif) -> do
        advanceP
        fmt <- conditionP
        emitP n (ScriptElif fmt) *> newlineP *> linesConditionP
    (n, LTok TElse) -> do
        advanceP *> emitP n ScriptElse *> newlineP *> branchLinesP
        endifP
    _ -> endifP
  where
    branchLinesP = statementP *> newlineP *> moreLinesP
    moreLinesP = peekP >>= \case
        (_, LTok d) | d `elem` [TElif, TElse, TEndif] -> pure ()
        (_, LEnd) -> pure ()
        _ -> branchLinesP

endifP :: ScriptP ()
endifP = peekP >>= \case
    (n, LTok TEndif) -> advanceP *> emitP n ScriptEndif
    _ -> unexpectedP

-- | A conditional within a line, after its @%if@ and format.
inlineConditionP :: ScriptP ()
inlineConditionP = commandListP *> peekP >>= \case
    (n, LTok TElif) -> do
        advanceP
        fmt <- conditionP
        emitP n (ScriptElif fmt) *> inlineConditionP
    (n, LTok TElse) -> advanceP *> emitP n ScriptElse *> commandListP *> endifP
    _ -> endifP

-- | Commands and conditionals separated by semicolons; at least one.
commandListP :: ScriptP ()
commandListP = elementP *> commandListTailP

commandListTailP :: ScriptP ()
commandListTailP = peekP >>= \case
    (_, LTok TSemi) -> advanceP *> peekP >>= \case
        (_, LTok (TWord _)) -> commandListP
        (_, LTok TIf) -> commandListP
        _ -> commandListTailP
    _ -> pure ()

elementP :: ScriptP ()
elementP = peekP >>= \case
    (n, LTok TIf) -> do
        advanceP
        fmt <- conditionP
        emitP n (ScriptIf fmt) *> inlineConditionP
    (n, LTok (TWord w))
        | Just (name, value) <- assignment w -> do
            advanceP *> emitP n (ScriptAssign EnvVisible name value)
            peekP >>= \case
                (_, LTok (TWord c)) | Nothing <- assignment c -> advanceP *> argumentsP n c
                (_, LTok (TWord _)) -> unexpectedP
                (_, LTok (TBrace _)) -> unexpectedP
                _ -> pure ()
        | otherwise -> advanceP *> argumentsP n w
    _ -> unexpectedP

-- | A command's arguments, after its name.
argumentsP :: Int -> Text -> ScriptP ()
argumentsP n name = go []
  where
    go args = peekP >>= \case
        (_, LTok (TWord w)) -> advanceP *> go (w : args)
        (_, LTok (TBrace b)) -> advanceP *> go (b : args)
        _ -> emitP n (ScriptCommand (name : reverse args))

-- | A word shaped @NAME=value@ (tmux's @EQUALS@ token): a name of ASCII
-- letters, digits and underscores, not starting with a digit.
assignment :: Text -> Maybe (Text, Text)
assignment w = case T.breakOn "=" w of
    (name, rest)
        | Just value <- T.stripPrefix "=" rest
        , Just (c, cs) <- T.uncons name
        , isVar c && not (isDigit c)
        , T.all isVar cs -> Just (name, value)
    _ -> Nothing
  where
    isVar c = isAscii c && (isAlphaNum c || c == '_')

-- The lexer ----------------------------------------------------------------

-- | One chunk's tokens. The token after a directive may be a format.
tokensP :: Parser [ScriptToken]
tokensP = space *> go False
  where
    go afterDirective = option [] $ do
        t <- tokenP afterDirective
        space
        (t :) <$> go (isDirective t)
    tokenP afterDirective = choice
        [ TSemi <$ char ';'
        , if afterDirective then formatP else empty
        , directiveP
        , TBrace <$> braceBlock
        , TWord . T.concat <$> some (choice [singleQuoted, doubleQuoted, bareWord])
        ]
    isDirective t = t `elem` [TIf, TElif, TElse, TEndif, THidden, TInvalid]

-- | A word starting @%@, up to whitespace: a directive, unless it is only
-- percent signs and digits.
directiveP :: Parser ScriptToken
directiveP = do
    w <- lookAhead (char '%') *> takeWhile1P (Just "directive") (`notElem` (" \t\n" :: String))
    pure $ if T.all (\c -> c == '%' || isDigit c) w then TWord w else case w of
        "%if"     -> TIf
        "%elif"   -> TElif
        "%else"   -> TElse
        "%endif"  -> TEndif
        "%hidden" -> THidden
        _         -> TInvalid

-- | A format, or the rest of the chunk as 'TInvalid' when it never closes.
formatP :: Parser ScriptToken
formatP = do
    rest <- lookAhead (string "#{" *> getInput)
    case formatSpan ("#{" <> rest) of
        Just (fmt, _) -> TFormat <$> string fmt
        Nothing -> TInvalid <$ takeRest

-- | The @#{...}@ at the start of the text and what follows it (tmux's
-- @yylex_format@): nested @#{@ open further levels, any character after a
-- @#@ is taken as is, and a line ends it unclosed.
formatSpan :: Text -> Maybe (Text, Text)
formatSpan t = do
    body <- T.stripPrefix "#{" t
    n <- go (1 :: Int) 2 body
    pure (T.splitAt n t)
  where
    go depth n s = case T.uncons s of
        Nothing -> Nothing
        Just ('\n', _) -> Nothing
        Just ('#', s1) -> case T.uncons s1 of
            Nothing -> Nothing
            Just ('\n', _) -> Nothing
            Just (c, s2) -> go (if c == '{' then depth + 1 else depth) (n + 2) s2
        Just ('}', s1)
            | depth == 1 -> Just (n + 1)
            | otherwise -> go (depth - 1) (n + 1) s1
        Just (_, s1) -> go depth (n + 1) s1

-- Blocks like @bind X { ... }@ and multi-line @"..."@ strings can span
-- lines. Keep joining while brace depth is positive OR we're mid-quote.
joinBraces :: [(Int, Text)] -> [(Int, Text)]
joinBraces = collapse 0 Free []
  where
    collapse 0 Free [] [] = []
    collapse _ _ acc [] = [joined acc]  -- unbalanced
    collapse depth q acc (l : rest) =
        let (d', q') = lineEndState depth q (snd l)
            acc' = l : acc
        in if d' > 0 || q' /= Free
            then collapse d' q' acc' rest
            else joined acc' : collapse 0 Free [] rest
    joined acc = case reverse acc of
        ls@((n, _) : _) -> (n, T.intercalate "\n" (map snd ls))
        [] -> (0, "")

lineEndState :: Int -> QuoteState -> Text -> (Int, QuoteState)
lineEndState d0 q0 = T.foldl' step (d0, q0)
  where
    step (n, Free) '{'    = (n + 1, Free)
    step (n, Free) '}'    = (n - 1, Free)
    step (n, Free) '\''   = (n, Single)
    step (n, Free) '"'    = (n, Double)
    step (n, Single) '\'' = (n, Free)
    step (n, Double) '"'  = (n, Free)
    step (n, Double) '\\' = (n, DoubleEsc)
    step (n, DoubleEsc) _ = (n, Double)
    step st _             = st

data QuoteState = Free | Single | Double | DoubleEsc
    deriving (Eq)

-- Lines ending in a backslash continue on the next line.
joinContinuations :: [(Int, Text)] -> [(Int, Text)]
joinContinuations = \case
    [] -> []
    ((n, l) : rest)
        | "\\" `T.isSuffixOf` l
        , ((_, next) : rest') <- joinContinuations rest ->
            (n, T.dropEnd 1 l <> " " <> next) : rest'
        | otherwise -> (n, l) : joinContinuations rest

-- | Drop comments as tmux's lexer skips them: a @#@ that starts a word (at
-- a line's start, after a blank, or after an unquoted @;@, @{@ or @}@) runs
-- to the end of its line, unless it opens the format a directive tests. A
-- line a quote continues loses its leading blanks, and then, if it starts a
-- comment (a @#@ not before one of @,#{}:@), the rest of it too.
stripComments :: [(Int, Text)] -> [(Int, Text)]
stripComments = go Free
  where
    go _ [] = []
    go q ((n, l) : rest) = case stripLine q l of
        (l', q') -> (n, l') : go q' rest

stripLine :: QuoteState -> Text -> (Text, QuoteState)
stripLine q0 line0
    | q0 == Free = scan line0 Free
    | otherwise = case T.dropWhile (`elem` [' ', '\t']) line0 of
        rest | Just ('#', after) <- T.uncons rest
             , maybe True ((`notElem` (",#{}:" :: String)) . fst) (T.uncons after) ->
                ("", q0)
             | otherwise -> scan rest q0
  where
    scan line q = go 0 q True False
      where
        n = T.length line
        at = T.index line
        cut i st = (T.take i line, st)
        go i st boundary afterDirective
            | i >= n = cut n st
            | otherwise = case (st, at i) of
                (Single, '\'') -> go (i + 1) Free False False
                (Single, _) -> go (i + 1) Single False False
                (Double, '"') -> go (i + 1) Free False False
                (Double, '\\') -> go (i + 2) Double False False
                (Double, _) -> go (i + 1) Double False False
                (DoubleEsc, _) -> go (i + 1) Double False False
                (Free, c) -> case c of
                    '\'' -> go (i + 1) Single False False
                    '"' -> go (i + 1) Double False False
                    '\\' -> go (i + 2) Free False False
                    '#' | boundary -> case formatSpan (T.drop i line) of
                        Just (fmt, _) | afterDirective ->
                            go (i + T.length fmt) Free False False
                        _ -> cut i Free
                    '%' | boundary ->
                        let w = T.takeWhile (`notElem` [' ', '\t']) (T.drop i line)
                        in go (i + T.length w) Free False
                            (not (T.all (\d -> d == '%' || isDigit d) w))
                    _ | c == ' ' || c == '\t' -> go (i + 1) Free True afterDirective
                      | c `elem` [';', '{', '}'] -> go (i + 1) Free True False
                      | otherwise -> go (i + 1) Free False False

commandsP :: Parser [[Text]]
commandsP = do
    space
    sepBy commandP (char ';' *> space)

commandP :: Parser [Text]
commandP = many (wordP <* space)

-- | A brace block, or a word: adjacent quoted and bare pieces, joined.
wordP :: Parser Text
wordP = braceBlock <|> (T.concat <$> some (choice [singleQuoted, doubleQuoted, bareWord]))

singleQuoted :: Parser Text
singleQuoted = do
    _ <- char '\''
    body <- takeWhileP (Just "single-quoted text") (/= '\'')
    _ <- char '\''
    pure body

doubleQuoted :: Parser Text
doubleQuoted = do
    _ <- char '"'
    parts <- many $ choice
        [ char '\\' *> (T.singleton <$> anySingle)
        , takeWhile1P (Just "double-quoted text") (\c -> c /= '"' && c /= '\\')
        ]
    _ <- char '"'
    pure (T.concat parts)

-- The block's contents are kept verbatim (minus the outer braces) so
-- they can be re-parsed when the command runs.
braceBlock :: Parser Text
braceBlock = do
    _ <- char '{'
    body <- bodyP
    _ <- char '}'
    pure body
  where
    bodyP = fmap T.concat . many $ choice
        [ takeWhile1P (Just "block text") (\c -> c /= '{' && c /= '}')
        , do
            inner <- braceBlock
            pure ("{" <> inner <> "}")
        ]

bareWord :: Parser Text
bareWord = fmap T.concat . some $ choice
    [ char '\\' *> (T.singleton <$> anySingle)
    , takeWhile1P (Just "word")
        (\c -> not (isSpace c) && c `notElem` (";'\"{}\\" :: String))
    ]
