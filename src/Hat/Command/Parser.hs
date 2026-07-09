-- | The command language: one megaparsec tokenizer shared by the
-- config file, the command prompt, and @hat <command>@ from a shell.
--
-- A line holds one or more commands separated by unescaped semicolons.
-- Words quote with single quotes (literal), double quotes (backslash
-- escapes), or balanced brace blocks (verbatim, for command blocks).
module Hat.Command.Parser
    ( parseCommandLine
    , parseConfig
    , parseArgv
    ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

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

-- | Parse a whole config file: comments, blank lines, continuations.
parseConfig :: Text -> Either Text [[Text]]
parseConfig input = fmap concat . traverse parseCommandLine
    $ filter (not . T.null . T.strip)
    $ map stripComment
    $ joinContinuations (T.lines input)

-- Lines ending in a backslash continue on the next line.
joinContinuations :: [Text] -> [Text]
joinContinuations = \case
    [] -> []
    (l : rest)
        | "\\" `T.isSuffixOf` l
        , (next : rest') <- joinContinuations rest ->
            (T.dropEnd 1 l <> " " <> next) : rest'
        | otherwise -> l : joinContinuations rest

-- A # starts a comment unless it is inside a word (quoting is handled
-- coarsely: we only strip comments that follow whitespace or start the
-- line, which matches tmux config practice).
stripComment :: Text -> Text
stripComment line = go 0 False False
  where
    n = T.length line
    go i inSingle inDouble
        | i >= n = line
        | otherwise = case T.index line i of
            '\'' | not inDouble -> go (i + 1) (not inSingle) inDouble
            '"' | not inSingle -> go (i + 1) inSingle (not inDouble)
            '\\' | inDouble -> go (i + 2) inSingle inDouble
            '#' | not inSingle && not inDouble
                , i == 0 || isSpace (T.index line (i - 1)) ->
                    T.take i line
            _ -> go (i + 1) inSingle inDouble

commandsP :: Parser [[Text]]
commandsP = do
    space
    sepBy commandP (char ';' *> space)

commandP :: Parser [Text]
commandP = many (wordP <* space)

wordP :: Parser Text
wordP = choice
    [ singleQuoted
    , doubleQuoted
    , braceBlock
    , bareWord
    ]

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
