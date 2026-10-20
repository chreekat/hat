-- | The shared vocabulary every command implementation speaks: the
-- reply a command yields, the handler signature the dispatch table maps
-- names to, and the getopt-style argument parser they all reach for.
module Hat.Server.Command.Types
    ( Reply (..)
    , CommandImpl
    , Dispatch (..)
    , parseArgs
    , parseArgsDigits
    , unknownFlag
    , strtonum
    , templateReplace
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Model (Client, Pane, ServerState)
import Hat.Server.ArgNumber (strtonum)

-- | A single line of command output: normal text or an error; or raw
-- bytes for the issuing command client's standard output.
data Reply = ROutput Text | RErr Text | RData ByteString
    deriving (Eq, Show)

-- | A command handler: the server, the issuing client (if any), and the
-- command's argument words, producing reply lines.
type CommandImpl = ServerState -> Maybe Client -> [Text] -> IO [Reply]

-- | The command engine's entry points, in the three shapes a caller has a
-- command in: one argv, a batch of them, or an unparsed line. Carrying them
-- as a value is what lets a client path run a command while knowing nothing
-- of the dispatch table.
data Dispatch = Dispatch
    { runArgv :: ServerState -> Maybe Client -> [Text] -> IO [Reply]
    , runCommands :: ServerState -> Maybe Client -> [[Text]] -> IO [Reply]
    , runCommandText :: ServerState -> Maybe Client -> Text -> IO [Reply]
    , unknownCommand :: ServerState -> [Text] -> IO (Maybe Text)
        -- ^ the error for a command whose name resolves to nothing, which
        --   tmux reports when parsing, before anything runs
    , newEditorPane :: (Pane -> IO ()) -> CommandImpl
        -- ^ 'Hat.Server.Command.Pane.cmdNewEditorPane', for the modes that
        --   open an editor
    }

-- getopt-style flag parser: @spec@ lists the letters that take a
-- value. Bundled forms work like tmux: @-dsfoo@ is @-d -s foo@.
-- Returns (value flags as ("-s", value), boolean flags as "-d",
-- positional args).
parseArgs :: [Char] -> [Text] -> ([(Text, Text)], [Text], [Text])
parseArgs spec = parseArgsDigits spec []

-- | 'parseArgs' for a command whose flags include the digits @digits@
-- (@command-prompt -1@): an argument of those digits alone is flags, not
-- the number it otherwise reads as.
parseArgsDigits :: [Char] -> [Char] -> [Text] -> ([(Text, Text)], [Text], [Text])
parseArgsDigits spec digits = go [] []
  where
    go opts flags = \case
        [] -> (opts, flags, [])
        ("--" : rest) -> (opts, flags, rest)   -- end-of-flags separator
        (a : rest)
            | Just bundle <- T.stripPrefix "-" a
            , not (T.null bundle)
            , not (isNumber a) || T.all (`elem` digits) bundle ->
                let (opts', flags', rest') = scanBundle bundle rest
                in go (opts' <> opts) (flags' <> flags) rest'
            | otherwise -> (opts, flags, a : rest)
    scanBundle bundle rest = case T.uncons bundle of
        Nothing -> ([], [], rest)
        Just (c, more)
            | c `elem` spec ->
                case (T.null more, rest) of
                    (False, _) -> ([(dash c, more)], [], rest)
                    (True, v : rest') -> ([(dash c, v)], [], rest')
                    (True, []) -> ([(dash c, "")], [], [])
            | otherwise ->
                let (opts', flags', rest') = scanBundle more rest
                in (opts', dash c : flags', rest')
    dash c = T.pack ['-', c]
    isNumber a = case TR.signed TR.decimal a of
        Right (_ :: Int, restT) -> T.null restT
        Left _ -> False

-- | tmux's error for the first boolean flag of @name@ not among the
-- @allowed@ letters, if any.
unknownFlag :: Text -> [Char] -> [Text] -> Maybe Text
unknownFlag name allowed flags =
    case [f | f <- flags, maybe True (`notElem` allowed) (flagLetter f)] of
        f : _ -> Just ("command " <> name <> ": unknown flag " <> f)
        [] -> Nothing
  where
    flagLetter f = case T.unpack f of
        ['-', c] -> Just c
        _ -> Nothing

-- | tmux's @cmd_template_replace@: put @s@ in for the template's first
-- @%%@ (single-quote escaped for a @'%%'@ context) and every @%idx@; a
-- doubled @%%%@ or @%idx%@ form escapes it for double quotes instead.
templateReplace :: Text -> Text -> Int -> Text
templateReplace template s idx
    | not (T.any (== '%') template) = template
    | otherwise = go False (T.unpack template)
  where
    go _ [] = ""
    go replaced ('%' : rest) = case rest of
        c : rest'
            | c >= '1' && c <= '9', fromEnum c - fromEnum '0' == idx ->
                let (q, rest'') = quoting NoQuote rest'
                in quote q <> go replaced rest''
        '%' : rest' | not replaced ->
            let (q, rest'') = quoting SingleQuote rest'
            in quote q <> go True rest''
        _ -> T.cons '%' (go replaced rest)
    go replaced (c : rest) = T.cons c (go replaced rest)
    quoting _ ('%' : rest) = (DoubleQuote, rest)
    quoting q rest = (q, rest)
    quote = \case
        NoQuote -> s
        SingleQuote -> T.replace "'" "'\\''" s
        DoubleQuote -> T.concatMap
            (\c -> if c `elem` ("\"\\$;~" :: String) then T.pack ['\\', c] else T.singleton c) s

-- | How 'templateReplace' escapes the string it puts in.
data TemplateQuote = NoQuote | SingleQuote | DoubleQuote
