{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | The shared vocabulary every command implementation speaks: the
-- reply a command yields, the handler signature the dispatch table maps
-- names to, and the getopt-style argument parser they all reach for.
module Hat.Server.Command.Types
    ( Reply (ROutput, RErr, RData, RExitStatus)
    , errQuoting
    , replyErrorBytes
    , CommandImpl
    , ArgvImpl
    , textOnly
    , Dispatch (..)
    , FlagWord
    , parseArgs
    , parseArgsDigits
    , parseArgsOver
    , unknownFlag
    , strtonum
    , templateReplace
    ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Char (chr)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR

import Hat.Command.Arg (Arg (..), argFromBytes, argLossy)
import Hat.Model (Client, Pane, ServerState)
import Hat.Server.ArgNumber (strtonum)

-- | A single line of command output: normal text or an error; or raw
-- bytes for the issuing command client's standard output; or the status
-- that client exits with (tmux's @c->retval@), the last one given winning.
data Reply
    = ROutput Text
    | RErrText Text
    | RErrQuoting Text ByteString
        -- ^ an error's prose and the argument it quotes, never valid UTF-8;
        --   see 'errQuoting'
    | RData ByteString
    | RExitStatus Int
    deriving (Eq, Show)

-- | Any error as text, a quoted argument's invalid bytes as U+FFFD.
pattern RErr :: Text -> Reply
pattern RErr e <- (errorText -> Just e)
  where RErr e = RErrText e
{-# COMPLETE ROutput, RErr, RData, RExitStatus #-}

errorText :: Reply -> Maybe Text
errorText = \case
    RErrText e -> Just e
    RErrQuoting prose bytes -> Just (prose <> TE.decodeUtf8Lenient bytes)
    _ -> Nothing

-- | An error quoting an argument verbatim, as tmux's @cmdq_error@ does.
errQuoting :: Text -> Arg -> Reply
errQuoting prose = \case
    Utf8Arg t -> RErr (prose <> t)
    RawArg bytes -> RErrQuoting prose bytes

-- | An error's bytes when they are not UTF-8: a quoted argument's verbatim.
replyErrorBytes :: Reply -> Maybe ByteString
replyErrorBytes = \case
    RErrQuoting prose bytes -> Just (TE.encodeUtf8 prose <> bytes)
    _ -> Nothing

-- | A command handler: the server, the issuing client (if any), and the
-- command's argument words, producing reply lines.
type CommandImpl = ServerState -> Maybe Client -> [Text] -> IO [Reply]

-- | A command handler taking its words as they arrived, UTF-8 or not.
type ArgvImpl = ServerState -> Maybe Client -> [Arg] -> IO [Reply]

-- | A handler that takes only text: a word that is not UTF-8 fails the
-- command @name@, quoting it.
textOnly :: Text -> CommandImpl -> ArgvImpl
textOnly name impl st mclient args = case traverse text args of
    Right ts -> impl st mclient ts
    Left raw -> pure [errQuoting (name <> ": argument is not valid UTF-8: ") raw]
  where
    text = \case
        Utf8Arg t -> Right t
        raw -> Left raw

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
parseArgs spec = parseArgsOver spec []

-- | 'parseArgs' for a command whose flags include the digits @digits@
-- (@command-prompt -1@): an argument of those digits alone is flags, not
-- the number it otherwise reads as.
parseArgsDigits :: [Char] -> [Char] -> [Text] -> ([(Text, Text)], [Text], [Text])
parseArgsDigits = parseArgsOver

-- | A word flags are read from. Flag letters are ASCII, so a word that is
-- not UTF-8 splits as bytes.
class FlagWord w where
    wordUncons :: w -> Maybe (Char, w)
    wordText :: w -> Text  -- ^ U+FFFD where it is not UTF-8

instance FlagWord Text where
    wordUncons = T.uncons
    wordText = id

instance FlagWord Arg where
    wordUncons = \case
        Utf8Arg t -> fmap Utf8Arg <$> T.uncons t
        RawArg bytes -> do
            (w, rest) <- B.uncons bytes
            pure (if w < 0x80 then chr (fromIntegral w) else '\xFFFD', argFromBytes rest)
    wordText = argLossy

-- | 'parseArgsDigits' over any 'FlagWord'.
{-# INLINABLE parseArgsOver #-}
parseArgsOver :: FlagWord w => [Char] -> [Char] -> [w] -> ([(Text, w)], [Text], [w])
parseArgsOver spec digits = go [] []
  where
    go opts flags = \case
        [] -> (opts, flags, [])
        (a : rest)
            | wordText a == "--" -> (opts, flags, rest)   -- end-of-flags separator
            | Just ('-', bundle) <- wordUncons a
            , not (blank bundle)
            , not (isNumber (wordText a)) || T.all (`elem` digits) (wordText bundle) ->
                let (opts', flags', rest') = scanBundle bundle rest
                in go (opts' <> opts) (flags' <> flags) rest'
            | otherwise -> (opts, flags, a : rest)
    blank = isNothing . wordUncons
    scanBundle bundle rest = case wordUncons bundle of
        Nothing -> ([], [], rest)
        Just (c, more)
            | c `elem` spec ->
                case (blank more, rest) of
                    (False, _) -> ([(dash c, more)], [], rest)
                    (True, v : rest') -> ([(dash c, v)], [], rest')
                    (True, []) -> ([(dash c, more)], [], [])
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
