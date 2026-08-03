-- | The shared vocabulary every command implementation speaks: the
-- reply a command yields, the handler signature the dispatch table maps
-- names to, and the getopt-style argument parser they all reach for.
module Hat.Server.Command.Types
    ( Reply (..)
    , CommandImpl
    , parseArgs
    ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import Hat.Model (Client, ServerState)

-- | A single line of command output: normal text or an error.
data Reply = ROutput Text | RErr Text
    deriving (Eq, Show)

-- | A command handler: the server, the issuing client (if any), and the
-- command's argument words, producing reply lines.
type CommandImpl = ServerState -> Maybe Client -> [Text] -> IO [Reply]

-- getopt-style flag parser: @spec@ lists the letters that take a
-- value. Bundled forms work like tmux: @-dsfoo@ is @-d -s foo@.
-- Returns (value flags as ("-s", value), boolean flags as "-d",
-- positional args).
parseArgs :: [Char] -> [Text] -> ([(Text, Text)], [Text], [Text])
parseArgs spec = go [] []
  where
    go opts flags = \case
        [] -> (opts, flags, [])
        ("--" : rest) -> (opts, flags, rest)   -- end-of-flags separator
        (a : rest)
            | Just bundle <- T.stripPrefix "-" a
            , not (T.null bundle)
            , not (isNumber a) ->
                let (opts', flags', rest') = scanBundle bundle rest
                in go (opts' <> opts) (flags' <> flags) rest'
            | otherwise -> (opts, flags, a : rest)
    scanBundle bundle rest = case T.uncons bundle of
        Nothing -> ([], [], rest)
        Just (c, more)
            | c `elem` spec ->
                let val = fromMaybe more (T.stripPrefix "=" more)
                in case (T.null val, rest) of
                    (False, _) -> ([(dash c, val)], [], rest)
                    (True, v : rest') -> ([(dash c, v)], [], rest')
                    (True, []) -> ([(dash c, "")], [], [])
            | otherwise ->
                let (opts', flags', rest') = scanBundle more rest
                in (opts', dash c : flags', rest')
    dash c = T.pack ['-', c]
    isNumber a = case TR.signed TR.decimal a of
        Right (_ :: Int, restT) -> T.null restT
        Left _ -> False
