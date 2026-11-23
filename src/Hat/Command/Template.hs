-- | tmux's command arguments as @args_parse@ reads them against a
-- command's template.
module Hat.Command.Template
    ( Entry (..)
    , parseTmuxArgs
    ) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

-- | One parsed argument entry: a flag letter with its values (none for a
-- boolean flag, which counts its repeats; 'Nothing' for an optional value
-- left off).
data Entry = Boolean Int | Valued [Text] | OptionalBare

-- | tmux's @args_parse@ over a template: flags (sorted by letter, as its
-- argument tree keeps them) and the positional arguments, or its error.
parseTmuxArgs :: Text -> [Text] -> Either Text ([(Char, Entry)], [Text])
parseTmuxArgs template = go Map.empty
  where
    spec = T.unpack template
    kindOf c = case dropWhile (/= c) spec of
        (_ : ':' : ':' : _) -> Just OptionalArg
        (_ : ':' : _) -> Just RequiredArg
        (_ : _) -> Just NoArg
        [] -> Nothing
    go acc = \case
        ("--" : rest) -> Right (Map.toList acc, rest)
        (a : rest)
            | Just bundle <- T.stripPrefix "-" a, not (T.null bundle) ->
                flagsIn acc (T.unpack bundle) rest
        rest -> Right (Map.toList acc, rest)
    flagsIn acc [] rest = go acc rest
    flagsIn acc (c : more) rest
        | not (isAlphaNum c) = Left ("invalid flag -" <> T.singleton c)
        | otherwise = case kindOf c of
            Nothing -> Left ("unknown flag -" <> T.singleton c)
            Just NoArg -> flagsIn (Map.insertWith bump c (Boolean 1) acc) more rest
            Just kind
                | not (null more) -> go (addValue c (T.pack more) acc) rest
                | v : rest' <- rest, kind == RequiredArg || takesOptional v ->
                    go (addValue c v acc) rest'
                | kind == OptionalArg -> go (Map.insertWith keepValued c OptionalBare acc) rest
                | otherwise -> Left ("-" <> T.singleton c <> " expects an argument")
    takesOptional v = case T.unpack v of
        ('-' : c2 : _) -> not (c2 == '-' || isAlpha c2)
        _ -> True
    bump (Boolean a) (Boolean b) = Boolean (a + b)
    bump new _ = new
    keepValued new old = case old of
        Valued _ -> old
        _ -> new
    addValue c v = Map.insertWith (\_ old -> case old of
        Valued vs -> Valued (vs <> [v])
        _ -> Valued [v]) c (Valued [v])

data ArgKind = NoArg | RequiredArg | OptionalArg
    deriving (Eq, Show)
