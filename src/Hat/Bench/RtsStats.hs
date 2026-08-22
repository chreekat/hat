-- | The @+RTS -t/-s --machine-readable@ stats block, as a lookup table.
-- Pure so the benchmark driver's arithmetic is testable away from a live
-- process.
module Hat.Bench.RtsStats
    ( Stats
    , parseRtsStats
    , statCount
    , statWord
    , statDouble
    ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Void (Void)
import Data.Word (Word64)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

-- | The counters one RTS run reported, keyed by the RTS's own names
-- (@allocated_bytes@, @GC_cpu_seconds@, …). Kept untyped so a GHC release
-- that adds or drops a counter still parses.
newtype Stats = Stats (Map Text Text)
    deriving stock (Eq, Show)

-- | Read the stats block, skipping the argv line the RTS prints above it.
-- Absent a block — the program died before the RTS could report — this is a
-- 'Left' rather than an empty table, so a missing measurement can never read
-- as a zero one.
parseRtsStats :: Text -> Either Text Stats
parseRtsStats input = case parse statsBlock "" input of
    Left e -> Left (T.pack (errorBundlePretty e))
    Right ps -> Right (Stats (Map.fromList ps))

statsBlock :: Parser [(Text, Text)]
statsBlock = skipManyTill anySingle (try open) *> pairs
  where
    -- The block's own bracket, told from a bracket in the argv line above by
    -- the pair that must follow it.
    open = space *> char '[' *> space <* lookAhead (char '(')
    pairs = pair `sepBy` try (space *> char ',' *> space) <* space <* char ']'
    pair = (,) <$> (char '(' *> space *> quoted <* space <* char ',' <* space)
               <*> (quoted <* space <* char ')')
    quoted = char '"' *> takeWhileP Nothing (/= '"') <* char '"'

-- | How many counters the block held.
statCount :: Stats -> Int
statCount (Stats m) = Map.size m

-- | An integral counter, named for the miss so a typo reads as one.
statWord :: Text -> Stats -> Either Text Word64
statWord = readWith TR.decimal

-- | A fractional counter (the @*_seconds@ family).
statDouble :: Text -> Stats -> Either Text Double
statDouble = readWith TR.double

readWith :: TR.Reader a -> Text -> Stats -> Either Text a
readWith reader key (Stats m) = case Map.lookup key m of
    Nothing -> Left ("no such RTS counter: " <> key)
    Just raw -> case reader raw of
        Right (v, rest) | T.null (T.strip rest) -> Right v
        _ -> Left ("RTS counter " <> key <> " is not a number: " <> raw)
