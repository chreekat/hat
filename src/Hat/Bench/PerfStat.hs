-- | The counter rows of a @perf stat -x, -o file@ run, as a lookup table.
-- Pure so the benchmark driver's arithmetic is testable away from perf.
module Hat.Bench.PerfStat
    ( PerfStat
    , parsePerfStat
    , counterCount
    , counterWord
    , counterDouble
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Word (Word64)

-- | One run's events, keyed by perf's own names (modifier suffix included,
-- e.g. @instructions:u@). Uncountable events keep their @<not counted>@ /
-- @<not supported>@ markers so a lookup can name the reason it has no number.
newtype PerfStat = PerfStat (Map Text Text)
    deriving stock (Eq, Show)

-- | Read the CSV that @-x,@ emits: comment and blank lines skipped, every
-- other line an event row of at least value, unit, and name. Zero rows — or a
-- line that is no row at all — is a 'Left', so a failed run can never read as
-- a zero measurement.
parsePerfStat :: Text -> Either Text PerfStat
parsePerfStat input = do
    rows <- traverse row (filter countable (T.lines input))
    if null rows
        then Left "no perf counter rows"
        else Right (PerfStat (Map.fromList rows))
  where
    countable l = not (T.null (T.strip l)) && not ("#" `T.isPrefixOf` l)
    row l = case T.splitOn "," l of
        value : _unit : event : _ -> Right (event, value)
        _ -> Left ("not a perf counter row: " <> l)

-- | How many event rows the run reported.
counterCount :: PerfStat -> Int
counterCount (PerfStat m) = Map.size m

-- | An integral counter (the @instructions@ family).
counterWord :: Text -> PerfStat -> Either Text Word64
counterWord = readWith TR.decimal

-- | A fractional counter (e.g. @task-clock@ msec).
counterDouble :: Text -> PerfStat -> Either Text Double
counterDouble = readWith TR.double

-- | A bare query name also finds its modifier-suffixed row (@instructions@
-- matches @instructions:u@), since perf appends the suffix on its own when
-- the paranoid level restricts counting to user space.
readWith :: TR.Reader a -> Text -> PerfStat -> Either Text a
readWith reader key (PerfStat m) = case exact <> suffixed of
    [] -> Left ("no such perf counter: " <> key)
    raw : _
        | raw == "<not counted>" || raw == "<not supported>" ->
            Left ("perf counter " <> key <> " was " <> T.drop 1 (T.dropEnd 1 raw))
        | otherwise -> case reader raw of
            Right (v, rest) | T.null (T.strip rest) -> Right v
            _ -> Left ("perf counter " <> key <> " is not a number: " <> raw)
  where
    exact = maybe [] pure (Map.lookup key m)
    suffixed = [ v | (k, v) <- Map.toList m, T.takeWhile (/= ':') k == key ]
