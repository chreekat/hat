-- | tmux's numeric argument readers, with the error words its messages
-- embed.
module Hat.Server.ArgNumber
    ( strtonum
    , stringPercentage
    ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

-- | A bounded decimal argument, failing with OpenBSD @strtonum@'s words
-- (\"invalid\", \"too small\", \"too large\") that tmux's errors embed.
strtonum :: Int -> Int -> Text -> Either Text Int
strtonum lo hi t = case TR.signed TR.decimal t of
    Right (n, rest) | T.null rest ->
        if n < toInteger lo then Left "too small"
        else if n > toInteger hi then Left "too large"
        else Right (fromInteger n)
    _ -> Left "invalid"

-- | tmux's @args_string_percentage@: a count in @[lo, hi]@, or @N%@ of
-- @cur@ (N up to 1000) landing in that range.
stringPercentage :: Int -> Int -> Int -> Text -> Either Text Int
stringPercentage lo hi cur v
    | T.null v = Left "empty"
    | Just body <- T.stripSuffix "%" v = do
        pct <- strtonum 0 1000 body
        let n = (cur * pct) `div` 100
        if n < lo then Left "too small"
        else if n > hi then Left "too large"
        else Right n
    | otherwise = strtonum lo hi v
