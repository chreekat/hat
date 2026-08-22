-- | A small fzf-style fuzzy matcher: score a query against a candidate string
-- and rank a collection by the result. Standalone (no @Hat.Server@ deps, no
-- external libraries) — 'Hat.Server.Picker' uses it to order choose-tree
-- matches.
module Hat.FuzzyMatch
    ( score
    ) where

import Data.Array (Array, listArray, (!))
import Data.Char (isAlphaNum, isLower, isUpper, toLower)
import Data.Text (Text)
import Data.Text qualified as T

-- | Fuzzy-match @query@ against @candidate@. 'Nothing' if @query@ is not a
-- subsequence of @candidate@ (compared case-insensitively); otherwise 'Just'
-- a score where higher is a better match. A character matched at a word
-- boundary, on a CamelCase hump, or contiguously with the previous match
-- scores more, so the best alignment of the query is the one returned. An
-- empty query matches everything with score @0@.
score :: Text -> Text -> Maybe Int
score query candidate
    | nq == 0 = Just 0
    | best < 0 = Nothing
    | otherwise = Just best
  where
    q = T.unpack (T.toLower query)
    c = T.unpack candidate
    nq = length q
    nc = length c
    qa = listArray (0, max 0 (nq - 1)) q :: Array Int Char
    ca = listArray (0, max 0 (nc - 1)) c :: Array Int Char
    cla = listArray (0, max 0 (nc - 1)) (map toLower c) :: Array Int Char

    best = go 0 0 0

    -- @go i j adj@: the best score matching @q[i..]@ within @c[j..]@, where
    -- @adj@ is 1 when @c[j-1]@ was a matched query character (so a match at
    -- @j@ earns the consecutive bonus). Memoized through 'memo'.
    go :: Int -> Int -> Int -> Int
    go i j adj = memo ! (i, j, adj)
    memo :: Array (Int, Int, Int) Int
    memo = listArray ((0, 0, 0), (nq, nc, 1))
        [ compute i j adj
        | i <- [0 .. nq], j <- [0 .. nc], adj <- [0, 1] ]
    compute :: Int -> Int -> Int -> Int
    compute i j adj
        | i == nq   = 0                     -- whole query placed
        | j == nc   = nope                  -- ran out of candidate
        | otherwise = max matchHere skipHere
      where
        matchHere
            | cla ! j == qa ! i =
                matchScore + bonusAt j
                    + (if adj == 1 then consecutiveBonus else 0)
                    + go (i + 1) (j + 1) 1
            | otherwise = nope
        skipHere = go i (j + 1) 0

    -- The placement bonus for matching at candidate position @j@.
    bonusAt j
        | j == 0                             = boundaryBonus  -- first character
        | not (isAlphaNum (ca ! (j - 1)))    = boundaryBonus  -- after a separator
        | isLower (ca ! (j - 1))
            && isUpper (ca ! j)              = camelBonus     -- CamelCase hump
        | otherwise                          = 0

    matchScore       = 16
    boundaryBonus    = 8
    camelBonus       = 7
    consecutiveBonus = 8
    -- Small enough to lose to any real alignment, large enough that summing a
    -- few real bonuses onto it stays negative (so it never masquerades as a
    -- match).
    nope = -1000000
