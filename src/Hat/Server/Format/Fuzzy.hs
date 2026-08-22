-- | tmux's fzf-style fuzzy matcher (@fuzzy.c@), backing the @#{m/z:}@ and
-- @#{m/p:}@ format modifiers: @|@-separated OR groups of space-separated
-- AND terms; a leading @'@ makes a term an exact substring, @^@\/@$@ anchor
-- it, @!@ inverts it (inverse terms are exact substrings). Smart-case with
-- ASCII-only folding; style sequences (@#[...]@) are invisible and occupy
-- no columns.
module Hat.Server.Format.Fuzzy
    ( fuzzyMatch
    ) where

import Data.Char (isAsciiUpper, toLower)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hat.TextWidth (charWidth)

-- A visible character of the text: the char, its display column, its width.
data VChar = VChar
    { ch    :: Char
    , col   :: Int
    , width :: Int
    }

-- | Match @pattern@ against @text@: 'Just' the display columns occupied by
-- the matched characters (empty for an empty pattern, which matches
-- everything), or 'Nothing' if there is no match.
fuzzyMatch :: Text -> Text -> Maybe [Int]
fuzzyMatch pattern text
    | T.null (T.dropWhile (\c -> c == ' ' || c == '|') pattern) = Just []
    | otherwise = case best of
        Nothing -> Nothing
        Just (_, matched) -> Just
            [ c | i <- IntSet.toAscList matched
                , let vc = cs V.! i
                , c <- [vc.col .. vc.col + vc.width - 1] ]
  where
    cs = scanVisible text
    fold = not (T.any isAsciiUpper pattern)
    groups = T.splitOn "|" pattern
    best = List.foldl' keepBest Nothing (map (matchGroup fold cs) groups)
    keepBest acc Nothing = acc
    keepBest Nothing (Just g) = Just g
    keepBest (Just a@(sa, _)) (Just b@(sb, _))
        | sb > sa = Just b
        | otherwise = Just a

-- Scan the text into visible characters, skipping #[...] styles and
-- halving runs of escaped #s, and assigning display columns.
scanVisible :: Text -> V.Vector VChar
scanVisible = V.fromList . place 0 . go
  where
    place _ [] = []
    place c (x : xs) = let w = charWidth x in VChar x c w : place (c + w) xs
    go t = case T.uncons t of
        Nothing -> []
        Just ('#', _) ->
            let n = T.length (T.takeWhile (== '#') t)
                after = T.drop n t
            in case T.uncons after of
                Just ('[', rest)
                    | even n -> replicate (n `div` 2) '#' <> ('[' : go rest)
                    | otherwise -> replicate (n `div` 2) '#' <> case skipStyle rest of
                        Nothing -> []
                        Just rest' -> go rest'
                _ -> replicate ((n + 1) `div` 2) '#' <> go after
        Just (c, rest)
            | c < ' ' || c == '\DEL' -> go rest
            | otherwise -> c : go rest
    -- Consume up to the style's closing ], honouring #-escapes and #{}.
    skipStyle = goSkip (0 :: Int)
      where
        goSkip depth t = case T.uncons t of
            Nothing -> Nothing
            Just ('#', t')
                | Just (c2, t'') <- T.uncons t', c2 `elem` (",#{}:]" :: String) ->
                    goSkip (if c2 == '{' then depth + 1 else depth) t''
            Just (']', t') | depth == 0 -> Just t'
            Just (c, t') -> goSkip (if c == '}' then depth - 1 else depth) t'

-- One parsed query term.
data Term = Term
    { inverse :: Bool
    , exact   :: Bool
    , prefix  :: Bool
    , suffix  :: Bool
    , body    :: [Char]
    }

parseTerm :: Text -> Maybe Term
parseTerm t0 = do
    (inv, t1) <- pure $ case T.stripPrefix "!" t0 of
        Just r -> (True, r)
        Nothing -> (False, t0)
    (ex, pre, t2) <- pure $ case T.uncons t1 of
        Just ('\'', r) -> (True, False, r)
        Just ('^', r) -> (True, True, r)
        _ -> (False, False, t1)
    (suf, t3) <- pure $ case T.stripSuffix "$" t2 of
        Just r -> (True, r)
        Nothing -> (False, t2)
    if T.null t3
        then Nothing
        else Just Term
            { inverse = inv
            , exact = ex || suf || inv
            , prefix = pre
            , suffix = suf
            , body = T.unpack t3
            }

-- Compare characters, folding ASCII case if wanted; non-ASCII is exact.
charEqual :: Bool -> Char -> Char -> Bool
charEqual fold a b
    | fold, a < '\x80', b < '\x80' = toLower a == toLower b
    | otherwise = a == b

boundary :: Char -> Bool
boundary c = c `elem` (" -_/.:" :: String)

-- Match one AND group; Just (score, matched char indices) on success.
matchGroup :: Bool -> V.Vector VChar -> Text -> Maybe (Int, IntSet.IntSet)
matchGroup fold cs grp = do
    terms <- traverse parseTerm (T.words grp)
    if null terms then Nothing else
        List.foldl' step (Just (0, IntSet.empty)) terms
  where
    step Nothing _ = Nothing
    step (Just (score, matched)) term
        | term.inverse = case matchExact fold cs term of
            Nothing -> Just (score, matched)
            Just _ -> Nothing
        | term.exact = case matchExact fold cs term of
            Nothing -> Nothing
            Just (s, pos) -> Just (score + s, matched <> IntSet.fromList pos)
        | otherwise = case matchFuzzy fold cs term.body of
            Nothing -> Nothing
            Just (s, pos) -> Just (score + s, matched <> IntSet.fromList pos)

-- Fuzzy subsequence match: forward scan, then backward compaction to
-- prefer a shorter span; returns the score and matched indices.
matchFuzzy :: Bool -> V.Vector VChar -> [Char] -> Maybe (Int, [Int])
matchFuzzy fold cs tok
    | null tok || V.null cs = Nothing
    | otherwise = do
        fwd <- forward 0 tok
        pos <- case reverse fwd of
            (lastIx : _) -> backward lastIx
            [] -> Nothing
        pure (scorePositions cs pos, pos)
  where
    n = V.length cs
    forward _ [] = Just []
    forward ci (q : qs) = case findFrom ci q of
        Nothing -> Nothing
        Just i -> (i :) <$> forward (i + 1) qs
    findFrom ci q
        | ci >= n = Nothing
        | charEqual fold q (cs V.! ci).ch = Just ci
        | otherwise = findFrom (ci + 1) q
    -- From the last match, walk each query char down to its highest
    -- position at or below the cursor.
    backward start = go start (reverse tok) []
      where
        go _ [] acc = Just acc
        go ci (q : qs) acc = case findDown ci q of
            Nothing -> Nothing
            Just i -> go (if null qs then i else i - 1) qs (i : acc)
        findDown ci q
            | ci < 0 = Nothing
            | charEqual fold q (cs V.! ci).ch = Just ci
            | otherwise = findDown (ci - 1) q

scorePositions :: V.Vector VChar -> [Int] -> Int
scorePositions _ [] = 0
scorePositions cs pos@(p0 : rest) =
    startScore + consec + gapPenalty
  where
    startScore
        | p0 == 0 = 12
        | boundary (cs V.! (p0 - 1)).ch = 8 - lead
        | otherwise = negate lead
    lead = min p0 10
    consec = sum
        [ if b == a + 1 then 6
          else if boundary (cs V.! (b - 1)).ch then 8 else 0
        | (a, b) <- zip pos rest ]
    pLast = List.foldl' (\_ x -> x) p0 rest
    gapPenalty = negate ((pLast - p0 + 1) - length pos)

-- Exact/prefix/suffix match: best-scoring occurrence.
matchExact :: Bool -> V.Vector VChar -> Term -> Maybe (Int, [Int])
matchExact fold cs term
    | m == 0 || m > n = Nothing
    | otherwise = List.foldl' keep Nothing starts
  where
    tok = term.body
    m = length tok
    n = V.length cs
    starts
        | term.prefix && term.suffix = [0 | m == n]
        | term.prefix = [0]
        | term.suffix = [n - m]
        | otherwise = [0 .. n - m]
    keep acc i
        | not (occursAt i) = acc
        | otherwise =
            let s = scoreExact i
            in case acc of
                Just (sb, _) | sb >= s -> acc
                _ -> Just (s, [i .. i + m - 1])
    occursAt i = and [charEqual fold q (cs V.! (i + j)).ch | (j, q) <- zip [0 ..] tok]
    scoreExact i =
        1000 + m * 6
        + (if term.prefix then 200 else 0)
        + (if term.suffix then 100 else 0)
        + (if i == 0 then 12
           else if boundary (cs V.! (i - 1)).ch then 8 else 0)
        - min i 10
        - (if term.prefix || term.suffix then 0 else n - (i + m))
