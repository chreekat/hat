-- | The #{...} format-string engine: a pure evaluator over an
-- environment map. @#(shell)@ segments are delegated to a resolver the
-- caller supplies (the server wires in a cache); strftime @%@
-- sequences are left alone for the caller to post-process.
module Hat.Server.Format
    ( FormatEnv
    , evaluate
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

type FormatEnv = Map Text Text

evaluate :: FormatEnv -> (Text -> Text) -> Text -> Text
evaluate env shellRes = go
  where
    go t = case T.breakOn "#" t of
        (pre, rest)
            | T.null rest -> pre
            | otherwise -> pre <> dispatch (T.drop 1 rest)

    dispatch t = case T.uncons t of
        Nothing -> "#"
        Just ('#', rest) -> "#" <> go rest
        Just ('{', rest) ->
            let (inner, rest') = balanced '{' '}' rest
            in expand inner <> go rest'
        Just ('(', rest) ->
            let (inner, rest') = balanced '(' ')' rest
            in shellRes inner <> go rest'
        Just ('S', rest) -> var "session_name" <> go rest
        Just ('I', rest) -> var "window_index" <> go rest
        Just ('W', rest) -> var "window_name" <> go rest
        Just ('F', rest) -> var "window_flags" <> go rest
        Just ('P', rest) -> var "pane_index" <> go rest
        Just ('H', rest) -> var "host" <> go rest
        Just ('T', rest) -> var "pane_title" <> go rest
        Just (c, rest) -> "#" <> T.singleton c <> go rest

    var name = Map.findWithDefault "" name env

    expand inner
        | Just rest <- T.stripPrefix "?" inner = conditional rest
        | Just rest <- T.stripPrefix "=" inner = truncation rest
        | Just rest <- T.stripPrefix "e|" inner = arith rest
        | otherwise = var inner

    conditional rest = case splitTop rest of
        (cond : thenPart : elseParts) ->
            if truthy (condValue cond)
                then go thenPart
                else go (T.intercalate "," elseParts)
        [cond] -> if truthy (condValue cond) then "" else ""
        [] -> ""
    condValue cond
        | "#" `T.isInfixOf` cond = go cond
        | otherwise = var cond
    truthy v = not (T.null v) && v /= "0"

    truncation rest = case T.breakOn ":" rest of
        (nT, rest') | Just (n, "") <- decimalMaybe nT ->
            T.take n (go ("#{" <> T.drop 1 rest' <> "}"))
        _ -> ""

    arith rest = case T.breakOn ":" rest of
        (opT, rest') -> case splitTop (T.drop 1 rest') of
            [aT, bT] ->
                let ma = number (go aT)
                    mb = number (go bT)
                in case (ma, mb) of
                    (Just a, Just b) -> applyOp opT a b
                    _ -> ""
            _ -> ""
    applyOp op a b = case op of
        ">" -> bool (a > b)
        "<" -> bool (a < b)
        ">=" -> bool (a >= b)
        "<=" -> bool (a <= b)
        "==" -> bool (a == b)
        "!=" -> bool (a /= b)
        "+" -> showNum (a + b)
        "-" -> showNum (a - b)
        "*" -> showNum (a * b)
        "/" -> if b == 0 then "" else showNum (a / b)
        _ -> ""
    bool b = if b then "1" else "0"
    showNum :: Double -> Text
    showNum x
        | x == fromIntegral (round x :: Integer) =
            T.pack (show (round x :: Integer))
        | otherwise = T.pack (show x)
    number t = case TR.signed TR.double t of
        Right (d, restT) | T.null restT -> Just d
        _ -> Nothing
    decimalMaybe t = case TR.decimal t of
        Right (n, restT) -> Just (n :: Int, restT)
        Left _ -> Nothing

-- Split on top-level commas, ignoring commas inside #{...} and #(...).
splitTop :: Text -> [Text]
splitTop = go 0 "" []
  where
    go :: Int -> Text -> [Text] -> Text -> [Text]
    go depth acc parts t = case T.uncons t of
        Nothing -> Prelude.reverse (acc : parts)
        Just (',', rest)
            | depth == 0 -> go 0 "" (acc : parts) rest
        Just (c, rest)
            | c == '{' || c == '(' -> go (depth + 1) (acc <> T.singleton c) parts rest
            | c == '}' || c == ')' -> go (max 0 (depth - 1)) (acc <> T.singleton c) parts rest
            | otherwise -> go depth (acc <> T.singleton c) parts rest

-- Consume up to the matching close bracket; returns (inner, rest).
balanced :: Char -> Char -> Text -> (Text, Text)
balanced open close = go (0 :: Int) ""
  where
    go depth acc t = case T.uncons t of
        Nothing -> (acc, "")
        Just (c, rest)
            | c == close && depth == 0 -> (acc, rest)
            | c == close -> go (depth - 1) (acc <> T.singleton c) rest
            | c == open -> go (depth + 1) (acc <> T.singleton c) rest
            | otherwise -> go depth (acc <> T.singleton c) rest
