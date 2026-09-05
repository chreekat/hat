-- | fnmatch(3)-style glob matching, shared by target resolution
-- ('Hat.Server.Target'), @update-environment@ ('Hat.Server.Environ') and
-- format @m@\/search modifiers ('Hat.Server.Format'). Standalone (no
-- @Hat.Server@ deps).
module Hat.Glob
    ( globMatch
    ) where

import Data.Char (toLower)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T

-- | fnmatch(3)-style glob: @*@, @?@, @[...]@ classes (ranges, @!@\/@^@
-- negation), backslash escapes; an unmatched @[@ is a literal. The flag
-- makes the whole match case-insensitive.
globMatch :: Bool -> Text -> Text -> Bool
globMatch icase pat txt = go (prep pat) (prep txt)
  where
    prep = (if icase then map toLower else id) . T.unpack
    go [] [] = True
    go ('*' : ps) cs = any (go ps) (List.tails cs)
    go ('?' : ps) (_ : cs) = go ps cs
    go ('[' : ps) (c : cs) = case charClass ps of
        Just (member, ps') -> member c && go ps' cs
        Nothing -> c == '[' && go ps cs
    go ('\\' : p : ps) (c : cs) = p == c && go ps cs
    go (p : ps) (c : cs) = p == c && go ps cs
    go _ _ = False
    charClass ps0 =
        let (neg, ps1) = case ps0 of
                ('!' : r) -> (True, r)
                ('^' : r) -> (True, r)
                _ -> (False, ps0)
            items acc = \case
                (']' : r) | not (null acc) -> Just (acc, r)
                (a : '-' : b : r) | b /= ']' -> items ((\c -> c >= a && c <= b) : acc) r
                (a : r) -> items ((== a) : acc) r
                [] -> Nothing
        in do
            (tests, rest) <- items [] ps1
            pure (\c -> neg /= any ($ c) tests, rest)
