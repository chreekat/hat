-- | fnmatch(3)-style glob matching, shared by target resolution
-- ('Hat.Server.Target'), @update-environment@ ('Hat.Server.Environ') and
-- format @m@\/search modifiers ('Hat.Server.Format'). Standalone (no
-- @Hat.Server@ deps).
module Hat.Glob
    ( globMatch
    , globPaths
    ) where

import Control.Exception (IOException, catch)
import Data.Char (toLower)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesPathExist, listDirectory)

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

-- | glob(3) over the filesystem: the paths an absolute pattern names, in
-- order. A component with no metacharacter is taken as it is; one with
-- some matches the names in its directory, a leading dot only explicitly.
globPaths :: FilePath -> IO [FilePath]
globPaths pat = case pat of
    '/' : rest -> walk "/" (splitPath rest)
    _ -> walk "" (splitPath pat)
  where
    splitPath = filter (not . null) . splitOn
    splitOn s = case break (== '/') s of
        (a, []) -> [a]
        (a, _ : r) -> a : splitOn r
    walk dir [] = do
        ok <- doesPathExist dir
        pure [ dir | ok ]
    walk dir (c : cs)
        | any (`elem` ("*?[" :: String)) c = do
            names <- listDirectory (if null dir then "." else dir)
                `catch` \(_ :: IOException) -> pure []
            let hits = List.sort
                    [ n | n <- names
                    , take 1 n /= "." || take 1 c == "."
                    , globMatch False (T.pack c) (T.pack n) ]
            concat <$> mapM (\n -> walk (dir `under` n) cs) hits
        | otherwise = walk (dir `under` unescape c) cs
    under "" n = n
    under "/" n = '/' : n
    under d n = d <> "/" <> n
    unescape = \case
        '\\' : x : r -> x : unescape r
        x : r -> x : unescape r
        [] -> []
