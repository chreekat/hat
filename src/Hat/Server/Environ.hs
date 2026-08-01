-- | The environment engine (tmux's environ.c): named variables held at a
-- scope — the global environment or a session's — where an entry either
-- carries a value or is cleared (masking an inherited variable without
-- removing it), and may be hidden from plain output.
module Hat.Server.Environ
    ( Environ
    , EnvEntry (..)
    , EnvVisibility (..)
    , EnvForm (..)
    , emptyEnviron
    , environFromPairs
    , environSet
    , environClear
    , environUnset
    , environFind
    , environMerge
    , environPairs
    , environUpdate
    , renderEnvLine
    , globMatch
    ) where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Whether an entry shows in plain output and spawned panes' environments,
-- or only under @show-environment -h@.
data EnvVisibility = EnvVisible | EnvHidden
    deriving (Eq, Show)

-- | One variable: a 'Nothing' value is a cleared entry.
data EnvEntry = EnvEntry
    { value      :: Maybe Text
    , visibility :: EnvVisibility
    }
    deriving (Eq, Show)

-- | The variables at one scope, ordered by name like tmux's red-black tree.
type Environ = Map Text EnvEntry

emptyEnviron :: Environ
emptyEnviron = Map.empty

-- | Seed a scope from plain pairs (a client's or the server's environment).
environFromPairs :: [(Text, Text)] -> Environ
environFromPairs kvs =
    Map.fromList [(k, EnvEntry (Just v) EnvVisible) | (k, v) <- kvs]

-- | Set a variable, replacing any previous value and visibility.
environSet :: EnvVisibility -> Text -> Text -> Environ -> Environ
environSet vis name v = Map.insert name EnvEntry
    { value = Just v, visibility = vis }

-- | Clear a variable: drop its value but keep the entry (and its
-- visibility) so it masks an inherited variable; created if absent.
environClear :: Text -> Environ -> Environ
environClear = Map.alter $ \case
    Just entry -> Just entry { value = Nothing }
    Nothing    -> Just EnvEntry { value = Nothing, visibility = EnvVisible }

-- | Remove a variable entirely.
environUnset :: Text -> Environ -> Environ
environUnset = Map.delete

environFind :: Text -> Environ -> Maybe EnvEntry
environFind = Map.lookup

-- | Overlay one scope on another: overlay entries shadow the base, so a
-- cleared overlay entry masks the base variable of the same name.
environMerge :: Environ -> Environ -> Environ
environMerge base overlay = Map.union overlay base

-- | The pairs a spawned process receives: valued, visible entries only
-- (tmux's environ_push skips hidden and cleared ones).
environPairs :: Environ -> [(Text, Text)]
environPairs env =
    [ (k, v)
    | (k, entry) <- Map.toAscList env
    , entry.visibility == EnvVisible
    , Just v <- [entry.value]
    ]

-- | Fold @update-environment@ patterns from a client's environment into a
-- session's (tmux's environ_update): every client variable matching a
-- pattern is copied in as a visible entry; a pattern matching nothing is
-- cleared in the session, masking the stale value.
environUpdate :: [Text] -> [(Text, Text)] -> Environ -> Environ
environUpdate pats clientEnv env0 = List.foldl' step env0 pats
  where
    step env pat = case [kv | kv@(k, _) <- clientEnv, globMatch pat k] of
        []  -> environClear pat env
        kvs -> List.foldl' (\e (k, v) -> environSet EnvVisible k v e) env kvs

-- | The two output shapes of @show-environment@.
data EnvForm
    = EnvPlain        -- ^ @NAME=value@; cleared as @-NAME@
    | EnvShellExport  -- ^ @NAME="value"; export NAME;@; cleared as @unset NAME;@
    deriving (Eq, Show)

renderEnvLine :: EnvForm -> Text -> EnvEntry -> Text
renderEnvLine EnvPlain name entry = case entry.value of
    Just v  -> name <> "=" <> v
    Nothing -> "-" <> name
renderEnvLine EnvShellExport name entry = case entry.value of
    Just v  -> name <> "=\"" <> shellEscape v <> "\"; export " <> name <> ";"
    Nothing -> "unset " <> name <> ";"

-- POSIX interprets $ ` " and \ inside double quotes; backslash-escape them.
shellEscape :: Text -> Text
shellEscape = T.concatMap $ \c ->
    if c `elem` ("$`\"\\" :: String) then T.pack ['\\', c] else T.singleton c

-- | Shell-style pattern match (fnmatch): @*@ any run, @?@ one character,
-- anything else literal.
globMatch :: Text -> Text -> Bool
globMatch pat = go (T.unpack pat) . T.unpack
  where
    go ('*' : ps) s       = any (go ps) (List.tails s)
    go ('?' : ps) (_ : s) = go ps s
    go (p : ps) (c : s)   = p == c && go ps s
    go [] []              = True
    go _ _                = False
