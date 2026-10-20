-- | A parsed file of commands (tmux's @cmd-parse.y@ statements): commands,
-- environment assignments and the @%if@ conditionals, in order, and the
-- running of its conditionals and assignments ('evalScript').
module Hat.Command.Script
    ( Script (..)
    , ScriptItem (..)
    , evalScript
    ) where

import Data.Text (Text)

import Hat.Server.Environ (EnvVisibility (..))

-- | A file's items, each with its line, up to the error that stopped the
-- parse, if one did.
data Script = Script
    { items   :: [(Int, ScriptItem)]
    , failure :: Maybe (Int, Text)
    }
    deriving (Eq, Show)

data ScriptItem
    = ScriptCommand [Text]
    | ScriptAssign EnvVisibility Text Text  -- ^ @NAME=value@, @%hidden NAME=value@
    | ScriptIf Text                         -- ^ @%if@ and its unexpanded format
    | ScriptElif Text
    | ScriptElse
    | ScriptEndif
    deriving (Eq, Show)

-- | One open conditional, for choosing commands: whether its enclosing
-- branch is taken, whether one of its own branches has been, and whether
-- the current one is.
data Branch = Branch
    { inTaken  :: Bool
    , anyTaken :: Bool
    , taken    :: Bool
    }

-- | Run a script's items as tmux's parser does while it reads them: ask
-- @holds@ of every condition and make every live assignment in file order,
-- and give the commands of the branches taken. An assignment is live by
-- tmux's scope flags, not by the branch taken: each @%if@ and @%elif@
-- flags its own condition, an @%else@ the negation of the flag before it,
-- and every enclosing flag must be set.
evalScript
    :: Monad m
    => (Text -> m Bool) -> (EnvVisibility -> Text -> Text -> m ())
    -> [(Int, ScriptItem)] -> m [(Int, [Text])]
evalScript holds assign = go [] [] []
  where
    go _ _ acc [] = pure (reverse acc)
    go flags branches acc ((n, item) : rest) = case item of
        ScriptCommand argv
            | live branches -> go flags branches ((n, argv) : acc) rest
            | otherwise -> go flags branches acc rest
        ScriptAssign vis name value -> do
            if and flags then assign vis name value else pure ()
            go flags branches acc rest
        ScriptIf fmt -> do
            t <- holds fmt
            go (t : flags) (Branch (live branches) t t : branches) acc rest
        ScriptElif fmt -> do
            t <- holds fmt
            go (t : drop 1 flags) (map (elif t) (take 1 branches) <> drop 1 branches) acc rest
        ScriptElse ->
            go (map not (take 1 flags) <> drop 1 flags)
                (map (elif True) (take 1 branches) <> drop 1 branches) acc rest
        ScriptEndif -> go (drop 1 flags) (drop 1 branches) acc rest
    live = \case
        b : _ -> b.inTaken && b.taken
        [] -> True
    elif t b = b { anyTaken = b.anyTaken || t, taken = not b.anyTaken && t }
