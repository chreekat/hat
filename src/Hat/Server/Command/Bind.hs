-- | The @bind-key@\/@unbind-key@ commands: editing the key tables that
-- 'Hat.Server.runKeys' dispatches through.
module Hat.Server.Command.Bind
    ( cmdBind
    , cmdUnbind
    ) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Command.Parser (parseScript)
import Hat.Model
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.Keys
import Hat.Server.Script (FileLoad (..), resolveScript)

-- | @bind-key [-nr] [-T key-table] [-N note] key [command ...]@. Without a
-- command it only re-notes (and with @-r@ marks repeating) a key already
-- bound, as tmux's @key_bindings_add@ does.
cmdBind :: CommandImpl
cmdBind st mclient args = do
    let (opts, flags, pos) = parseArgs "TN" args
        table
            | "-n" `elem` flags = "root"
            | Just t <- lookup "-T" opts = t
            | otherwise = "prefix"
        note = lookup "-N" opts
        repeats = if "-r" `elem` flags then Repeats else Once
    case pos of
        [keyName] | Just key <- parseKeyName keyName -> do
            atomically $ modifyTVar' st.keymap $
                Map.adjust (Map.adjust (renote note repeats) key.name) table
            pure []
        (keyName : rest)
            | Just key <- parseKeyName keyName
            , not (null rest) -> splitBinding st mclient rest >>= \case
                Left e -> pure [RErr e]
                Right cmds -> do
                    atomically $ modifyTVar' st.keymap $
                        Map.insertWith Map.union table
                            (Map.singleton key.name Binding
                                { commands = cmds, repeats, note })
                    pure []
        (keyName : _) ->
            pure [RErr ("bind: bad key or command: " <> keyName)]
        _ -> pure [RErr "usage: bind [-n] [-T table] key command..."]
  where
    renote note repeats b = b
        { note = maybe b.note Just note
        , repeats = if repeats == Repeats then Repeats else b.repeats }

-- A binding's command part: one brace block to parse as a script, its
-- conditionals and assignments run now, or argv split on ";" tokens (from
-- escaped semicolons).
splitBinding :: ServerState -> Maybe Client -> [Text] -> IO (Either Text [[Text]])
splitBinding st mclient = \case
    [block] | T.any (\c -> c == ' ' || c == ';' || c == '\n') block ->
        either (Left . snd) (Right . map snd)
            <$> resolveScript st mclient RunFile (parseScript block)
    rest -> pure (Right (filter (not . null) (splitOnSemis rest)))
  where
    splitOnSemis xs = case break (== ";") xs of
        (before, []) -> [before]
        (before, _ : after) -> before : splitOnSemis after

cmdUnbind :: CommandImpl
cmdUnbind st _ args = do
    let (opts, flags, pos) = parseArgs "T" args
        table
            | "-n" `elem` flags = "root"
            | Just t <- lookup "-T" opts = t
            | otherwise = "prefix"
    case pos of
        [keyName] | Just key <- parseKeyName keyName -> do
            atomically $ modifyTVar' st.keymap $
                Map.adjust (Map.delete key.name) table
            pure []
        _ -> pure [RErr "usage: unbind [-n] [-T table] key"]
