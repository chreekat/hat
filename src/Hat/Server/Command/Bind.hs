-- | The @bind-key@\/@unbind-key@ commands: editing the key tables that
-- 'Hat.Server.runKeys' dispatches through.
module Hat.Server.Command.Bind
    ( cmdBind
    , cmdUnbind
    ) where

import Control.Concurrent.STM (atomically, modifyTVar')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Hat.Command.Parser (parseConfig)
import Hat.Model
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.Keys

cmdBind :: CommandImpl
cmdBind st _ args = do
    let (opts, flags, pos) = parseArgs "TN" args
        table
            | "-n" `elem` flags = "root"
            | Just t <- lookup "-T" opts = t
            | otherwise = "prefix"
    case pos of
        (keyName : rest)
            | Just key <- parseKeyName keyName
            , not (null rest) -> do
                let cmds = splitBinding rest
                atomically $ modifyTVar' st.keymap $
                    Map.insertWith Map.union table
                        (Map.singleton key.name cmds)
                pure []
        (keyName : _) ->
            pure [RErr ("bind: bad key or command: " <> keyName)]
        _ -> pure [RErr "usage: bind [-n] [-T table] key command..."]

-- A binding's command part: one brace block to re-parse, or argv split
-- on ";" tokens (from escaped semicolons).
splitBinding :: [Text] -> [[Text]]
splitBinding = \case
    [block] | T.any (\c -> c == ' ' || c == ';' || c == '\n') block ->
        case parseConfig block of
            Right cmds -> cmds
            Left _ -> [[block]]
    rest -> filter (not . null) (splitOnSemis rest)
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
