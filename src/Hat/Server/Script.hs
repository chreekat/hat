-- | Running a parsed file of commands' conditionals and assignments against
-- the server, as tmux does while it parses the file.
module Hat.Server.Script
    ( FileLoad (..)
    , resolveScript
    ) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Hat.Command.Script (Script (..), evalScript)
import Hat.Model
import Hat.Server.Environ (environSet)
import Hat.Server.Format (FormatCtx (..), FormatEnv, evaluateCtx, truthy)
import Hat.Server.FormatEnv
    (clientContextFormatEnv, clientFormatVars, formatCtxFor, paneFormatEnv)
import Hat.Server.Locate (findTarget, paneIndexOf)
import Hat.Server.Target qualified as Target

-- | Whether a file's commands run, or are only checked (@source-file -n@).
data FileLoad = RunFile | ParseOnly
    deriving (Eq, Show)

-- | A script's commands of the branches taken, each with its line, or the
-- error that stopped its parse. Its assignments are made as it is read
-- (all of them before any error, none when only checking).
resolveScript
    :: ServerState -> Maybe Client -> FileLoad -> Script
    -> IO (Either (Int, Text) [(Int, [Text])])
resolveScript st mclient load script = do
    cmds <- evalScript (fmap truthy . expandCondition st mclient) assign script.items
    pure (maybe (Right cmds) Left script.failure)
  where
    assign vis name value = case load of
        RunFile -> atomically (modifyTVar' st.globalEnviron (environSet vis name value))
        ParseOnly -> pure ()

-- | A condition's expansion (tmux's parse-time @format_expand@): in the
-- context of @mclient@ (its current pane, else the best session's, else
-- none), without running jobs or strftime.
expandCondition :: ServerState -> Maybe Client -> Text -> IO Text
expandCondition st mclient fmt = do
    env <- conditionFormatEnv st mclient
    ctx <- formatCtxFor st env
    pure $! evaluateCtx ctx { shellRes = const "" } fmt

conditionFormatEnv :: ServerState -> Maybe Client -> IO FormatEnv
conditionFormatEnv st mclient = do
    cvars <- maybe (pure []) (clientFormatVars st) mclient
    found <- findTarget st mclient Target.FindPane Nothing
    Map.union (Map.fromList cvars) <$> case found of
        Right (sess, wix, win, pane) -> do
            pix <- paneIndexOf st win pane
            paneFormatEnv st sess wix win pix pane
        Left _ -> clientContextFormatEnv st Nothing
