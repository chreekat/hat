-- | Prompts drawn over a pane instead of the status line (@command-prompt
-- -P@, tmux's @window_pane_set_prompt@): at most one per pane, taking the
-- keys of a client whose window shows it.
module Hat.Server.PanePrompt
    ( openPanePrompt
    , closePanePrompt
    , windowPanePrompt
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, when)
import Data.List qualified as List
import Data.Map.Strict qualified as Map

import Hat.Model
import Hat.Server.Hooks (PayloadItem (..), notifyPane)
import Hat.Server.Prompt (promptTypeName)

-- | Open a prompt over the pane, firing @pane-prompt-opened@; 'False', and
-- nothing opened, when the pane has one open already.
openPanePrompt :: ServerState -> Pane -> PromptState -> IO Bool
openPanePrompt st pane pr = do
    opened <- atomically $ do
        open <- Map.member pane.id <$> readTVar st.panePrompts
        if open then pure False else do
            modifyTVar' st.panePrompts (Map.insert pane.id pr)
            True <$ bumpDirty st
    when opened $ notifyPane st "pane-prompt-opened" pane
        [ ("prompt_type", PText (promptTypeName pr.promptType)) ]
    pure opened

-- | Close the pane's prompt, if it has one, firing @pane-prompt-closed@.
closePanePrompt :: ServerState -> Pane -> IO ()
closePanePrompt st pane = do
    closed <- atomically $ do
        m <- readTVar st.panePrompts
        forM_ (Map.lookup pane.id m) $ \_ -> do
            writeTVar st.panePrompts (Map.delete pane.id m)
            bumpDirty st
        pure (Map.lookup pane.id m)
    forM_ closed $ \pr -> notifyPane st "pane-prompt-closed" pane
        [ ("prompt_type", PText (promptTypeName pr.promptType)) ]

-- | The pane prompt a client's keys go to (tmux's
-- @server_client_handle_key@): its window's active pane's, else the first
-- other pane's in the window with one open.
windowPanePrompt :: ServerState -> Window -> STM (Maybe (Pane, PromptState))
windowPanePrompt st win = do
    prompts <- readTVar st.panePrompts
    active <- readTVar win.activeId
    ps <- windowPanes win
    let open = [ (p, pr) | p <- ps, Just pr <- [Map.lookup p.id prompts] ]
    pure $ case List.find ((== active) . (.id) . fst) open of
        Just hit -> Just hit
        Nothing -> case open of
            hit : _ -> Just hit
            [] -> Nothing
