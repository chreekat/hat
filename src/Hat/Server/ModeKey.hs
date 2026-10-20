-- | A key reaching a pane in a mode with no key table binding it (tmux's
-- @window_pane_key@ once a mode holds the pane).
module Hat.Server.ModeKey
    ( paneModeKey
    ) where

import Control.Concurrent.STM

import Hat.Model
import Hat.Server.Command.Types (Dispatch)
import Hat.Server.Keys (Key)
import Hat.Server.PaneMode (resetPaneMode)
import Hat.Server.TreeMode (treeModeKey)

-- | Give a key to the pane's top mode: clock mode ends, a tree mode acts on
-- it, and copy mode, whose keys are its tables', drops it. 'False' when the
-- pane shows no mode, so the key is its program's.
paneModeKey :: Dispatch -> ServerState -> Maybe Client -> Pane -> Key -> IO Bool
paneModeKey d st mclient pane key = atomically (topMode pane) >>= \case
    Nothing -> pure False
    Just ClockMode -> True <$ resetPaneMode st pane
    Just (TreeMode _) -> True <$ treeModeKey d st mclient pane key
    Just (CopyMode _) -> pure True
