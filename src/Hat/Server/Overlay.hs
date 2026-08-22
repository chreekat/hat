-- | The overlays that take over a client's keyboard while they are open:
-- the chooser and the command prompt. Each edits its own state until the
-- keystroke that runs a command or cancels.
module Hat.Server.Overlay
    ( handlePickerInput
    , handlePromptInput
    , closePicker
    ) where

import Control.Concurrent.STM
import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.List qualified as List
import Data.Text qualified as T

import Hat.Model
import Hat.Server.Command.Types (Dispatch (..))
import Hat.Server.Keys (Key, tokenizeKeys)
import Hat.Server.Picker qualified as Picker
import Hat.Server.Prompt qualified as Prompt
import Hat.Server.Toast (toastReplies)

-- | While a chooser is open it owns every keystroke: navigate/search
-- until Enter (run the item's command and close) or Escape (close).
handlePickerInput
    :: Dispatch -> ServerState -> Client -> PickerState -> [Key] -> IO ()
handlePickerInput _ _ _ _ [] = pure ()
handlePickerInput d st client pk0 keys = do
    let step acc k = case acc of
            Picker.PickerStay pk -> Picker.editPicker pk k
            done -> done
        result = List.foldl' step (Picker.PickerStay pk0) keys
    case result of
        Picker.PickerStay pk -> atomically $ do
            writeTVar client.picker (Just pk)
            bumpDirty st
        Picker.PickerCancel -> closePicker st client
        Picker.PickerRun line -> do
            closePicker st client
            toastReplies st client =<< d.runCommandText st (Just client) line

closePicker :: ServerState -> Client -> IO ()
closePicker st client = atomically $ do
    writeTVar client.picker Nothing
    bumpDirty st

-- | While the command prompt is open it owns every keystroke: the line
-- editor consumes them until Enter (run and close) or Escape (close).
handlePromptInput
    :: Dispatch -> ServerState -> Client -> PromptState -> B.ByteString -> IO ()
handlePromptInput d st client pr0 bs = do
    history <- readTVarIO st.cmdHistory
    let step acc k = case acc of
            Prompt.Editing pr -> Prompt.editPrompt history pr k
            done -> done
        result = List.foldl' step (Prompt.Editing pr0) (tokenizeKeys bs)
    case result of
        Prompt.Editing pr -> atomically $ do
            writeTVar client.prompt (Just pr)
            bumpDirty st
        Prompt.Cancel -> atomically $ do
            writeTVar client.prompt Nothing
            bumpDirty st
        Prompt.Submit line -> do
            let templated = not (T.null pr0.template)
                cmd = Prompt.applyTemplate pr0.template line
            atomically $ do
                writeTVar client.prompt Nothing
                -- Templated prompts (rename-window, …) keep the : history
                -- clean; only bare command lines are remembered.
                unless templated $
                    modifyTVar' st.cmdHistory (Prompt.pushHistory line)
                bumpDirty st
            -- A blank submission cancels: never rename a window to "".
            unless (T.null (T.strip line)) $ do
                toastReplies st client =<< d.runCommandText st (Just client) cmd
