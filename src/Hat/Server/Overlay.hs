-- | The overlays that take over a client's keyboard while they are open:
-- the window's menu, the chooser and the command prompt. Each edits its own
-- state until the keystroke that runs a command or cancels.
module Hat.Server.Overlay
    ( handleMenuInput
    , handlePickerInput
    , handlePromptInput
    , closePicker
    ) where

import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forM_, unless)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Model
import Hat.Geometry (Pos (..), Size (..))
import Hat.Model.Options (Options (..), optionNumber)
import Hat.Server.Command.Types (Dispatch (..))
import Hat.Server.Keys (Key (..), KeyState (..))
import Hat.Server.Locate (clientOptions)
import Hat.Server.Menu
    (MenuInput (..), MenuItem (..), MenuOutcome (..), WindowMenu (event, target), menuKey)
import Hat.Server.Mouse (MouseInput (..), MouseState (..), parseMouseInput)
import Hat.Server.View (statusLayout)
import Hat.Server.Picker qualified as Picker
import Hat.Server.Prompt qualified as Prompt
import Hat.Server.Toast (toastReplies)

-- | A key or mouse report while the client's window shows a menu (tmux's
-- @server_client_handle_menu_key@): the menu takes it, and a chosen item's
-- command runs once the menu has closed, from the menu's target and the
-- mouse event that opened it.
handleMenuInput :: Dispatch -> ServerState -> Client -> Window -> Key -> IO ()
handleMenuInput d st client win k = do
    minput <- if k.name == "Mouse" then menuMouseInput st client k
        else pure (Just (MenuKeyPress k.name))
    forM_ minput $ \input -> do
        outcome <- atomically $ readTVar win.menu >>= \case
            Nothing -> pure Nothing
            Just wm -> do
                let o = menuKey input wm
                writeTVar win.menu $ case o of
                    MenuStays wm' -> Just wm'
                    _ -> Nothing
                bumpDirty st
                pure (Just (wm, o))
        forM_ outcome $ \case
            (wm, MenuChose item) -> runMenuCommand d st client wm item.command
            _ -> pure ()

-- | A mouse report as a menu sees it: its button, and its place in the
-- window, none over the status line. The report is the client's last one
-- from now on, whoever took it.
menuMouseInput :: ServerState -> Client -> Key -> IO (Maybe MenuInput)
menuMouseInput st client k = case parseMouseInput k.raw of
    Nothing -> pure Nothing
    Just m -> do
        modifyIORef' client.keyState $ \ks ->
            ks { mouse = ks.mouse { prevX = m.x, prevY = m.y, prevB = m.b } }
        sopts <- clientOptions st client
        csize <- readTVarIO client.size
        o <- (.origin) <$> readTVarIO client.viewport
        let (_, mrow) = statusLayout sopts.statusPosition sopts.statusLines
                (fromIntegral csize.rows)
            inWindow x y = Just (x + o.col, y + o.row)
            at = case mrow of
                Just 0 | m.y < sopts.statusLines -> Nothing
                       | otherwise -> inWindow m.x (m.y - sopts.statusLines)
                Just r | m.y >= r -> Nothing
                _ -> inWindow m.x m.y
        pure (Just (MenuMouseAt m.b at))

-- | Run a chosen menu item's command (tmux's @menu_key@ "chosen" path).
runMenuCommand :: Dispatch -> ServerState -> Client -> WindowMenu -> Text -> IO ()
runMenuCommand d st client wm cmd = do
    ks <- readIORef client.keyState
    writeIORef client.keyState ks { mouseEvent = wm.event, commandState = Just wm.target }
    replies <- withCommandBatch st (d.runCommandText st (Just client) cmd)
        `finally` modifyIORef' client.keyState
            (\after -> after { mouseEvent = Nothing, commandState = Nothing })
    toastReplies st client replies

-- | While a chooser is open it owns every keystroke: navigate/search
-- until Enter (run the item's command and close) or Escape (close). The
-- keys after the one that closed it are returned unread.
handlePickerInput
    :: Dispatch -> ServerState -> Client -> PickerState -> [Key] -> IO [Key]
handlePickerInput _ _ _ _ [] = pure []
handlePickerInput d st client pk0 keys = do
    let go pk = \case
            [] -> (Picker.PickerStay pk, [])
            k : ks -> case Picker.editPicker pk k of
                Picker.PickerStay pk' -> go pk' ks
                done -> (done, ks)
        (result, rest) = go pk0 keys
    rest <$ case result of
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
-- editor consumes them until Enter (run and close) or Escape (close). The
-- keys after the one that closed it are returned unread.
handlePromptInput
    :: Dispatch -> ServerState -> Client -> PromptState -> [Key] -> IO [Key]
handlePromptInput d st client pr0 keys = do
    history <- readTVarIO st.cmdHistory
    let go pr = \case
            [] -> (Prompt.Editing pr, [])
            k : ks -> case Prompt.editPrompt history pr k of
                Prompt.Editing pr' -> go pr' ks
                done -> (done, ks)
        (result, rest) = go pr0 keys
    rest <$ case result of
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
                unless templated $ do
                    limit <- optionNumber "prompt-history-limit" <$> readTVar st.options
                    modifyTVar' st.cmdHistory (Prompt.pushHistory limit line)
                bumpDirty st
            -- A blank submission cancels: never rename a window to "".
            unless (T.null (T.strip line)) $ do
                toastReplies st client =<< d.runCommandText st (Just client) cmd
