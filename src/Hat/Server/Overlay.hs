-- | The overlays that take over a client's keyboard while they are open:
-- the window's menu and the command prompt. Each edits its own state until
-- the keystroke that runs a command or cancels.
module Hat.Server.Overlay
    ( handleMenuInput
    , PromptHome (..)
    , handlePromptInput
    ) where

import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forM_, when)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Model
import Hat.Geometry (Pos (..), Size (..))
import Hat.Model.Options (Options (..), optionNumber)
import Hat.Server.Command.Types (Dispatch (..))
import Hat.Server.HookTypes (HooksState (..))
import Hat.Server.Hooks (PayloadItem (..), notifyPane)
import Hat.Server.Keys (Key (..), KeyState (..))
import Hat.Server.Locate (clientOptions)
import Hat.Server.Menu
    (MenuInput (..), MenuItem (..), MenuOutcome (..), WindowMenu (event, target), menuKey)
import Hat.Server.Mouse (MouseInput (..), MouseState (..), parseMouseInput)
import Hat.Server.View (statusLayout)
import Hat.Server.Prompt qualified as Prompt
import Hat.Server.Toast (showReplies)

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
    showReplies st client replies

-- | Where an open prompt is drawn: on the client's status line, or over
-- a pane (@command-prompt -P@).
data PromptHome = OnStatusLine | OverPane Pane

-- | While a prompt is open it owns every keystroke: the line editor
-- consumes them until one answers or cancels it. The keys after the one
-- that closed it are returned unread, along with a key the prompt closed
-- on but did not take (a @-N@ prompt's non-digit).
handlePromptInput
    :: Dispatch -> ServerState -> Client -> PromptHome -> PromptState -> [Key] -> IO [Key]
handlePromptInput d st client home pr0 keys0 = do
    env <- promptEnv st pr0.promptType
    let go pr = \case
            [] -> [] <$ atomically (store pr)
            k : ks -> case Prompt.editPrompt env pr k of
                Prompt.Editing pr' -> go pr' ks
                Prompt.Answering pr' ans -> do
                    atomically (store pr')
                    run (Prompt.applyTemplate pr'.template (pr'.answers <> [ans]))
                    go pr' ks
                Prompt.Cancel -> ks <$ closeWith pr Nothing
                Prompt.Submit line -> ks <$ submitted pr line True
                Prompt.SubmitPassing line -> (k : ks) <$ submitted pr line False
    go pr0 keys0
  where
    store pr = do
        case home of
            OnStatusLine -> writeTVar client.prompt (Just pr)
            OverPane pane -> modifyTVar' st.panePrompts (Map.insert pane.id pr)
        bumpDirty st
    run cmd = showReplies st client =<< d.runCommandText st (Just client) cmd
    submitted pr line remember = do
        let answered = Prompt.answerPrompt pr line
            -- A blank submission cancels: never rename a window to "".
            blank = T.null (T.strip line)
        runNow <- atomically $ do
            when (remember && pr.promptKind `elem` [LineEntry, BackspaceExitEntry]) $ do
                limit <- optionNumber "prompt-history-limit" <$> readTVar st.options
                modifyTVar' (historyOf pr.promptType) (Prompt.pushHistory limit line)
            case answered of
                Prompt.NextPrompt nxt -> Nothing <$ store nxt
                Prompt.AnswersDone cmd
                    | blank -> Nothing <$ answer pr Nothing
                    | Just _ <- pr.waiter -> Nothing <$ answer pr (Just cmd)
                    | otherwise -> Just cmd <$ answer pr Nothing
        case answered of
            Prompt.NextPrompt _ -> pure ()
            Prompt.AnswersDone _ -> closed
        forM_ runNow run
    closeWith pr cmd = atomically (answer pr cmd) >> closed
    -- Close the prompt, handing a waiting command what it is to run, in the
    -- one transaction, so the waiter never sees it closed unanswered.
    answer pr cmd = do
        case home of
            OnStatusLine -> writeTVar client.prompt Nothing
            OverPane pane -> modifyTVar' st.panePrompts (Map.delete pane.id)
        forM_ pr.waiter $ \(PromptWaiter w) -> tryPutTMVar w cmd
        bumpDirty st
    closed = case home of
        OnStatusLine -> pure ()
        OverPane pane -> notifyPane st "pane-prompt-closed" pane
            [ ("prompt_type", PText (Prompt.promptTypeName pr0.promptType)) ]
    historyOf = \case
        CommandPrompt -> st.cmdHistory
        SearchPrompt -> st.searchHistory

-- | What a prompt of a type edits with: that type's history, and the
-- commands and command aliases @Tab@ completes.
promptEnv :: ServerState -> PromptType -> IO Prompt.PromptEnv
promptEnv st ty = atomically $ do
    history <- readTVar $ case ty of
        CommandPrompt -> st.cmdHistory
        SearchPrompt -> st.searchHistory
    names <- Set.toList <$> readTVar st.hooks.knownCommands
    opts <- readTVar st.options
    let aliases = [ T.takeWhile (/= '=') a | a <- Map.elems opts.commandAlias, T.any (== '=') a ]
    pure Prompt.PromptEnv { history, commands = names <> aliases }
