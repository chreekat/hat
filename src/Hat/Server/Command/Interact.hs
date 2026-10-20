-- | The interactive client commands: detaching, sending keys to a pane,
-- entering copy-mode, and the command prompt.
module Hat.Server.Command.Interact
    ( cmdDetachClient
    , cmdSendPrefix
    , cmdCopyMode
    , cmdClockMode
    , cmdCommandPrompt
    , promptCursor
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, void, when)
import Data.Maybe (isJust)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.ClientIO (send)
import Hat.Server.Command.Types (CommandImpl, Dispatch (..), Reply (..), parseArgs, unknownFlag)
import Hat.Server.CopyMode qualified as CopyMode
import Hat.Server.FormatEnv (windowFormatEnv)
import Hat.Server.Format (FormatEnv)
import Hat.Server.Keys
import Hat.Server.Locate (clientOptions, clientView, findTarget, findTargetClient, formatClient)
import Hat.Server.Pane (typeKeyIntoPane)
import Hat.Server.PaneMode (enterClockMode, modifyTopMode, resetPaneModeAll, setPaneMode)
import Hat.Server.Command.CopyMode (runCopyModeCommand)
import Hat.Server.MouseIO (clientMouseEvent, mouseEventPane, startSelectionDrag)
import Hat.Server.Target qualified as Target
import Hat.Server.Prompt qualified as Prompt
import Hat.Server.FormatEnv (expandFormat)
import Hat.Term.Emulator qualified as Emu
import Hat.Transport.Wire

-- | @detach-client@: detach the issuing client, or with @-s@ every client
-- attached to a session, or with @-a@ every attached client but the
-- issuing one.
cmdDetachClient :: CommandImpl
cmdDetachClient st mclient args
    | Just e <- unknownFlag "detach-client" "a" flags = pure [RErr e]
    | Just _ <- lookup "-E" opts =
        pure [RErr "detach-client -E: not supported"]
    | Just t <- lookup "-s" opts = do
        res <- findTarget st mclient Target.FindSession (Just t)
        case res of
            -- tmux's source session can fail to resolve: nothing to detach.
            Left _ -> pure []
            Right (sess, _, _, _) -> do
                cs <- atomically (sessionClients st sess.id)
                [] <$ mapM_ (`send` DetachOk) cs
    | otherwise = do
        etarget <- case lookup "-t" opts of
            Just name -> fmap Just <$> formatClient st mclient Nothing (Just name)
            Nothing -> pure (Right mclient)
        case etarget of
            Left e -> pure [RErr e]
            Right mtarget
                | "-a" `elem` flags -> do
                    cs <- Map.elems <$> readTVarIO st.clients
                    others <- atomically $ filterM' cs $ \c -> do
                        sid <- readTVar c.session
                        live <- Map.member sid <$> readTVar st.sessions
                        pure (live && Just c.id /= fmap (.id) mtarget)
                    [] <$ mapM_ (`send` DetachOk) others
                | otherwise -> [] <$ forM_ mtarget (`send` DetachOk)
  where
    (opts, flags, _) = parseArgs "Est" args
    filterM' xs p = map fst . filter snd . zip xs <$> mapM p xs

-- | @send-prefix [-2] [-t pane]@: press the session's @prefix@ (or
-- @prefix2@) in the pane; @None@ presses nothing.
cmdSendPrefix :: CommandImpl
cmdSendPrefix st mclient args = do
    let (opts, flags, _) = parseArgs "t" args
        which = if "-2" `elem` flags then "prefix2" else "prefix"
    res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
    case res of
        Left e -> pure [RErr e]
        Right (sess, _, win, pane) -> do
            o <- resolveForPaneIO st sess win pane
            [] <$ forM_ (parseKeyName (optionText which o)) (typeKeyIntoPane st pane)

cmdCopyMode :: CommandImpl
cmdCopyMode st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
        quit = "-q" `elem` flags
        mouse = "-M" `elem` flags
    -- -M: the pane under the mouse event being acted on, if there is one.
    keyMouse <- clientMouseEvent mclient
    let mev = if mouse then keyMouse else Nothing
    res <- case mev of
        Just ev -> maybe (Left "no mouse target") (\(s, w, p, _) -> Right (s, 0 :: Int, w, p))
            <$> mouseEventPane st ev
        Nothing
            | mouse -> pure (Left "no mouse target")
            | otherwise -> findTarget st mclient Target.FindPane (lookup "-t" opts)
    case res of
        Left _ -> pure []
        Right (_, _, _, pane)
            | quit -> [] <$ resetPaneModeAll st pane
            | otherwise -> do
                let exit = if "-k" `elem` flags then KillPane else KeepPane
                _ <- setPaneMode st pane "copy-mode" (enterCopyMode pane) exit
                -- The flags apply whether or not the mode was already open;
                -- a mouse key opens it without line numbers.
                atomically $ modifyTopMode st pane $ \case
                    CopyMode pm -> CopyMode pm
                        { copyState = pm.copyState
                            { scrollExit = pm.copyState.scrollExit || "-e" `elem` flags
                            , hidePosition = pm.copyState.hidePosition || "-H" `elem` flags
                            , lineNumbers = if isJust keyMouse then LinesHidden else LinesFromOption } }
                    other -> other
                when ("-u" `elem` flags) $
                    void (runCopyModeCommand st pane "page-up" [])
                forM_ ((,) <$> mclient <*> mev) $ \(client, ev) ->
                    startSelectionDrag st client ev
                pure []
  where
    enterCopyMode pane = do
        scr <- Emu.snapshot pane.emulator
        frozen <- CopyMode.freezeGrid pane.emulator
        srvOpts <- readTVarIO st.options
        let table = case srvOpts.modeKeys of
                KeysVi -> "copy-mode-vi"
                KeysEmacs -> "copy-mode"
            startRow = frozen.fgHsize + scr.cursor.row
            startCol = scr.cursor.col
        pure $ CopyMode CopyModeData
            { kind = ModeCopy, frozen = frozen
            , copyState = copyModeAt table startRow startCol }

-- | @clock-mode [-t pane]@.
cmdClockMode :: CommandImpl
cmdClockMode st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
    case res of
        Left e -> pure [RErr e]
        Right (_, _, _, pane) -> [] <$ enterClockMode st pane

-- | @command-prompt [-bFkl] [-I inputs] [-p prompts] [-t target-client]
-- [template]@. Opens the line editor on the target client, asking each
-- comma-separated prompt in turn (@-l@: one prompt, commas and all); @-I@
-- pre-fills each (format-expanded, so @#W@ is the window name), the answers
-- are spliced into the @template@ (see 'Prompt.applyTemplate'; expanded
-- first under @-F@), and @-k@ takes one key, by name, instead of a line. A
-- command from another client waits for the answers and runs the template
-- itself, unless @-b@. Backs the @,@ rename binding:
-- @command-prompt -I "#W" "rename-window '%%'"@.
cmdCommandPrompt :: Dispatch -> CommandImpl
cmdCommandPrompt d st mclient args
    | Just e <- unknownFlag "command-prompt" "1CbeFiklINPptT" flags = pure [RErr e]
    | bad : _ <- [ f | f <- flags, f `notElem` ["-b", "-F", "-k", "-l"] ]
        <> [ "-T" | Just _ <- [lookup "-T" opts] ]
        -- a bare -1 parses as a number
        <> [ "-1" | "-1" `elem` takeWhile ("-" `T.isPrefixOf`) args ] =
        pure [RErr ("command-prompt " <> bad <> ": not supported")]
    | otherwise = findTargetClient st mclient Nothing (lookup "-t" opts) >>= \case
        Left e -> pure [RErr e]
        Right client -> do
            env <- clientPromptEnv st client
            inputs <- case lookup "-I" opts of
                Nothing -> pure []
                Just raw -> mapM (expandFormat st env)
                    (if literal then [raw] else T.splitOn "," raw)
            tmpl <- if "-F" `elem` flags then expandFormat st env tmpl0 else pure tmpl0
            copts <- clientOptions st client
            let waiting = "-b" `notElem` flags && fmap (.id) mclient `notElem` [Nothing, Just client.id]
            mw <- if waiting then Just . PromptWaiter <$> newEmptyTMVarIO else pure Nothing
            let keys = PromptKeys
                    { statusKeys = if optionText "status-keys" copts == "vi"
                        then KeysVi else KeysEmacs
                    , wordSeparators = copts.wordSeparators
                    , insertCursor = promptCursor "prompt-cursor" copts
                    , commandCursor = promptCursor "prompt-command-cursor" copts }
                prompts = zip labels (inputs <> repeat "")
            -- A client already prompting keeps its prompt.
            opened <- atomically $ readTVar client.prompt >>= \case
                Just _ -> pure False
                Nothing -> case prompts of
                    [] -> pure False
                    (label, initial) : rest -> True <$ do
                        writeTVar client.prompt $ Just (Prompt.promptFor label initial tmpl)
                            { promptKeys = keys, queued = rest
                            , promptKind = if "-k" `elem` flags then KeyEntry else LineEntry
                            , waiter = mw }
                        bumpDirty st
            case mw of
                Just (PromptWaiter w) | opened -> do
                    cmd <- outsideCommandBatch st
                        (atomically (takeTMVar w `orElse` abandoned client mw))
                    maybe (pure []) (d.runCommandText st mclient) cmd
                _ -> pure []
  where
    (opts, flags, pos) = parseArgs "IpTt" args
    tmpl0 = T.unwords pos
    literal = "-l" `elem` flags
    labels = case lookup "-p" opts of
        Just p
            | literal -> [p]
            | otherwise -> map (<> " ") (T.splitOn "," p)
        Nothing
            | T.null tmpl0 -> [":"]
            | otherwise -> ["(" <> T.takeWhile (/= ' ') tmpl0 <> ") "]
    -- The prompt closed without answering: its client went, or another
    -- prompt replaced it.
    abandoned client mw = do
        live <- Map.member client.id <$> readTVar st.clients
        cur <- readTVar client.prompt
        if live && fmap (.waiter) cur == Just mw then retry else pure Nothing

-- | A prompt's cursor from its @…-colour@ and @…-style@ options.
promptCursor :: Text -> Options -> PromptCursor
promptCursor prefix opts = PromptCursor
    { colour = optionText (prefix <> "-colour") opts
    , shape = case lookup (optionText (prefix <> "-style") opts) shapes of
        Just s -> s
        Nothing -> CursorDefault }
  where
    shapes = zip
        [ "default", "blinking-block", "block", "blinking-underline"
        , "underline", "blinking-bar", "bar" ]
        [minBound ..]

-- | The format environment of a client's current window, for expanding a
-- @command-prompt -I@ initial string. Empty if the client has no window.
clientPromptEnv :: ServerState -> Client -> IO FormatEnv
clientPromptEnv st client = do
    mv <- atomically (clientView st client)
    case mv of
        Nothing -> pure Map.empty
        Just (sess, win) -> do
            ix <- readTVarIO sess.currentIx
            windowFormatEnv st sess ix win
