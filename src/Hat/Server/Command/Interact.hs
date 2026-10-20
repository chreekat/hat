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
import Data.Maybe (fromMaybe, isJust)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.ClientIO (send)
import Hat.Server.Command.Types (CommandImpl, Dispatch (..), Reply (..), parseArgs, parseArgsDigits, unknownFlag)
import Hat.Server.CopyMode qualified as CopyMode
import Hat.Server.FormatEnv (windowFormatEnv)
import Hat.Server.Format (FormatEnv)
import Hat.Server.Keys
import Hat.Server.Locate (clientOptions, clientView, findTarget, findTargetClient, formatClient)
import Hat.Server.Pane (typeKeyIntoPane)
import Hat.Server.PanePrompt (openPanePrompt)
import Hat.Server.Toast (showReplies)
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

-- | @command-prompt [-1bCeFiklNP] [-I inputs] [-p prompts] [-t
-- target-client] [-T prompt-type] [template]@. Opens the line editor on the
-- target client's status line, or with @-P@ over its current pane, asking
-- each comma-separated prompt in turn (@-l@: one prompt, commas and all);
-- @-I@ pre-fills each (format-expanded, so @#W@ is the window name), the
-- answers are spliced into the @template@ (see 'Prompt.applyTemplate';
-- expanded first under @-F@). The prompt takes one key by name (@-k@), one
-- character (@-1@), digits (@-N@), a line answered as it is edited (@-i@),
-- or a line @BSpace@ on empty abandons (@-e@). A command from another
-- client waits for the answers and runs the template itself, unless @-b@
-- (or @-i@). Backs the @,@ rename binding: @command-prompt -I "#W"
-- "rename-window '%%'"@.
cmdCommandPrompt :: Dispatch -> CommandImpl
cmdCommandPrompt d st mclient args
    | Just e <- unknownFlag "command-prompt" "1CbeFiklNP" flags = pure [RErr e]
    | Just t <- lookup "-T" opts, Nothing <- Prompt.parsePromptType t =
        pure [RErr ("unknown type: " <> t)]
    | otherwise = findTargetClient st mclient Nothing (lookup "-t" opts) >>= \case
        Left e -> pure [RErr e]
        Right client -> do
            mhome <- if has "-P"
                then atomically $ clientView st client >>= \case
                    Nothing -> pure Nothing
                    Just (_, win) -> do
                        mpane <- activePane win
                        prompts <- readTVar st.panePrompts
                        pure $ case mpane of
                            Just pane | not (Map.member pane.id prompts) -> Just (Just pane)
                            _ -> Nothing
                else (\shown -> if shown then Nothing else Just Nothing) . isJust
                    <$> readTVarIO client.prompt
            case mhome of
                -- The prompt it would open is open already.
                Nothing -> pure []
                Just mpane -> open client mpane
  where
    (opts, flags, pos) = parseArgsDigits "IpTt" "1" args
    has f = f `elem` flags
    tmpl0 = T.unwords pos
    literal = has "-l"
    kind
        | has "-1" = SingleEntry
        | has "-N" = NumericEntry
        | has "-i" = IncrementalEntry
        | has "-k" = KeyEntry
        | has "-e" = BackspaceExitEntry
        | otherwise = LineEntry
    labels = case lookup "-p" opts of
        Just p
            | literal -> [p]
            | otherwise -> map (<> " ") (T.splitOn "," p)
        Nothing
            | T.null tmpl0 -> [":"]
            | otherwise -> ["(" <> T.takeWhile (/= ' ') tmpl0 <> ") "]
    open client mpane = do
        env <- clientPromptEnv st client
        inputs <- case lookup "-I" opts of
            Nothing -> pure []
            Just raw -> mapM (expandFormat st env)
                (if literal then [raw] else T.splitOn "," raw)
        tmpl <- if has "-F" then expandFormat st env tmpl0 else pure tmpl0
        copts <- clientOptions st client
        let waiting = not (has "-b" || has "-i")
                && fmap (.id) mclient `notElem` [Nothing, Just client.id]
        mw <- if waiting then Just . PromptWaiter <$> newEmptyTMVarIO else pure Nothing
        let keys = PromptKeys
                { statusKeys = if optionText "status-keys" copts == "vi"
                    then KeysVi else KeysEmacs
                , wordSeparators = copts.wordSeparators
                , insertCursor = promptCursor "prompt-cursor" copts
                , commandCursor = promptCursor "prompt-command-cursor" copts }
            prompts = zip labels (inputs <> repeat "")
            ptype = fromMaybe CommandPrompt (lookup "-T" opts >>= Prompt.parsePromptType)
            fresh (label, initial) rest = (Prompt.promptFor label initial tmpl)
                { promptKeys = keys, queued = rest, promptKind = kind, promptType = ptype
                , freeze = if has "-C" then NoFreeze else FreezeScreen
                , waiter = mw }
            -- An incremental prompt starts empty, its input kept for C-r and
            -- C-s to bring back.
            started pr
                | kind == IncrementalEntry = pr { input = "", cursor = 0, lastInput = pr.input }
                | otherwise = pr
        case prompts of
            [] -> pure []
            first : rest -> do
                let pr = started (fresh first rest)
                opened <- case mpane of
                    Nothing -> atomically $ readTVar client.prompt >>= \case
                        Just _ -> pure False
                        Nothing -> True <$ do
                            writeTVar client.prompt (Just pr)
                            bumpDirty st
                    Just pane -> openPanePrompt st pane pr
                -- tmux's prompt_incremental_start.
                when (opened && kind == IncrementalEntry) $
                    showReplies st client =<< d.runCommandText st (Just client)
                        (Prompt.applyTemplate tmpl ["=" <> pr.input])
                case mw of
                    Just (PromptWaiter w) | opened -> do
                        cmd <- outsideCommandBatch st
                            (atomically (takeTMVar w `orElse` abandoned client mpane mw))
                        maybe (pure []) (d.runCommandText st mclient) cmd
                    _ -> pure []
    -- The prompt closed without answering: its client or pane went, or
    -- another prompt replaced it.
    abandoned client mpane mw = do
        live <- Map.member client.id <$> readTVar st.clients
        cur <- case mpane of
            Nothing -> readTVar client.prompt
            Just pane -> Map.lookup pane.id <$> readTVar st.panePrompts
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
