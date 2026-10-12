-- | The interactive client commands: detaching, sending keys to a pane,
-- entering copy-mode, the command prompt, and the choose-tree\/choose-window
-- pickers.
module Hat.Server.Command.Interact
    ( cmdDetachClient
    , cmdSendPrefix
    , cmdCopyMode
    , cmdClockMode
    , cmdCommandPrompt
    , promptCursor
    , cmdChooseTree
    , cmdChooseWindow
    , cmdDisplayPanes
    , buildTreeNodes
    , buildWindowItems
    ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, unless, void, when)
import Data.Maybe (isJust, listToMaybe)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.ClientIO (send)
import Hat.Server.Command.Types (CommandImpl, Dispatch (..), Reply (..), parseArgs, unknownFlag)
import Hat.Server.CopyMode qualified as CopyMode
import Hat.Server.DisplayPanes (armPanes)
import Hat.Server.FormatEnv (windowFormatEnv)
import Hat.Server.Format (FormatEnv)
import Hat.Server.Hooks (PayloadItem (..), notifyPane)
import Hat.Server.Keys
import Hat.Server.Locate (clientOptions, clientView, findTarget, findTargetClient)
import Hat.Server.Pane (enterClockMode, typeKeyIntoPane)
import Hat.Server.Command.CopyMode (runCopyModeCommand)
import Hat.Server.MouseIO (clientMouseEvent, mouseEventPane, startSelectionDrag)
import Hat.Server.Target qualified as Target
import Hat.Server.Picker qualified as Picker
import Hat.Server.Prompt qualified as Prompt
import Hat.Server.ViewMode (leaveViewMode)
import Hat.Server.FormatEnv (expandFormat)
import Hat.Term.Emulator qualified as Emu
import Hat.Transport.Wire

-- | @detach-client@: detach the issuing client, or with @-s@ every client
-- attached to a session, or with @-a@ every attached client but the
-- issuing one.
cmdDetachClient :: CommandImpl
cmdDetachClient st mclient args
    | Just e <- unknownFlag "detach-client" "a" flags = pure [RErr e]
    | Just f <- listToMaybe [ f | (f, _) <- opts, f `elem` ["-E", "-t"] ] =
        pure [RErr ("detach-client " <> f <> ": not supported")]
    | Just t <- lookup "-s" opts = do
        res <- findTarget st mclient Target.FindSession (Just t)
        case res of
            -- tmux's source session can fail to resolve: nothing to detach.
            Left _ -> pure []
            Right (sess, _, _, _) -> do
                cs <- atomically (sessionClients st sess.id)
                [] <$ mapM_ (`send` DetachOk) cs
    | "-a" `elem` flags = do
        cs <- Map.elems <$> readTVarIO st.clients
        others <- atomically $ filterM' cs $ \c -> do
            sid <- readTVar c.session
            live <- Map.member sid <$> readTVar st.sessions
            pure (live && Just c.id /= fmap (.id) mclient)
        [] <$ mapM_ (`send` DetachOk) others
    | otherwise = [] <$ forM_ mclient (`send` DetachOk)
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

-- | @display-panes [-bN] [-d delay-ms] [template]@: overlay each pane's
-- number on the issuing client; a digit press runs @template@ (default
-- @select-pane -t '%%'@) against that pane. @-d@ overrides
-- @display-panes-time@; @-N@ keeps the overlay up on non-selecting keys.
-- @-b@ is tmux's \"don't block the command queue\" — hat never blocks, so
-- it is accepted as already satisfied.
cmdDisplayPanes :: CommandImpl
cmdDisplayPanes st mclient args = do
    let (opts, flags, pos) = parseArgs "d" args
        hold = if "-N" `elem` flags then PanesHold else PanesClose
        template = case pos of
            (t : _) -> t
            [] -> "select-pane -t '%%'"
        mdelay = case lookup "-d" opts of
            Nothing -> Right Nothing
            Just v -> case TR.decimal v of
                Right (n, rest) | T.null rest -> Right (Just n)
                _ -> Left ("display-panes: bad delay: " <> v)
    case (mclient, mdelay) of
        (_, Left e) -> pure [RErr e]
        (Nothing, _) -> pure [RErr "display-panes: no client"]
        (Just client, Right delay) ->
            [] <$ armPanes st client delay hold template

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
            | not quit -> do
                was <- isJust <$> readTVarIO pane.mode
                unless was (enterCopyMode pane)
                -- copy mode comes to the top over a clock
                atomically $ modifyTVar' pane.clock $ \case
                    ClockOnTop -> ClockUnderCopy
                    c -> c
                -- The flags apply whether or not the mode was already open;
                -- a mouse key opens it without line numbers.
                atomically $ modifyTVar' pane.mode $ fmap $ \pm -> pm
                    { copyState = pm.copyState
                        { scrollExit = pm.copyState.scrollExit || "-e" `elem` flags
                        , hidePosition = pm.copyState.hidePosition || "-H" `elem` flags
                        , lineNumbers = if isJust keyMouse then LinesHidden else LinesFromOption } }
                when ("-u" `elem` flags) $
                    void (runCopyModeCommand st pane "page-up" [])
                forM_ ((,) <$> mclient <*> mev) $ \(client, ev) ->
                    startSelectionDrag st client ev
                pure []
            | quit -> do
                was <- atomically $ do
                    old <- readTVar pane.mode
                    writeTVar pane.mode Nothing
                    leaveViewMode pane
                    writeTVar pane.clock NoClock
                    bumpDirty st
                    pure old
                forM_ was $ \pm -> do
                    notifyPane st "pane-mode-changed" pane []
                    notifyPane st "pane-mode-exited" pane
                        [ ("mode_entered", PInt 0)
                        , ("current_mode", PText "")
                        , ("previous_mode", PText (modeName pm.kind)) ]
                pure []
            | otherwise -> pure []
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
                    state = copyModeAt table startRow startCol
                atomically $ do
                    writeTVar pane.mode (Just PaneMode
                        { kind = ModeCopy, frozen = frozen, copyState = state })
                    bumpDirty st
                notifyPane st "pane-mode-changed" pane []
                notifyPane st "pane-mode-entered" pane
                    [ ("mode_entered", PInt 1)
                    , ("current_mode", PText "copy-mode")
                    , ("previous_mode", PText "") ]

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

-- | Open a chooser overlay on the invoking client.
openPicker :: ServerState -> Client -> Text -> PickerFill -> [PickerNode] -> IO ()
openPicker st client titleText fill picked = atomically $ do
    writeTVar client.picker $ Just PickerState
        { title = titleText
        , roots = picked
        , cursor = 0
        , query = ""
        , search = ""
        , mode = Browsing
        , fill = fill
        }
    bumpDirty st

-- | @choose-tree [-GswZ]@: a filterable tree of every session, its
-- windows and their panes; Enter switches to the chosen one. @-s@ opens
-- with sessions collapsed (sessions only), @-w@ with windows expanded but
-- panes collapsed, and neither fully expanded. The config opens it with
-- @… \; send-keys /@ to jump straight into search.
cmdChooseTree :: CommandImpl
cmdChooseTree st mclient args = do
    let (_, flags, _) = parseArgs "" args
        sessionsOnly = "-s" `elem` flags
        windowsOnly  = "-w" `elem` flags
        windowsExp = if sessionsOnly then Collapsed else Expanded
        panesExp   = if sessionsOnly || windowsOnly then Collapsed else Expanded
        fill = if "-Z" `elem` flags then FillWindow else PaneRegion
    forM_ mclient $ \client -> do
        picked <- buildTreeNodes st windowsExp panesExp
        openPicker st client "choose a window" fill picked
    pure []

-- Panes are shown under their window for visual context, but tmux can't name
-- them, so they carry no meaningful search text; the picker marks them
-- (via 'PreviewPane') as non-matching so search never targets them.
buildTreeNodes :: ServerState -> Expansion -> Expansion -> IO [PickerNode]
buildTreeNodes st windowsExp panesExp = do
    sessions <- Map.elems <$> readTVarIO st.sessions
    forM sessions $ \sess -> do
        sname <- readTVarIO sess.name
        ws <- Map.toAscList <$> readTVarIO sess.windows
        winNodes <- forM ws $ \(ix, win) -> do
            wname <- readTVarIO win.name
            apid <- readTVarIO win.activeId
            ordered <- Map.elems <$> readTVarIO win.panes
            let winCmd = "switch-client -t " <> sname
                    <> " ; select-window -t " <> sname <> ":" <> tshow ix
                paneNodes =
                    [ PickerNode
                        { label = "pane " <> tshow pix
                            <> (if pane.id == apid then "*" else "")
                        , command = winCmd <> " ; select-pane -t " <> tshow pix
                        , preview = Just (PreviewPane pane.id)
                        , children = []
                        , expanded = Collapsed }
                    | (pix, pane) <- zip [0 :: Int ..] ordered ]
            pure PickerNode
                { label = tshow ix <> ":" <> wname
                , command = winCmd
                , preview = Just (PreviewWindow win.id)
                , children = Picker.windowChildren paneNodes
                , expanded = panesExp }
        pure PickerNode
            { label = sname
            , command = "switch-client -t " <> sname
            , preview = Just (PreviewSession sess.id)
            , children = winNodes
            , expanded = windowsExp }

-- | @choose-window <template>@: a list of the current session's windows;
-- selecting one runs @template@ with each @%%@ replaced by that window's
-- active pane id, so @choose-window 'join-pane -hs \"%%\"'@ joins it here.
cmdChooseWindow :: CommandImpl
cmdChooseWindow st mclient args = do
    let (_, _, pos) = parseArgs "" args
    case (mclient, pos) of
        (Just client, template : _) -> do
            picked <- buildWindowItems st client template
            openPicker st client "choose a window" PaneRegion picked
            pure []
        _ -> pure [RErr "usage: choose-window template"]

buildWindowItems :: ServerState -> Client -> Text -> IO [PickerNode]
buildWindowItems st client template = do
    sid <- readTVarIO client.session
    msess <- Map.lookup sid <$> readTVarIO st.sessions
    case msess of
        Nothing -> pure []
        Just sess -> do
            ws <- Map.toAscList <$> readTVarIO sess.windows
            forM ws $ \(ix, win) -> do
                wname <- readTVarIO win.name
                apid <- readTVarIO win.activeId
                let target = "%" <> tshow (rawPane apid)
                pure $ Picker.leaf (tshow ix <> ":" <> wname)
                    (T.replace "%%" target template)
