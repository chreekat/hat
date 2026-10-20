-- | The command engine: the name-to-implementation table every command
-- reaches the server through -- a config line, a key binding, a control
-- client, @hat <command>@ -- and the implementations that need the table
-- itself (@source-file@, @if-shell@) or the server's own life
-- (@kill-server@, @restart-server@).
module Hat.Server.Dispatch
    ( dispatch
    , runArgv
    , runCommands
    , runCommandText
    , resolveCommand
    , commandTable
    , installHooks
    , readConfigUtf8
    , cleanupInherited
    , cmdSourceFile
    , loadCommandFile
    , FileLoad (..)
    , cmdRestartServer
    , cmdRestart
    , cmdRestartClient
    , cmdKillServer
    , shutDownServer
    , cmdSendKeys
    , ReloadScope (..)
    , reloadFarewell
    , ReloadRequest (..)   -- ^ exported for the reload argument-parse test
    , parseReloadArgs      -- ^ exported for the reload argument-parse test
    , RestartClientOutcome (..)
    , restartClientAction
    ) where

import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (filterM, forM, forM_, replicateM, unless, void, when)
import Data.IORef (readIORef)
import Data.ByteString qualified as B
import Data.Char (isAlpha, isAlphaNum)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.Read qualified as TR
import System.Directory
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..))
import System.Posix.IO qualified as PIO
import System.Posix.Process (executeFile)
import System.Posix.Signals (sigHUP, signalProcess)
import System.Posix.Types (Fd (..))
import System.IO (IOMode (..), openFile)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Process
import Text.Read (readMaybe)
import Hat.Command.Parser (parseCommandLine, parseConfigLines)
import Hat.Log
import Hat.Server.Environ
import Hat.Model
import Hat.Model.Options (lookupCommandAlias, quoteIfNeeded)
import Hat.Path (expandTilde)
import Hat.Server.Reload (ReloadCleanup (..), encodeHandover)
import Hat.Server.Command.Bind (cmdBind, cmdUnbind)
import Hat.Server.Command.ListKeys (cmdListKeys, printCommands)
import Hat.Server.Command.ShowMessages (cmdShowMessages)
import Hat.Server.Command.Lock (cmdLockClient, cmdLockServer, cmdLockSession)
import Hat.Server.MessageLog (logCommand)
import Hat.Server.Command.CopyMode (runCopyModeCommand)
import Hat.Server.Command.Hook (cmdSetHook, cmdShowHooks, resolveHookTarget)
import Hat.Server.Command.Wait (cmdWaitFor)
import Hat.Server.Hooks
    (PayloadItem (..), inHook, installEventSink, installHookEngine, notify)
import Hat.Server.Command.Buffer
import Hat.Server.Command.Menu (cmdDisplayMenu)
import Hat.Server.Command.Popup (cmdDisplayPopup)
import Hat.Server.PopupIO (closeAllPopups)
import Hat.Server.Command.Interact
import Hat.Server.DisplayPanes (cmdDisplayPanes, panesModeMouse)
import Hat.Server.Command.Layout
import Hat.Server.Command.Option
import Hat.Server.Command.Pane
import Hat.Server.Command.Session
import Hat.Server.Command.Window
import Hat.Server.Command.Types
import Hat.Server.ClientIO (broadcast, farewellGrace, send, sendFarewells)
import Hat.Server.ColorScheme
import Hat.Server.Control (cmdRefreshClient, controlSink)
import Hat.Server.ControlQueue (controlNotify)
import Hat.Server.FormatEnv (clientContextFormatEnv, clientFormatVars, expandFormat, paneFormatEnv)
import Hat.Server.Handover
import Hat.Server.Toast
import Hat.Server.ConfigCauses (addConfigCause, printConfigCauses)
import Hat.Glob (globPaths)
import Hat.Server.ViewMode (ViewText (..), viewModeAdd, viewToClient)
import Hat.Server.Keys
import Hat.Server.PaneMode (dropModesForReload)
import Hat.Server.TreeMode (cmdChooseBuffer, cmdChooseClient, cmdChooseTree, cmdCustomizeMode)
import Hat.Server.ModeKey (paneModeKey)
import Hat.Server.Locate
import Hat.Server.Format (FormatEnv)
import Hat.Server.Mouse (mouseAt)
import Hat.Server.MouseIO
    (clientMouseEvent, mouseEventPane, mouseFormatVars, moveCopyCursor, sendMouse
    , startSelectionDrag)
import Hat.Server.Pane
import Hat.Server.PromptHistory (saveHistory)
import Hat.Server.Snapshot
import Hat.Server.Resize (announceResized)
import Hat.Server.Target qualified as Target
import Hat.Term.Emulator qualified as Emu
import Hat.Transport.Wire

-- | Read a config file as UTF-8, independent of the process locale and
-- tolerant of malformed bytes. 'TIO.readFile' decodes with the locale
-- encoding, so under a non-UTF-8 locale a config with any non-ASCII byte
-- (a @·@ separator, a @👀@ marker) threw mid-read and aborted the whole
-- startup — read the bytes and decode UTF-8 leniently instead.
readConfigUtf8 :: FilePath -> IO Text
readConfigUtf8 p = TE.decodeUtf8With TEE.lenientDecode <$> B.readFile p

-- The command engine ---------------------------------------------------------

dispatch :: Dispatch
dispatch = Dispatch
    { runArgv = runArgv
    , runCommands = runCommands
    , runCommandText = runCommandText
    , unknownCommand = unknownCommand
    , newEditorPane = cmdNewEditorPane }

unknownCommand :: ServerState -> [Text] -> IO (Maybe Text)
unknownCommand _ [] = pure Nothing
unknownCommand st argv@(name : _)
    | Just _ <- envAssignment argv = pure Nothing
    | otherwise = do
        opts <- readTVarIO st.options
        pure $ case lookupCommandAlias opts name of
            Just _ -> Nothing
            Nothing -> either Just (const Nothing) (resolveCommand name)

-- | Parse a command line and run it, as tmux's @cmd_parse_from_string@
-- does: a command no name resolves fails the whole line before any of it
-- runs. Run for a command read from a file, its errors name that file and
-- line (see 'loadCommandFile').
runCommandText :: ServerState -> Maybe Client -> Text -> IO [Reply]
runCommandText st mclient input = do
    msource <- currentCommandSource st
    let located e = maybe e (\(path, n) -> sourceLocated path n e) msource
    case parseCommandLine input of
        Left err -> pure [RErr (located err)]
        Right cmds -> do
            bad <- firstUnknown st cmds
            case bad of
                Just e -> pure [RErr (located e)]
                Nothing -> runCommands st mclient cmds

-- | The first command among @cmds@ no name resolves, as its error.
firstUnknown :: ServerState -> [[Text]] -> IO (Maybe Text)
firstUnknown st cmds = listToMaybe . catMaybes <$> mapM (unknownCommand st) cmds

-- | An error at a file's line, as tmux words it.
sourceLocated :: FilePath -> Int -> Text -> Text
sourceLocated path n e = T.pack path <> ":" <> tshow n <> ": " <> e

-- | The file and line of the command this thread runs, if it came from one.
currentCommandSource :: ServerState -> IO (Maybe (FilePath, Int))
currentCommandSource st = listToMaybe <$> commandSourceStack st

-- | The files this thread's commands are nested in, innermost first.
commandSourceStack :: ServerState -> IO [(FilePath, Int)]
commandSourceStack st = do
    tid <- myThreadId
    Map.findWithDefault [] tid <$> readTVarIO st.commandSources

-- | Run @act@ as the command at @path@'s line @n@.
withCommandSource :: ServerState -> FilePath -> Int -> IO a -> IO a
withCommandSource st path n act = do
    tid <- myThreadId
    let alter f = atomically (modifyTVar' st.commandSources (Map.alter f tid))
        push = Just . ((path, n) :) . fromMaybe []
        pop = \case
            Just (_ : outer@(_ : _)) -> Just outer
            _ -> Nothing
    bracket_ (alter push) (alter pop) act

-- | Load a file of commands (tmux's @load_cfg@): it is parsed whole, and
-- runs only if every command in it parses and names a command, each
-- command as the one at its file and line. A file that fails is a
-- configuration cause, as is every error of a clientless load's commands,
-- whose output goes nowhere. Under 'ParseOnly' nothing runs.
loadCommandFile :: ServerState -> Maybe Client -> FileLoad -> FilePath -> Text -> IO [Reply]
loadCommandFile st mclient load path contents = case parseConfigLines contents of
    Left (n, err) -> [] <$ cause n (parseErrorLine err)
    Right cmds -> do
        bad <- listToMaybe . catMaybes <$>
            mapM (\(n, argv) -> fmap (n,) <$> unknownCommand st argv) cmds
        case (bad, load) of
            (Just (n, e), _) -> [] <$ cause n e
            (Nothing, ParseOnly) -> pure []
            (Nothing, RunFile) -> fmap concat . forM cmds $ \(n, argv) -> do
                replies <- withCommandSource st path n (runArgv st mclient argv)
                case mclient of
                    Just _ -> pure replies
                    Nothing -> [] <$ forM_ [ e | RErr e <- replies ] (cause n)
  where
    cause n e = atomically (addConfigCause st (sourceLocated path n e))

-- | Whether 'loadCommandFile' runs the commands it parses (@source-file -n@
-- only checks them).
data FileLoad = RunFile | ParseOnly
    deriving (Eq, Show)

-- | A parse error, on one line: the parser's own words without its picture
-- of the input.
parseErrorLine :: Text -> Text
parseErrorLine err = case filter keep (T.lines err) of
    [] -> T.strip err
    ls -> T.intercalate ", " (map T.strip ls)
  where
    keep l = not (T.null (T.strip l)) && not ("|" `T.isInfixOf` l) && not ("<command>" `T.isPrefixOf` l)

runCommands :: ServerState -> Maybe Client -> [[Text]] -> IO [Reply]
runCommands st mclient cmds =
    withCommandBatch st (concat <$> mapM (runArgv st mclient) cmds)

runArgv :: ServerState -> Maybe Client -> [Text] -> IO [Reply]
runArgv _ _ [] = pure []
runArgv st mclient (name : args) = do
    -- Attribute every command to its issuing client, so a duplicate or
    -- unexpected control command (e.g. a second restart-server nobody typed)
    -- is traceable to its source.
    logEvent st.logger CommandRun
        { client = maybe (-1) (\c -> rawClient c.id) mclient
        , command = T.unwords (name : args) }
    case envAssignment (name : args) of
        Just (vis, n, v) -> [] <$ atomically
            (modifyTVar' st.globalEnviron (environSet vis n v))
        Nothing -> do
            -- Like tmux, a command-alias match beats the builtin table.
            opts <- readTVarIO st.options
            case lookupCommandAlias opts name of
                Just body -> runAlias st mclient body args
                Nothing -> runBuiltin st mclient name args

-- | Run an alias expansion: the body parses as a command sequence, the
-- invocation's own arguments append to its last command, and the expanded
-- names resolve as builtins only, so an alias never recurses.
runAlias :: ServerState -> Maybe Client -> Text -> [Text] -> IO [Reply]
runAlias st mclient body args = case parseCommandLine body of
    Left err -> pure [RErr err]
    Right [] -> pure []
    Right cmds -> do
        let expanded = init cmds <> [last cmds <> args]
        concat <$> mapM run expanded
  where
    run [] = pure []
    run (n : as) = runBuiltin st mclient n as

-- | Dispatch one command through the builtin table, then fire its
-- @after-*@ hook (on success) or @command-error@ (on failure).
runBuiltin :: ServerState -> Maybe Client -> Text -> [Text] -> IO [Reply]
runBuiltin st mclient name0 args = case resolveCommand name0 of
    Left e -> pure [RErr e]
    Right name | Just impl <- Map.lookup name commandTable -> do
        logCommand st mclient (printCommands [name0 : args])
        replies <- impl st mclient args
            `catch` \(e :: SomeException) ->
                pure [RErr (name <> ": " <> T.pack (show e))]
        announceResized st
        fireCommandHooks st mclient name args replies
        pure replies
    Right name -> pure [RErr ("unknown command: " <> name)]

-- | tmux's command lookup: an exact name or alias, else the one command
-- whose name the word begins (the candidates listed when there are more).
-- Hat's own commands answer only to their full names, so they never make
-- an abbreviation tmux accepts ambiguous.
resolveCommand :: Text -> Either Text Text
resolveCommand name
    | Map.member name commandTable = Right name
    | otherwise = case List.sort [ c | (c : _, _) <- commandSpecs
                                     , c `notElem` hatOnlyCommands
                                     , name `T.isPrefixOf` c ] of
        [c] -> Right c
        [] -> Left ("unknown command: " <> name)
        cs -> Left ("ambiguous command: " <> name <> ", could be: "
            <> T.intercalate ", " cs)

-- Commands run from inside a hook fire nothing (tmux's NOHOOKS state).
fireCommandHooks
    :: ServerState -> Maybe Client -> Text -> [Text] -> [Reply] -> IO ()
fireCommandHooks st mclient name args replies = do
    suppressed <- inHook st
    unless suppressed $ do
        let canonical = Map.findWithDefault name name canonicalNames
            spec = hookArgSpec canonical
            failed = not (null [ e | RErr e <- replies ])
            (optsRev, boolFlags, pos) = parseArgs spec args
        tgt <- resolveHookTarget st mclient (lookup "-t" optsRev)
        notify st (if failed then "command-error" else "after-" <> canonical)
            tgt (argsPayload (reverse optsRev) boolFlags pos)

-- | Canonical spelling for every command alias, so @renamew@ fires
-- @after-rename-window@.
canonicalNames :: Map.Map Text Text
canonicalNames = Map.fromList
    [ (alias, canon)
    | (names, _) <- commandSpecs
    , canon : _ <- [names]
    , alias <- names ]

-- The value-taking flags each command's own parser uses, for rebuilding
-- its argument payload; commands not listed only take @-t@.
hookArgSpec :: Text -> [Char]
hookArgSpec = \case
    "new-session" -> "sctnxy"
    "attach-session" -> "tc"
    "new-window" -> "nct"
    "split-window" -> "BceFlmpRsStT"
    "new-pane" -> "BceFlmpRsStTxXyY"
    "join-pane" -> "lpst"
    "move-pane" -> "lPstXYz"
    "break-pane" -> "FnstxXyY"
    "kill-pane" -> "ft"
    "resize-pane" -> "txy"
    "respawn-pane" -> "cet"
    "respawn-window" -> "cet"
    "capture-pane" -> "bESt"
    "select-pane" -> "tT"
    "copy-mode" -> "st"
    "swap-pane" -> "st"
    "move-window" -> "st"
    "link-window" -> "st"
    "swap-window" -> "st"
    "kill-window" -> "ft"
    "resize-window" -> "txy"
    "send-keys" -> "tN"
    "set-buffer" -> "btn"
    "delete-buffer" -> "b"
    "show-buffer" -> "b"
    "save-buffer" -> "b"
    "load-buffer" -> "bt"
    "paste-buffer" -> "bst"
    "list-buffers" -> "FfO"
    "list-clients" -> "FfOt"
    "display-message" -> "cdFt"
    "run-shell" -> "cdst"
    "list-sessions" -> "F"
    "list-windows" -> "Ft"
    "list-panes" -> "Ft"
    "bind-key" -> "TN"
    "unbind-key" -> "T"
    "command-prompt" -> "IpTt"
    "display-panes" -> "dst"
    "set-hook" -> "tB"
    "show-hooks" -> "tF"
    "wait-for" -> "Fw"
    _ -> "t"

-- tmux's args_print payload: the whole argument list (flags in flag order,
-- then positionals), each positional, and each flag's value(s).
argsPayload :: [(Text, Text)] -> [Text] -> [Text] -> [(Text, PayloadItem)]
argsPayload opts boolFlags pos =
    ("arguments", PText printed)
    : [ ("argument_" <> tshow i, PText a) | (i, a) <- zip [0 :: Int ..] pos ]
    <> concatMap flagItems flagChars
  where
    flagChars = List.sort . List.nub $
        [ c | (f, _) <- opts, Just (_, c) <- [T.unsnoc f] ]
        <> [ c | f <- boolFlags, Just (_, c) <- [T.unsnoc f] ]
    valsOf c = [ v | (f, v) <- opts, f == dash c ]
    dash c = T.pack ['-', c]
    flagItems c = case valsOf c of
        [] -> [("flag_" <> T.singleton c, PText "1")]
        vs -> ("flag_" <> T.singleton c, PText (last vs))
            : [ ("flag_" <> T.singleton c <> "_" <> tshow i, PText v)
              | (i, v) <- zip [0 :: Int ..] vs ]
    printed = T.unwords (concatMap flagTokens flagChars <> map quoteArg pos)
    flagTokens c = case valsOf c of
        [] -> [dash c]
        vs -> concat [ [dash c, quoteArg v] | v <- vs ]
    quoteArg = quoteIfNeeded

-- | The config assignment forms (tmux's environ_put): a bare @NAME=value@
-- line sets a global environment variable, @%hidden NAME=value@ a hidden
-- one. Anything not shaped exactly like these stays a command lookup, so a
-- typo still fails loud.
envAssignment :: [Text] -> Maybe (EnvVisibility, Text, Text)
envAssignment = \case
    [w]            -> assign EnvVisible w
    ["%hidden", w] -> assign EnvHidden w
    _              -> Nothing
  where
    assign vis w = case T.breakOn "=" w of
        (n, rest)
            | Just v <- T.stripPrefix "=" rest
            , isVarName n -> Just (vis, n, v)
        _ -> Nothing
    isVarName n = case T.uncons n of
        Just (c, cs) -> (isAlpha c || c == '_')
            && T.all (\x -> isAlphaNum x || x == '_') cs
        Nothing -> False

-- | Wire the hooks engine to this command table: hook commands run
-- clientless through 'runCommandText', and @after-*@ hook names validate
-- against the canonical command names. Every event also reaches the
-- control-mode clients.
installHooks :: ServerState -> IO ()
installHooks st = do
    installHookEngine st
        (\txt -> void (runCommandText st Nothing txt))
        (expandFormat st)
        [ n | (n : _, _) <- commandSpecs ]
    installEventSink st (controlSink st)

-- | Commands tmux does not have.
hatOnlyCommands :: [Text]
hatOnlyCommands =
    [ "activity-window", "list-snapshots", "restore-snapshot"
    , "restart-server", "restart-client", "restart" ]

commandTable :: Map.Map Text CommandImpl
commandTable = Map.fromList (concatMap expand commandSpecs)
  where
    expand (names, impl) = [(n, impl) | n <- names]

commandSpecs :: [([Text], CommandImpl)]
commandSpecs =
    [ (["bind-key", "bind"], cmdBind)
    , (["list-keys", "lsk"], cmdListKeys)
    , (["unbind-key", "unbind"], cmdUnbind)
    , (["set-option", "set"], cmdSet DefaultSession)
    , (["set-window-option", "setw"], cmdSet DefaultWindow)
    , (["show-options", "show", "show-option"], cmdShow)
    , (["set-environment", "setenv"], cmdSetEnvironment)
    , (["show-environment", "showenv"], cmdShowEnvironment)
    , (["source-file", "source"], cmdSourceFile)
    , (["new-window", "neww"], cmdNewWindow)
    , (["select-window", "selectw"], cmdSelectWindow)
    , (["next-window", "next"], cmdNextWindow)
    , (["previous-window", "prev"], cmdPrevWindow)
    , (["last-window", "last"], cmdLastWindow)
    , (["activity-window"], cmdActivityWindow)
    , (["kill-window", "killw"], cmdKillWindow)
    , (["rename-window", "renamew"], cmdRenameWindow)
    , (["move-window", "movew"], cmdMoveWindow)
    , (["link-window", "linkw"], cmdLinkWindow)
    , (["unlink-window", "unlinkw"], cmdUnlinkWindow)
    , (["swap-window", "swapw"], cmdSwapWindow)
    , (["rotate-window", "rotatew"], cmdRotateWindow)
    , (["split-window", "splitw"], cmdSplitWindow)
    , (["new-pane", "newp"], cmdNewPane)
    , (["select-pane", "selectp"], cmdSelectPane)
    , (["kill-pane", "killp"], cmdKillPane)
    , (["respawn-pane", "respawnp"], cmdRespawnPane)
    , (["respawn-window", "respawnw"], cmdRespawnWindow)
    , (["swap-pane", "swapp"], cmdSwapPane)
    , (["clear-history", "clearhist"], cmdClearHistory)
    , (["break-pane", "breakp"], cmdBreakPane)
    , (["join-pane", "joinp"], cmdJoinPane)
    , (["move-pane", "movep"], cmdMovePane)
    , (["select-layout", "selectl"], cmdSelectLayout)
    , (["next-layout", "nextl"], cmdNextLayout)
    , (["previous-layout", "prevl"], cmdPreviousLayout)
    , (["resize-pane", "resizep"], cmdResizePane)
    , (["last-pane", "lastp"], cmdLastPane)
    , (["detach-client", "detach"], cmdDetachClient)
    , (["refresh-client", "refresh"], cmdRefreshClient)
    , (["send-prefix"], cmdSendPrefix)
    , (["send-keys", "send"], cmdSendKeys)
    , (["copy-mode"], cmdCopyMode)
    , (["clock-mode"], cmdClockMode)
    , (["command-prompt"], cmdCommandPrompt dispatch)
    , (["choose-tree"], cmdChooseTree)
    , (["choose-buffer"], cmdChooseBuffer)
    , (["choose-client"], cmdChooseClient)
    , (["customize-mode"], cmdCustomizeMode)
    , (["display-panes", "displayp"], cmdDisplayPanes)
    , (["display-menu", "menu"], cmdDisplayMenu)
    , (["display-popup", "popup"], cmdDisplayPopup)
    , (["show-buffer", "showb"], cmdShowBuffer)
    , (["set-buffer", "setb"], cmdSetBuffer)
    , (["list-buffers", "lsb"], cmdListBuffers)
    , (["delete-buffer", "deleteb"], cmdDeleteBuffer)
    , (["load-buffer", "loadb"], cmdLoadBuffer)
    , (["save-buffer", "saveb"], cmdSaveBuffer)
    , (["paste-buffer", "pasteb"], cmdPasteBuffer)
    , (["pipe-pane", "pipep"], cmdPipePane)
    , (["new-session", "new"], cmdNewSession)
    , (["attach-session", "attach"], cmdAttachSession)
    , (["kill-session"], cmdKillSession)
    , (["has-session", "has"], cmdHasSession)
    , (["start-server", "start"], cmdStartServer)
    , (["rename-session", "rename"], cmdRenameSession)
    , (["list-clients", "lsc"], cmdListClients)
    , (["list-sessions", "ls"], cmdListSessions)
    , (["list-windows", "lsw"], cmdListWindows)
    , (["list-panes", "lsp"], cmdListPanes)
    , (["capture-pane", "capturep"], cmdCapturePane)
    , (["resize-window", "resizew"], cmdResizeWindow)
    , (["switch-client", "switchc"], cmdSwitchClient)
    , (["kill-server"], cmdKillServer)
    , (["list-snapshots"], cmdListSnapshots)
    , (["restore-snapshot"], cmdRestoreSnapshot)
    , (["restart-server"], cmdRestartServer)
    , (["restart-client"], cmdRestartClient)
    , (["restart"], cmdRestart)
    , (["display-message", "display"], cmdDisplayMessage)
    , (["run-shell", "run"], cmdRunShell)
    , (["if-shell", "if"], cmdIfShell)
    , (["set-hook"], cmdSetHook)
    , (["show-hooks"], cmdShowHooks)
    , (["wait-for", "wait"], cmdWaitFor)
    , (["show-messages", "showmsgs"], cmdShowMessages)
    , (["lock-server", "lock"], cmdLockServer)
    , (["lock-session", "locks"], cmdLockSession)
    , (["lock-client", "lockc"], cmdLockClient)
    ]

-- Command implementations.

-- | Whether @set@\/@set-option@ (session) or @setw@\/@set-window-option@
-- (window) invoked the set: the default scope when no @-g@\/@-s@\/@-w@ picks
-- one. See 'cmdSet'.
-- | @source-file [-Fnq] path ...@ (tmux's @cmd-source-file.c@): each path
-- a glob, relative to the client's directory, and @-@ the command client's
-- standard input; each file loads as 'loadCommandFile' does, and once the
-- server has loaded its own configuration the causes are printed to the
-- client.
cmdSourceFile :: CommandImpl
cmdSourceFile st mclient args
    | Just e <- unknownFlag "source-file" "Fnqv" flags = pure [RErr e]
    | "-v" `elem` flags = pure [RErr "source-file -v: not supported"]
    | null pos = pure [RErr "too few arguments (need at least 1)"]
    | otherwise = do
        depth <- length <$> commandSourceStack st
        if depth >= 50 then pure [RErr "too many nested files"] else do
            env <- clientContextFormatEnv st mclient
            found <- forM pos $ \arg -> do
                path <- if "-F" `elem` flags then expandFormat st env arg else pure arg
                matches path
            loaded <- forM found $ \case
                Left e -> pure [RErr e]
                Right paths -> concat <$> forM paths loadOne
            phase <- readTVarIO st.startupPhase
            causes <- if phase == LoadingConfig then pure []
                else printConfigCauses st mclient
            pure (concat loaded <> causes)
  where
    (_, flags, pos) = parseArgs "t" args
    load = if "-n" `elem` flags then ParseOnly else RunFile
    quiet = "-q" `elem` flags
    matches "-" = pure (Right ["-"])
    matches path = do
        p <- expandTilde st.home (T.unpack path)
        let cwd = maybe "" (T.unpack . (.cwd)) mclient
            pat = if take 1 p == "/" then p else cwd <> "/" <> p
        hits <- globPaths pat
        pure $ case hits of
            [] | quiet -> Right []
               | otherwise -> Left ("No such file or directory: " <> path)
            _ -> Right hits
    loadOne "-" = readClientStdin mclient >>= \case
        Left e -> pure [RErr e]
        Right bytes -> loadCommandFile st mclient load "-"
            (TE.decodeUtf8With TEE.lenientDecode bytes)
    loadOne p = do
        r <- try (readConfigUtf8 p)
        case r of
            Left (e :: IOException) -> pure [RErr (T.pack (ioeReason e) <> ": " <> T.pack p)]
            Right contents -> loadCommandFile st mclient load p contents
    ioeReason e = if isDoesNotExistError e then "No such file or directory"
        else if isPermissionError e then "Permission denied" else "Input/output error"

-- | @restart-server [-C] [path]@: reload the server binary in place. See
-- 'cmdReload'; dropped clients exit and the users reattach.
cmdRestartServer :: CommandImpl
cmdRestartServer = cmdReload ServerOnly

-- | @restart [-C] [path]@: @restart-server@ with the attached clients told to
-- re-exec too ('reloadFarewell'), so one command upgrades both halves.
cmdRestart :: CommandImpl
cmdRestart = cmdReload ServerAndClients

-- | Which halves a reload restarts: the server alone (@restart-server@), or
-- the server and every attached client (@restart@). See 'reloadFarewell'.
data ReloadScope = ServerOnly | ServerAndClients
    deriving (Eq, Show)

-- | The command name a reload reports its errors under.
reloadName :: ReloadScope -> Text
reloadName ServerOnly = "restart-server"
reloadName ServerAndClients = "restart"

-- | The farewell 'cmdReload' sends each client it is about to drop: under
-- 'ServerAndClients' an attached client re-execs in place, told which
-- session to rejoin ('RestartClientTo', handled like @restart-client@);
-- everyone else exits.
reloadFarewell :: ReloadScope -> ClientRole -> Maybe Text -> [Text] -> ServerToClient
reloadFarewell ServerAndClients Attached msess hist =
    maybe RestartClient (`RestartClientTo` hist) msess
reloadFarewell _ _ _ _ = Exited

-- | Reload the server binary in place while every pane's program keeps
-- running. @-C@ drops all scrollback across the reload (a memory cleanup).
-- Serializes the live tree and its inherited fds to a handover file, sends
-- every client its 'reloadFarewell', then re-execs @path@ (default: the
-- on-PATH @hat@, see 'resolveReloadTarget'), which re-adopts the tree
-- ('resumeServer'). The pane pty masters and the listening socket survive the
-- exec, so a farewelled client that reattaches immediately just waits in the
-- accept backlog; the accepted sockets are close-on-exec, so dropped clients
-- hang up cleanly. A missing target is reported before anything is torn down,
-- so a typo'd path is a harmless error rather than a half-dropped server.
cmdReload :: ReloadScope -> CommandImpl
cmdReload scope st mclient args = do
    -- Fail loud rather than reload on top of an in-flight startup or
    -- reload: capturing a half-rebuilt tree and re-exec'ing through it is
    -- how a second restart-server strands the live programs. Startup lands
    -- at Ready the moment the tree is whole again.
    phase <- readTVarIO st.startupPhase
    if phase /= Ready
        then pure [RErr (reloadName scope <> ": startup or reload still in progress; try again shortly")]
        else cmdReload' scope st mclient args

-- | What a reload invocation asks for: the scrollback it carries, and the
-- binary it names (none: the on-PATH @hat@, see 'resolveReloadTarget').
data ReloadRequest = ReloadRequest
    { carry  :: ScrollbackCarry
    , target :: Maybe FilePath
    }
    deriving (Eq, Show)

-- | Parse a reload's @[-C] [path]@ under the command name @name@. An unknown
-- flag, a second path, or a flag written after the path is an error, so a
-- request the reload would not act on never looks accepted.
parseReloadArgs :: Text -> [Text] -> Either Text ReloadRequest
parseReloadArgs name args = case (filter (/= "-C") flags, pos) of
    (f : _, _) -> Left (name <> ": unknown flag: " <> f)
    (_, _ : _ : _) -> Left ("usage: " <> name <> " [-C] [path]")
    _ -> Right ReloadRequest
        { carry = if "-C" `elem` flags then DropScrollback else KeepScrollback
        , target = T.unpack <$> listToMaybe pos }
  where
    (_, flags, pos) = parseArgs "" args

cmdReload' :: ReloadScope -> CommandImpl
cmdReload' scope st mclient args =
    case parseReloadArgs (reloadName scope) args of
      Left err -> pure [RErr err]
      Right req -> do
        target <- maybe resolveReloadTarget pure req.target
        exists <- doesFileExist target
        if not exists
            then pure [RErr (reloadName scope <> ": no such binary: " <> T.pack target)]
            else do
                logEvent st.logger ServerReloading { target = target }
                -- A failure anywhere past the capture reopens what the
                -- reload closed — the parked readers and the accept gate —
                -- so a failed reload keeps serving instead of freezing.
                (do dropModesForReload st
                    (cleanup, tree) <- captureReload req.carry st
                    let blobPath = st.sockPath <> ".reload"
                    B.writeFile blobPath (encodeHandover cleanup tree)
                    keepOpenAcrossExec cleanup
                    -- Park the accept loop before any farewell goes out, so a
                    -- farewelled client's immediate reattach waits in the listen
                    -- backlog for the next image instead of being accepted by
                    -- this dying one.
                    atomically $ writeTVar st.acceptGate AcceptClosing
                    atomically $ readTVar st.acceptGate >>= check . (== AcceptParked)
                    sessions <- readTVarIO st.sessions
                    attached <- fmap concat . forM (Map.elems sessions) $ \sess -> do
                        nm <- readTVarIO sess.name
                        cs <- atomically (sessionClients st sess.id)
                        forM cs $ \c -> do
                            hist <- clientSessionHistNames st c
                            pure (c, reloadFarewell scope c.role (Just nm) hist)
                    issuer <- forM (maybe [] pure mclient) $ \client -> do
                        msess <- clientSessionName st client
                        hist <- clientSessionHistNames st client
                        pure (client, reloadFarewell scope client.role msess hist)
                    unsent <- sendFarewells farewellGrace (attached <> issuer)
                    forM_ unsent $ \c ->
                        logEvent st.logger FarewellUnsent { client = rawClient c.id }
                    -- Test-only knob: hold the farewell->exec window open (µs)
                    -- so the suite can pin the reattach race deterministically.
                    linger <- lookupEnv "HAT_TEST_RELOAD_LINGER"
                    forM_ (linger >>= readMaybe) threadDelay
                    mconfig <- readTVarIO st.serverConfig
                    let argv = ["--server", st.sockPath]
                            <> maybe [] (: []) mconfig
                            <> ["--reload-handover", blobPath]
                    -- The execve replaces this image atomically, so no bracket
                    -- unwinds: the gsettings monitor child would be orphaned (14 such
                    -- orphans accrued on the dev box across upgrades). Reap it here,
                    -- while we can still signal it.
                    reapMonitor st.monitorRegistry
                    -- A popup's command is no pane: nothing carries it across
                    -- the exec, so it ends here, hung up and reaped.
                    closeAllPopups st
                    saveHistory st  -- the next image loads it after its config
                    logEvent st.logger ReloadExec { panes = length cleanup.live }
                    -- The self-exec replaces this image, so 'withLogger's
                    -- flush-on-exit never runs; drain the queue now or the
                    -- reload trace is lost.
                    flushLogger st.logger
                    void $ executeFile target False argv Nothing)
                  `onException` do
                    resumeReaders st
                    atomically (writeTVar st.acceptGate AcceptOpen)
                pure []  -- unreachable: executeFile replaces this image

-- | The binary a no-argument @restart-server@ re-execs: @hat@ as resolved on
-- @PATH@. That is a stable profile\/system symlink the kernel follows at exec
-- time, so after an upgrade @hat restart-server@ picks up the newly-installed
-- version while keeping the pane programs alive — the point of the feature. The
-- handover's era gate keeps that safe across versions: a matching era adopts
-- the tree, a changed one falls back to a clean restart ('cleanupInherited').
-- 'getExecutablePath' would not do: it resolves @\/proc\/self\/exe@ to the
-- immutable Nix store path this process launched from — the OLD build — so it
-- can never see an upgrade. It is only the last-resort fallback when @hat@ is
-- not on @PATH@. A caller that needs determinism (tests) or a specific build
-- passes the path as @restart-server@'s argument, bypassing this entirely.
resolveReloadTarget :: IO FilePath
resolveReloadTarget = do
    onPath <- findExecutable "hat"
    maybe getExecutablePath pure onPath

-- Decide what crosses the execve. The fds the reload names — the listening
-- socket and every pane's pty master — are cleared of close-on-exec, since a
-- libc that set the flag would slam them shut and hang up every program.
-- Everything else is marked close-on-exec: the successor image opens its own
-- log, store and debug socket, so an inherited one is a leak that rides every
-- future reload too. The standard streams stay, or the new image would find
-- 0, 1 and 2 free for the kernel to hand out as something else.
keepOpenAcrossExec :: ReloadCleanup -> IO ()
keepOpenAcrossExec cleanup = do
    forM_ keep (setCloseOnExec False)
    held <- openFds
    forM_ (filter (`notElem` std <> keep) held) (setCloseOnExec True)
  where
    keep = cleanup.listenFd : map fst cleanup.live
    std = [0, 1, 2]
    setCloseOnExec on fd =
        PIO.setFdOption (Fd (fromIntegral fd)) PIO.CloseOnExec on
            `catch` \(_ :: IOException) -> pure ()

-- The fds this process holds. The directory handle the listing itself opens is
-- closed by the time the list is read back, so it lands as a stale entry.
openFds :: IO [Int]
openFds = (mapMaybe readMaybe <$> listDirectory "/proc/self/fd")
    `catch` \(_ :: IOException) -> pure []

-- Release fds a reload inherited but cannot use — an incompatible or corrupt
-- payload. Closing a pane master hangs its child up (and SIGHUP makes sure),
-- so the incoming image starts fresh without orphaning the old processes or
-- leaking the old socket.
cleanupInherited :: ReloadCleanup -> IO ()
cleanupInherited cleanup = do
    forM_ cleanup.live $ \(fd, pid) -> do
        signalProcess sigHUP (fromIntegral pid)
            `catch` \(_ :: IOException) -> pure ()
        PIO.closeFd (Fd (fromIntegral fd))
            `catch` \(_ :: IOException) -> pure ()
    PIO.closeFd (Fd (fromIntegral cleanup.listenFd))
        `catch` \(_ :: IOException) -> pure ()

-- | @restart-client@: tell the currently-attached client to re-exec itself in
-- place, keeping its session. A no-op when the command is not issued from an
-- attached client — a bare @hat restart-client@ from a shell reaches the server
-- as a control connection, so there is nothing rendering to restart.
cmdRestartClient :: CommandImpl
cmdRestartClient st mclient _ =
    case restartClientAction ((.role) <$> mclient) of
        NoAttachedClient -> pure []
        RestartAttached  -> do
            forM_ mclient $ \client -> do
                msess <- clientSessionName st client
                hist <- clientSessionHistNames st client
                send client (maybe RestartClient (`RestartClientTo` hist) msess)
            pure []

-- | The name of the session a client is attached to, if it still exists.
clientSessionName :: ServerState -> Client -> IO (Maybe Text)
clientSessionName st client = do
    sid <- readTVarIO client.session
    sessMap <- readTVarIO st.sessions
    traverse (readTVarIO . (.name)) (Map.lookup sid sessMap)

-- | The still-existing sessions in a client's MRU history, by name,
-- most-recent first — carried across a restart to keep @switch-client -l@
-- working. See 'Hat.Server.Conn.resolveSessionNames' for the inverse.
clientSessionHistNames :: ServerState -> Client -> IO [Text]
clientSessionHistNames st client = atomically $ do
    hist <- readTVar client.sessionHist
    sessMap <- readTVar st.sessions
    catMaybes <$>
        mapM (\sid -> traverse (readTVar . (.name)) (Map.lookup sid sessMap)) hist

-- | The outcome of 'cmdRestartClient': restart the issuing client, or do
-- nothing because no attached client issued the command.
data RestartClientOutcome = RestartAttached | NoAttachedClient
    deriving (Eq, Show)

-- | Decide 'cmdRestartClient' from the issuing client's role: only an
-- 'Attached' client restarts; a 'Control' connection (or none) is the no-op.
restartClientAction :: Maybe ClientRole -> RestartClientOutcome
restartClientAction = \case
    Just Attached -> RestartAttached
    _             -> NoAttachedClient

-- | @display-message [-lp] [-c target-client] [-d delay] [-t target-pane]
-- [-F format | message]@ (tmux's @cmd-display-message.c@). The message
-- expands for the target pane (@CMD_FIND_PANE@ with @CANFAIL@: an
-- unresolvable target leaves just the client's context) and for the client
-- shown it when that one views the pane's session, else the session's best
-- client.
cmdDisplayMessage :: CommandImpl
cmdDisplayMessage st mclient args
    | Just e <- unknownFlag "display-message" "aCIlNpv" flags = pure [RErr e]
    | f : _ <- filter (`elem` ["-a", "-C", "-I", "-N", "-v"]) flags =
        pure [RErr ("display-message " <> f <> ": not supported")]
    | isJust (lookup "-F" opts), not (null pos) =
        pure [RErr "only one of -F or argument must be given"]
    | Just (Left e) <- mdelay = pure [RErr ("delay " <> e)]
    | otherwise = do
        etc <- formatClient st mclient Nothing (lookup "-c" opts)
        case (lookup "-c" opts, etc) of
            (Just _, Left e) -> pure [RErr e]
            _ -> do
                let mtc = either (const Nothing) Just etc
                res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
                text <- case res of
                    Left _ -> do
                        env <- clientContextFormatEnv st mtc
                        cvars <- maybe (pure []) (clientFormatVars st) mtc
                        expand (Map.union (Map.fromList cvars) env)
                    Right (sess, wix, win, pane) -> do
                        tcSid <- traverse (readTVarIO . (.session)) mtc
                        mc <- if tcSid == Just sess.id then pure mtc
                            else either (const Nothing) Just
                                <$> formatClient st Nothing (Just sess.id) Nothing
                        env <- targetFormatEnv st mclient mc (sess, wix, win, pane)
                        expand env
                case (mclient, mtc) of
                    _ | "-p" `elem` flags -> pure [ROutput text]
                    (Nothing, _) -> pure [RErr text]
                    (_, Just tc) | Just cs <- tc.control ->
                        if fmap (.id) mclient == Just tc.id
                            then pure [ROutput ("%message " <> text)]
                            else [] <$ atomically
                                (controlNotify cs (TE.encodeUtf8 ("%message " <> text)))
                    (_, Just tc) -> do
                        maybe (showStyledToast st tc text)
                            (\d -> showStyledToastFor st tc d text)
                            (either (const Nothing) Just =<< mdelay)
                        pure []
                    (_, Nothing) -> pure []
  where
    (opts, flags, pos) = parseArgs "cdtF" args
    template = case lookup "-F" opts of
        Just f -> f
        Nothing | not (null pos) -> T.unwords pos
                | otherwise -> displayMessageTemplate
    expand env
        | "-l" `elem` flags = pure template
        | otherwise = expandFormat st env template
    mdelay = strtonum 0 4294967295 <$> lookup "-d" opts

-- | tmux's @DISPLAY_MESSAGE_TEMPLATE@: what a bare @display-message@ shows.
displayMessageTemplate :: Text
displayMessageTemplate =
    "[#{session_name}] #{window_index}:#{window_name}, current pane #{pane_index} - (%H:%M %d-%b-%y)"

-- | The format environment a command's arguments expand in: its target
-- pane's (tmux's @CMD_FIND_PANE@ with @CANFAIL@), with the @mouse_*@
-- variables of the mouse event a binding is acting on, and the variables of
-- the client it runs for (tmux's target client).
commandFormatEnv :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe FormatEnv)
commandFormatEnv st mclient mtarget = do
    res <- findTarget st mclient Target.FindPane mtarget
    case res of
        Left _ -> pure Nothing
        Right found@(sess, _, _, _) -> do
            mc <- either (const Nothing) Just
                <$> formatClient st mclient (Just sess.id) Nothing
            Just <$> targetFormatEnv st mclient mc found

-- | A resolved pane target's format environment, with @mc@'s client
-- variables and the mouse variables of @mclient@'s event.
targetFormatEnv
    :: ServerState -> Maybe Client -> Maybe Client -> (Session, Int, Window, Pane)
    -> IO FormatEnv
targetFormatEnv st mclient mc (sess, wix, win, pane) = do
    mev <- clientMouseEvent mclient
    mouseVars <- case (,) <$> mclient <*> mev of
        Just (client, ev) -> mouseFormatVars st client ev
        Nothing -> pure []
    cvars <- maybe (pure []) (clientFormatVars st) mc
    pix <- paneIndexOf st win pane
    env <- paneFormatEnv st sess wix win pix pane
    pure (Map.unions [Map.fromList mouseVars, Map.fromList cvars, env])

-- | @run-shell [-bCE] [-c start-directory] [-d delay] [-t target-pane]
-- [shell-command [argument ...]]@ (tmux's @cmd-run-shell.c@). The command
-- is a format, its further arguments its @#{1}@, @#{2}@, …; without @-b@ it
-- runs to its end before the next command (see 'runShellJob' for where its
-- output goes).
cmdRunShell :: CommandImpl
cmdRunShell st mclient args
    | Just e <- unknownFlag "run-shell" "bCE" flags = pure [RErr e]
    | Just _ <- lookup "-s" opts = pure [RErr "run-shell -s: not supported"]
    | Just d <- lookup "-d" opts, Nothing <- delayOf d =
        pure [RErr ("invalid delay time: " <> d)]
    | null pos = [] <$ when waits pause
    -- -C: the argument is hat's own commands, run in turn.
    | "-C" `elem` flags = pause >> runCommandText st mclient (T.unwords pos)
    | otherwise = do
        res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        let found = either (const Nothing) Just res
            mpane = case (lookup "-t" opts, found) of
                (Just _, Just (_, _, _, p)) -> Just p
                _ -> Nothing
        env <- case found of
            Just f@(sess, _, _, _) -> do
                mc <- either (const Nothing) Just
                    <$> formatClient st mclient (Just sess.id) Nothing
                targetFormatEnv st mclient mc f
            Nothing -> clientContextFormatEnv st mclient
        cmd <- expandFormat st (Map.union numbered env) (T.concat (take 1 pos))
        dir <- maybe (jobDirectory st mclient ((\(s, _, _, _) -> s) <$> found)) (pure . T.unpack)
            (lookup "-c" opts)
        let job = runShellJob cmd dir ("-E" `elem` flags)
        if waits
            then do
                ls <- outsideCommandBatch st (pause >> job)
                case (mpane, mclient) of
                    (Just p, _) -> [] <$ unless (null ls) (viewModeAdd st p ViewParsed ls)
                    (Nothing, Just c)
                        | c.role == Control || isJust c.control -> pure (map ROutput ls)
                        | otherwise -> [] <$ unless (null ls) (viewToClient st c ViewParsed ls)
                    (Nothing, Nothing) -> pure []
            else [] <$ forkIO (do
                ls <- pause >> job
                unless (null ls) $ do
                    mp <- maybe (backgroundPane st mclient) (pure . Just) mpane
                    forM_ mp $ \p -> viewModeAdd st p ViewParsed ls)
  where
    (opts, flags, pos) = parseArgs "cdst" args
    waits = "-b" `notElem` flags
    numbered = Map.fromList (zip (map tshow [1 :: Int ..]) (drop 1 pos))
    pause = forM_ (lookup "-d" opts >>= delayOf) $ \d ->
        threadDelay (round (max 0 d * 1000000))
    delayOf t = case TR.double t of
        Right (v, rest) | T.null rest -> Just v
        _ -> Nothing

-- | Run a @run-shell@ command to its end: its output lines (its errors too
-- under @-E@), then tmux's word on a failure.
runShellJob :: Text -> FilePath -> Bool -> IO [Text]
runShellJob cmd dir withStderr = do
    (r, w) <- createPipe
    devnull <- openFile "/dev/null" ReadWriteMode
    (_, _, _, ph) <- createProcess (shell (T.unpack cmd))
        { cwd = Just dir
        , std_in = UseHandle devnull
        , std_out = UseHandle w
        , std_err = UseHandle (if withStderr then w else devnull)
        , close_fds = True }
    out <- B.hGetContents r
    code <- waitForProcess ph
    let text = TE.decodeUtf8With TEE.lenientDecode out
        -- a last line with no newline is a line too
        ls = if T.null text then [] else T.splitOn "\n" (fromMaybe text (T.stripSuffix "\n" text))
        failure = case code of
            ExitSuccess -> []
            ExitFailure n
                | n < 0 -> ["'" <> cmd <> "' terminated by signal " <> tshow (negate n)]
                | otherwise -> ["'" <> cmd <> "' returned " <> tshow n]
    pure (ls <> failure)

-- | Where a job runs (tmux's @server_client_get_cwd@): a command client's
-- own directory, else the target session's.
jobDirectory :: ServerState -> Maybe Client -> Maybe Session -> IO FilePath
jobDirectory st mclient msess = case mclient of
    Just c | c.role == Control, not (T.null c.cwd) -> pure (T.unpack c.cwd)
    _ -> case msess of
        Just sess -> readTVarIO sess.startCwd
        Nothing -> pure (fromMaybe "/" st.home)

-- | Where a @run-shell -b@ with no @-t@ shows its output: the pane its
-- client is looking at, else the current pane of the best session.
backgroundPane :: ServerState -> Maybe Client -> IO (Maybe Pane)
backgroundPane st mclient = do
    etc <- formatClient st mclient Nothing Nothing
    mp <- either (const (pure Nothing)) (clientActivePane st) etc
    case mp of
        Just p -> pure (Just p)
        Nothing -> either (const Nothing) (\(_, _, _, p) -> Just p)
            <$> findTarget st Nothing Target.FindPane Nothing

cmdIfShell :: CommandImpl
cmdIfShell st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
    case pos of
        (cond0 : thenCmd : rest) -> do
            -- tmux expands #{...} in the condition before running it, so
            -- @if-shell '[ "#{@pane-theme}" = dark ]' …@ works.
            menv <- commandFormatEnv st mclient (lookup "-t" opts)
            cond <- maybe pure (expandFormat st) menv cond0
            -- -F: the expanded condition is the answer, true unless empty
            -- or 0, with no shell run.
            ok <- if "-F" `elem` flags
                then pure (not (T.null cond) && cond /= "0")
                else do
                    (code, _, _) <- readCreateProcessWithExitCode
                        (shell (T.unpack cond)) { close_fds = True } ""
                    pure (code == ExitSuccess)
            let chosen = case (ok, rest) of
                    (True, _) -> Just thenCmd
                    (False, [elseCmd]) -> Just elseCmd
                    _ -> Nothing
            case chosen of
                Nothing -> pure []
                Just cmdText -> runCommandText st mclient cmdText
        _ -> pure [RErr "usage: if-shell condition command [command]"]


cmdKillServer :: CommandImpl
cmdKillServer st mclient _ = do
    -- See Note [Retiring the socket].
    removeFile st.sockPath `catch` \(_ :: IOException) -> pure ()
    atomically $ writeTVar st.acceptGate AcceptRetiring
    [] <$ shutDownServer st mclient

-- | End the server with its sessions still alive: keep the tree for a
-- restore, send every client away (the issuing one too), and hang up every
-- pane, so the idle exit follows once the children are reaped (see
-- 'Hat.Server.serverIdle').
shutDownServer :: ServerState -> Maybe Client -> IO ()
shutDownServer st missuer = do
    -- Flag first: pane readers race us into closePane once the ptys go,
    -- and the shutdown path must know this drain is a kill, not the last
    -- window closing (which drops the store instead).
    atomically $ do
        writeTVar st.preserveStore True
        writeTVar st.shutdown ShuttingDown
    saveNow st  -- capture the tree before we tear it down
    sessions <- readTVarIO st.sessions
    forM_ (Map.keys sessions) $ \sid -> broadcast st sid Exited
    forM_ missuer $ \client -> send client Exited
    panes <- atomically $ do
        sess <- readTVar st.sessions
        held <- fmap concat . forM (Map.elems sess) $ \s -> do
            ws <- readTVar s.windows
            fmap concat . forM (Map.elems ws) $ windowPanes
        filterM claimPane held
    forM_ panes hangupPane
    atomically $ writeTVar st.sessions Map.empty

cmdSendKeys :: CommandImpl
cmdSendKeys st mclient args
    -- -M passes the mouse event being acted on to the pane under it: to
    -- its program, or to panes-mode; any other mode holding it drops it.
    | "-M" `elem` flags = do
        mev <- clientMouseEvent mclient
        case mev of
            Nothing -> pure [RErr "no mouse target"]
            Just ev -> do
                mp <- mouseEventPane st ev
                case mp of
                    Nothing -> pure [RErr "no mouse target"]
                    Just (_, _, pane, mbox) -> atomically (topMode pane) >>= \case
                        Nothing -> [] <$ sendMouse st ev
                        Just (PanesMode ps) -> do
                            firing <- maybe (pure Nothing)
                                (fmap (.firingKey) . readIORef . (.keyState)) mclient
                            [] <$ sequence_
                                [ panesModeMouse dispatch st mclient pane ps key at
                                | Just key <- [firing], Just box <- [mbox]
                                , Just at <- [mouseAt False ev box] ]
                        Just _ -> pure []
    | otherwise = do
        -- A full pane target (sess:win.N) resolves through cmd-find;
        -- the special pane tokens (%N, !, {marked}) keep the old path.
        let mtok = lookup "-t" opts
        mpane <- case Target.parsePaneTarget mtok of
            Target.PaneCurrent | Just t <- mtok -> do
                res <- findTarget st mclient Target.FindPane (Just t)
                pure $ case res of
                    Right (_, _, _, p) -> Just p
                    Left _ -> Nothing
            _ -> targetPane st mclient mtok
        case mpane of
            Nothing -> pure []
            Just pane -> do
                -- -R: reset the pane's terminal state, as RIS does.
                when ("-R" `elem` flags) $ do
                    _ <- Emu.feed pane.emulator "\ESCc"
                    atomically (bumpDirty st)
                case repeatCount of
                    Left e -> pure [RErr e]
                    Right n -> concat <$> replicateM n (keysInto pane)
  where
    -- -N: send the keys, or run the mode command, that many times.
    repeatCount = case lookup "-N" opts of
        Nothing -> Right 1
        Just v -> either (Left . ("repeat count " <>)) Right (strtonum 1 4294967295 v)
    keysInto pane
                    | modeCmd = case pos of
                        (name : cmdArgs) -> do
                            mev <- clientMouseEvent mclient
                            forM_ mev (moveCopyCursor st)
                            case ((,) <$> mclient <*> mev, name) of
                                -- With a mouse event, begin-selection starts
                                -- a drag (tmux's window_copy_start_drag).
                                (Just (client, ev), "begin-selection") ->
                                    [] <$ startSelectionDrag st client ev
                                _ -> runCopyModeCommand st pane name cmdArgs
                        [] -> pure []
                    -- -H: each argument is one hex byte.
                    | hexBytes = case mapM hexByte pos of
                        Just bytes -> injectAll pane (map (SendBytes . B.singleton) bytes)
                        Nothing -> pure [RErr "send-keys -H: bad hex byte"]
                    | otherwise = injectAll pane (map sendInput pos)
    sendInput a = case parseKeyName a of
        Just k | not literal -> SendKey k
        _ -> SendBytes (TE.encodeUtf8 a)
    -- tmux's cmd_send_keys_inject_key over the arguments in turn: a pane in
    -- no mode takes their bytes, written together; a mode takes each key
    -- as it comes, so the keys after one that leaves the mode reach the
    -- program.
    injectAll pane = go []
      where
        go acc [] = [] <$ typeWrites (concat (reverse acc))
        go acc (i : is) = atomically (topMode pane) >>= \case
            Nothing -> do
                ws <- case i of
                    SendKey k -> keyWrites st pane k
                    SendBytes bs -> syncedWrites st pane bs
                go (ws : acc) is
            Just m -> case i of
                SendBytes bs -> go acc (map SendKey (tokenizeKeys bs) <> is)
                SendKey k -> do
                    typeWrites (concat (reverse acc))
                    rs <- injectKey pane m k
                    (rs <>) <$> go [] is
    -- A mode with a key table runs the key's binding there against the
    -- pane; any other mode takes the key itself.
    injectKey pane m k = case m of
        CopyMode cm -> do
            km <- readTVarIO st.keymap
            case Map.lookup k.name =<< Map.lookup cm.copyState.keyTable km of
                Nothing -> pure []
                Just b -> withCommandState st mclient pane $
                    concat <$> mapM (runArgv st mclient) b.commands
        _ -> [] <$ paneModeKey dispatch st mclient pane k
    (opts, flags, pos) = parseArgs "tN" args
    literal = "-l" `elem` flags
    hexBytes = "-H" `elem` flags
    modeCmd = "-X" `elem` flags
    hexByte t = case TR.hexadecimal t of
        Right (n, rest) | T.null rest, n >= 0, n <= 255 ->
            Just (fromIntegral (n :: Int))
        _ -> Nothing

-- | One send-keys argument: a named key, or text sent as its bytes.
data SendInput = SendKey Key | SendBytes B.ByteString
