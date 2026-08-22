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
    , commandTable
    , readConfigUtf8
    , cleanupInherited
    , cmdSourceFile
    , cmdRestartServer
    , cmdRestart
    , cmdRestartClient
    , cmdKillServer
    , cmdSendKeys
    , ReloadScope (..)
    , reloadFarewell
    , RestartClientOutcome (..)
    , restartClientAction
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM, forM_, unless, void)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Directory
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import qualified System.Posix.IO as PIO
import System.Posix.Process (executeFile)
import System.Posix.Signals (sigHUP, signalProcess)
import System.Posix.Types (Fd (..))
import System.Process
import Text.Read (readMaybe)
import Hat.Command.Parser (parseCommandLine, parseConfig)
import Hat.Log
import Hat.Server.Environ
import Hat.Model
import Hat.Model.Options (lookupCommandAlias)
import Hat.Path (expandTilde)
import Hat.Server.Reload (ReloadCleanup (..), encodeHandover)
import qualified Hat.Term.Pty
import Hat.Server.Command.Bind (cmdBind, cmdUnbind)
import Hat.Server.Command.CopyMode (runCopyModeCommand)
import Hat.Server.Command.Buffer
import Hat.Server.Command.Interact
import Hat.Server.Command.Layout
import Hat.Server.Command.Option
import Hat.Server.Command.Pane
import Hat.Server.Command.Session
import Hat.Server.Command.Window
import Hat.Server.Command.Types
import Hat.Server.ClientIO (broadcast, send)
import Hat.Server.ColorScheme
import Hat.Server.FormatEnv (paneFormatEnv)
import Hat.Server.Handover
import Hat.Server.Toast
import Hat.Server.Keys
import Hat.Server.Overlay (handlePickerInput)
import Hat.Server.Locate
import Hat.Server.Pane
import Hat.Server.Snapshot
import qualified Hat.Server.Target as Target
import Hat.Server.View
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
    , runCommandText = runCommandText }

runCommandText :: ServerState -> Maybe Client -> Text -> IO [Reply]
runCommandText st mclient input = case parseCommandLine input of
    Left err -> pure [RErr err]
    Right cmds -> runCommands st mclient cmds

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

-- | Dispatch one command through the builtin table.
runBuiltin :: ServerState -> Maybe Client -> Text -> [Text] -> IO [Reply]
runBuiltin st mclient name args = case Map.lookup name commandTable of
    Nothing -> pure [RErr ("unknown command: " <> name)]
    Just impl -> impl st mclient args
        `catch` \(e :: SomeException) ->
            pure [RErr (name <> ": " <> T.pack (show e))]

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

commandTable :: Map.Map Text CommandImpl
commandTable = Map.fromList $ concatMap expand
    [ (["bind-key", "bind"], cmdBind)
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
    , (["split-window", "splitw"], cmdSplitWindow)
    , (["select-pane", "selectp"], cmdSelectPane)
    , (["kill-pane", "killp"], cmdKillPane)
    , (["swap-pane", "swapp"], cmdSwapPane)
    , (["clear-history", "clearhist"], cmdClearHistory)
    , (["break-pane", "breakp"], cmdBreakPane)
    , (["join-pane", "joinp"], cmdJoinPane)
    , (["select-layout", "selectl"], cmdSelectLayout)
    , (["next-layout", "nextl"], cmdNextLayout)
    , (["previous-layout", "prevl"], cmdPreviousLayout)
    , (["resize-pane", "resizep"], cmdResizePane)
    , (["last-pane", "lastp"], cmdLastPane)
    , (["detach-client", "detach"], cmdDetachClient)
    , (["send-prefix"], cmdSendPrefix)
    , (["send-keys", "send"], cmdSendKeys)
    , (["copy-mode"], cmdCopyMode)
    , (["command-prompt"], cmdCommandPrompt)
    , (["choose-tree"], cmdChooseTree)
    , (["choose-window", "choosew"], cmdChooseWindow)
    , (["show-buffer", "showb"], cmdShowBuffer)
    , (["set-buffer", "setb"], cmdSetBuffer)
    , (["list-buffers", "lsb"], cmdListBuffers)
    , (["delete-buffer", "deleteb"], cmdDeleteBuffer)
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
    ]
  where
    expand (names, impl) = [(n, impl) | n <- names]

-- Command implementations.

-- | Whether @set@\/@set-option@ (session) or @setw@\/@set-window-option@
-- (window) invoked the set: the default scope when no @-g@\/@-s@\/@-w@ picks
-- one. See 'cmdSet'.
cmdSourceFile :: CommandImpl
cmdSourceFile st mclient args = case pos of
    [path] -> do
        p <- expandTilde (T.unpack path)
        exists <- doesFileExist p
        if not exists
            then if "-q" `elem` flags
                then pure []
                else pure [RErr ("no such file: " <> path)]
            else do
                contents <- readConfigUtf8 p
                case parseConfig contents of
                    Left err -> pure [RErr err]
                    Right cmds
                        -- -n: check syntax, do not execute
                        | "-n" `elem` flags -> pure []
                        | otherwise ->
                            concat <$> mapM (runArgv st mclient) cmds
    _ -> pure [RErr "usage: source-file path"]
  where
    (_, flags, pos) = parseArgs "" args

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
-- 'ServerAndClients' an attached client re-execs in place ('RestartClient',
-- handled like @restart-client@); everyone else exits.
reloadFarewell :: ReloadScope -> ClientRole -> ServerToClient
reloadFarewell ServerAndClients Attached = RestartClient
reloadFarewell _ _ = Exited

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

cmdReload' :: ReloadScope -> CommandImpl
cmdReload' scope st mclient args = do
    let (_, flags, pos) = parseArgs "" args
        carry = if "-C" `elem` flags then DropScrollback else KeepScrollback
    case filter (/= "-C") flags of
      (f : _) -> pure [RErr (reloadName scope <> ": unknown flag: " <> f)]
      [] -> do
        target <- case pos of
            (p : _) -> pure (T.unpack p)    -- explicit binary path (deterministic)
            []      -> resolveReloadTarget  -- default: the on-PATH hat
        exists <- doesFileExist target
        if not exists
            then pure [RErr (reloadName scope <> ": no such binary: " <> T.pack target)]
            else do
                logEvent st.logger ServerReloading { target = target }
                (cleanup, tree) <- captureReload carry st
                let blobPath = st.sockPath <> ".reload"
                B.writeFile blobPath (encodeHandover cleanup tree)
                keepOpenAcrossExec cleanup
                sessions <- readTVarIO st.sessions
                forM_ (Map.keys sessions) $ \sid -> do
                    cs <- atomically (sessionClients st sid)
                    forM_ cs $ \c -> send c (reloadFarewell scope c.role)
                forM_ mclient $ \client ->
                    send client (reloadFarewell scope client.role)
                mconfig <- readTVarIO st.serverConfig
                let argv = ["--server", st.sockPath]
                        <> maybe [] (: []) mconfig
                        <> ["--reload-handover", blobPath]
                -- The execve replaces this image atomically, so no bracket
                -- unwinds: the gsettings monitor child would be orphaned (14 such
                -- orphans accrued on the dev box across upgrades). Reap it here,
                -- while we can still signal it.
                reapMonitor st.monitorRegistry
                -- The self-exec replaces this image, so 'withLogger's
                -- flush-on-exit never runs; drain the queue now or the
                -- reload trace is lost.
                flushLogger st.logger
                _ <- executeFile target False argv Nothing
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
cmdRestartClient _ mclient _ =
    case restartClientAction ((.role) <$> mclient) of
        NoAttachedClient -> pure []
        RestartAttached  -> do
            forM_ mclient $ \client -> send client RestartClient
            pure []

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

-- | @display-message [-p] [-t target] message@. The target is a pane
-- (tmux's @CMD_FIND_PANE@ with @CANFAIL@): the message expands in the
-- resolved pane's format environment, and an unresolvable target
-- degrades to the plain text instead of erroring.
cmdDisplayMessage :: CommandImpl
cmdDisplayMessage st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        raw = T.unwords pos
    res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
    text <- case res of
        Left _ -> pure raw
        Right (sess, wix, win, pane) -> do
            pix <- paneIndexOf st win pane
            env <- paneFormatEnv st sess wix win pix pane
            expandFormat st env raw
    if "-p" `elem` flags
        then pure [ROutput text]
        else do
            forM_ mclient $ \client -> showToast st client text
            pure []

cmdRunShell :: CommandImpl
cmdRunShell st mclient args = do
    let (_, _, pos) = parseArgs "t" args
        cmdText = T.unwords pos
    void . forkIO $ do
        (code, out, errOut) <- readCreateProcessWithExitCode
            (shell (T.unpack cmdText)) { close_fds = True } ""
        let firstLine = T.strip . T.takeWhile (/= '\n') . T.pack
        case code of
            ExitSuccess ->
                forM_ mclient $ \client ->
                    unless (null out) $ showToast st client (firstLine out)
            ExitFailure n ->
                forM_ mclient $ \client ->
                    showToast st client $
                        "run-shell exited " <> tshow n <> ": "
                        <> firstLine (out <> errOut)
    pure []

cmdIfShell :: CommandImpl
cmdIfShell st mclient args = do
    let (_, _, pos) = parseArgs "t" args
    case pos of
        (cond0 : thenCmd : rest) -> do
            -- tmux expands #{...} in the condition before running it, so
            -- @if-shell '[ "#{@pane-theme}" = dark ]' …@ works.
            msess <- targetSession st mclient Nothing
            cond <- case msess of
                Just sess -> do
                    env <- sessionFormatEnv st sess
                    expandFormat st env cond0
                Nothing -> pure cond0
            (code, _, _) <- readCreateProcessWithExitCode
                (shell (T.unpack cond)) { close_fds = True } ""
            let chosen = case (code, rest) of
                    (ExitSuccess, _) -> Just thenCmd
                    (ExitFailure _, [elseCmd]) -> Just elseCmd
                    _ -> Nothing
            case chosen of
                Nothing -> pure []
                Just cmdText -> runCommandText st mclient cmdText
        _ -> pure [RErr "usage: if-shell condition command [command]"]


cmdKillServer :: CommandImpl
cmdKillServer st mclient _ = do
    -- Flag first: pane readers race us into closePane once the ptys go,
    -- and the shutdown path must know this drain is a kill, not the last
    -- window closing (which drops the store instead).
    atomically $ writeTVar st.preserveStore True
    saveNow st  -- capture the tree before we tear it down
    sessions <- readTVarIO st.sessions
    forM_ (Map.keys sessions) $ \sid -> broadcast st sid Exited
    forM_ mclient $ \client -> send client Exited
    panes <- atomically $ do
        sess <- readTVar st.sessions
        fmap concat . forM (Map.elems sess) $ \s -> do
            ws <- readTVar s.windows
            fmap concat . forM (Map.elems ws) $ windowPanes
    forM_ panes hangupPane
    atomically $ do
        writeTVar st.sessions Map.empty
        writeTVar st.everAttached True
    pure []

cmdSendKeys :: CommandImpl
cmdSendKeys st mclient args = do
    let (opts, flags, pos) = parseArgs "tN" args
        literal = "-l" `elem` flags
        modeCmd = "-X" `elem` flags
    mpicker <- maybe (pure Nothing) (readTVarIO . (.picker)) mclient
    case (mpicker, mclient) of
        -- An open chooser owns send-keys: they drive its navigation/search
        -- (this is how the config's @… \; send-keys /@ enters search).
        (Just pk, Just client) | not modeCmd -> do
            handlePickerInput dispatch st client pk
                (concatMap (tokenizeKeys . argBytes literal) pos)
            pure []
        _ -> do
            mpane <- targetPane st mclient (lookup "-t" opts)
            case mpane of
                Nothing -> pure []
                Just pane
                    | modeCmd -> case pos of
                        (name : cmdArgs) -> runCopyModeCommand st pane name cmdArgs
                        [] -> pure []
                    | otherwise -> [] <$ Hat.Term.Pty.writePty pane.pty
                        (B.concat (map (argBytes literal) pos))
  where
    argBytes True a = TE.encodeUtf8 a
    argBytes False a = case parseKeyName a of
        Just k -> k.raw
        Nothing -> TE.encodeUtf8 a
