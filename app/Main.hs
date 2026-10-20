{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Network.Socket (Socket, close)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO
import System.Posix.Process (executeFile)
import System.Process
    ( CreateProcess (..), ProcessHandle, StdStream (..), createProcess
    , getProcessExitCode, proc )

import Hat.Client
import Hat.Command.Parser (parseArgv, parseScript)
import Hat.Command.Script (Script (..), ScriptItem (..))
import Hat.Debug (prepareDebugSocket)
import Hat.Path (hatPath, render, (</:>))
import Hat.Server (resumeServer, runServer)
import Hat.Transport.Socket (ConnectFailure (..), connectTo, defaultSocketPath)
import Hat.Version (versionText)

#ifdef GHC_DEBUG
import GHC.Debug.Stub qualified
#endif

data Cli = Cli
    { socketName :: String
    , socketPathOverride :: Maybe FilePath
    , configFile :: Maybe FilePath
    , controlMode :: Maybe ControlFlavour  -- ^ @-C@ once, or @-CC@
    , command :: [String]
    , action :: CliAction
    }

-- | What an invocation asks of hat.
data CliAction
    = RunCommand    -- ^ run 'command' against the server
    | PrintVersion  -- ^ @-V@: print the version and exit, as tmux does

-- Default config: ~/.config/hat/hat.conf, if it exists.
resolveConfig :: Cli -> IO Cli
resolveConfig cli = case cli.configFile of
    Just _ -> pure cli
    Nothing -> do
        mhome <- lookupEnv "HOME"
        case mhome of
            Nothing -> pure cli
            Just home -> do
                let p = render (hatPath home </:> ".config" </:> "hat" </:> "hat.conf")
                exists <- doesFileExist p
                pure cli { configFile = if exists then Just p else Nothing }

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--server" : path : rest) ->
            let (mhandover, cfgArgs) = takeReloadHandover rest
                mconfig = case cfgArgs of
                    (cfg : _) -> Just cfg
                    _ -> Nothing
            in case mhandover of
                Just h -> withServerDebug path (resumeServer path mconfig h)
                Nothing -> withServerDebug path (runServer path mconfig)
        _ -> let cli = parseCli args in case cli.action of
            PrintVersion -> TIO.putStrLn ("hat " <> versionText)
            RunCommand -> clientMain =<< resolveConfig cli

-- | Run the server as a ghc-debug debuggee on the socket 'prepareDebugSocket'
-- pins, so ghc-debug-brick can attach to the live process. Only the long-lived
-- server is a debuggee — one-shot client invocations are left alone, and so is
-- a server whose socket path would not fit an address. Without @-fghc-debug@
-- the stub is not linked and nothing listens, but the inherited sockets are
-- still released.
withServerDebug :: FilePath -> IO () -> IO ()
withServerDebug sock act = do
    mpath <- prepareDebugSocket sock
    maybe act (const (debuggee act)) mpath
  where
#ifdef GHC_DEBUG
    debuggee = GHC.Debug.Stub.withGhcDebug
#else
    debuggee = id
#endif

-- | Pull @--reload-handover PATH@ (the in-place reload marker
-- 'Hat.Server.cmdRestartServer' passes to its re-exec) out of the server
-- args, returning it and the remaining args (a leading config path, if any).
takeReloadHandover :: [String] -> (Maybe FilePath, [String])
takeReloadHandover = go (Nothing, [])
  where
    go (mh, keep) = \case
        ("--reload-handover" : h : rest) -> go (Just h, keep) rest
        (a : rest) -> go (mh, keep <> [a]) rest
        [] -> (mh, keep)

parseCli :: [String] -> Cli
parseCli = go Cli
    { socketName = "default"
    , socketPathOverride = Nothing
    , configFile = Nothing
    , controlMode = Nothing
    , command = []
    , action = RunCommand
    }
  where
    go cli = \case
        [] -> cli
        ("-C" : rest) -> go cli { controlMode = Just (moreControl cli) } rest
        ("-CC" : rest) -> go cli { controlMode = Just ControlDcs } rest
        ("-L" : name : rest) -> go cli { socketName = name } rest
        ("-S" : path : rest) -> go cli { socketPathOverride = Just path } rest
        ("-f" : path : rest) -> go cli { configFile = Just path } rest
        -- -u asks for UTF-8, which hat's client always speaks.
        ("-u" : rest) -> go cli rest
        ("-V" : _) -> cli { action = PrintVersion }
        (arg : rest)
            | Just v <- attached "-L" arg -> go cli { socketName = v } rest
            | Just v <- attached "-S" arg ->
                go cli { socketPathOverride = Just v } rest
            | Just v <- attached "-f" arg -> go cli { configFile = Just v } rest
        rest -> cli { command = rest }
    moreControl cli = maybe ControlLines (const ControlDcs) cli.controlMode
    attached flag arg = case splitAt 2 arg of
        (pre, v) | pre == flag && not (null v) -> Just v
        _ -> Nothing

clientMain :: Cli -> IO ()
clientMain cli = do
    path <- maybe (defaultSocketPath cli.socketName) pure cli.socketPathOverride
    let (hist, cmdArgs) = takeReattachHist cli.command
        cmds = parseArgv cmdArgs
    case cli.controlMode of
        Just flavour -> controlMode path cli.configFile flavour
            (if null cmds then [["new-session"]] else cmds)
        Nothing
            | null cmdArgs -> defaultClientCommand path cli.configFile
            | otherwise -> runCommandLine path cli.configFile (pure ()) hist cmds

-- | Run a command line from the shell: attach for one that grabs the
-- terminal, else run it as a command client. @answered@ runs once the
-- server first answers.
runCommandLine :: FilePath -> Maybe FilePath -> IO () -> [T.Text] -> [[T.Text]] -> IO ()
runCommandLine path mconfig answered hist cmds = case cmds of
    [[verb]] | verb `elem` ["attach", "attach-session"] ->
        attach path mconfig answered [] hist
    _ -> case terminalMode cmds of
        GrabsTerminal -> attach path mconfig answered cmds hist
        FireAndForget -> control path mconfig answered cmds

-- | A bare @hat@: run the server's @default-client-command@, as tmux's
-- server does for a client that sent no command. The client asks for it
-- itself, since its hello cannot say the command was left out, and asks as
-- a command client the server holds until its config has loaded, so even a
-- server this starts answers with its config's value. That connection
-- stays open until the server answers the next, so a server it started
-- cannot exit idle in between. A server too old to know the option is
-- attached to as a bare @hat@ always did.
defaultClientCommand :: FilePath -> Maybe FilePath -> IO ()
defaultClientCommand path mconfig = do
    (sock, origin) <- connectOrStart path mconfig
    r <- captureControl sock origin
        [["show-options", "-gv", "default-client-command"]]
    let release = close sock
    case r of
        Right out -> case parseScript (TE.decodeUtf8Lenient out) of
            Script items Nothing
                | Just cmds <- traverse (commandOf . snd) items ->
                    runCommandLine path mconfig release [] cmds
                | otherwise -> failWith "conditionals and assignments are not supported here"
            Script _ (Just (_, e)) -> failWith e
        Left (Rejected _) -> attach path mconfig release [] []
        Left _ -> do
            hPutStrLn stderr "hat: server exited unexpectedly"
            exitFailure
  where
    commandOf = \case
        ScriptCommand argv -> Just argv
        _ -> Nothing
    failWith e = do
        hPutStrLn stderr ("hat: default-client-command: " <> T.unpack e)
        exitFailure

-- | Pull the internal @--reattach-hist NAME@ flags (repeated, one per MRU
-- session) out of a restarting client's argv, returning the names
-- most-recent-first and the argv without them. See 'restartClient'.
takeReattachHist :: [String] -> ([T.Text], [String])
takeReattachHist = go ([], [])
  where
    go (hist, keep) = \case
        ("--reattach-hist" : n : rest) -> go (hist <> [T.pack n], keep) rest
        (a : rest) -> go (hist, keep <> [a]) rest
        [] -> (hist, keep)

-- | @-C@: tmux control mode, started by the given command (a bare @-C@
-- starts a new session, as plain @hat@ does). See 'Hat.Client.runControlMode'.
controlMode :: FilePath -> Maybe FilePath -> ControlFlavour -> [[T.Text]] -> IO ()
controlMode path mconfig flavour setup = do
    (sock, origin) <- connectOrStart path mconfig
    r <- runControlMode sock origin flavour setup
    case r of
        Right 0 -> pure ()
        Right n -> exitWith (ExitFailure n)
        Left (Rejected e) -> do
            hPutStrLn stderr ("hat: " <> T.unpack e)
            exitWith (ExitFailure 1)
        Left ServerDied -> do
            hPutStrLn stderr "hat: server exited unexpectedly"
            exitWith (ExitFailure 1)
        Left _ -> exitWith (ExitFailure 1)

attach :: FilePath -> Maybe FilePath -> IO () -> [[T.Text]] -> [T.Text] -> IO ()
attach path mconfig answered setup hist = do
    -- Attaching from inside a pane of the *same* server ($TMUX names its
    -- socket) would render the session inside itself. Refuse, like tmux.
    -- Attaching to a different server, and control commands (list-sessions,
    -- send-keys, …), stay available to scripts running in panes.
    nested <- lookupEnv "TMUX"
    case nested of
        Just v | nestsOwnServer v path -> do
            hPutStrLn stderr
                "hat: sessions should be nested with care, unset TMUX to force"
            exitFailure
        _ -> pure ()
    (sock, origin) <- connectOrStart path mconfig
    reason <- runClient sock origin answered setup hist
    case reason of
        Detached -> putStrLn "[detached]"
        SessionEnded -> putStrLn "[exited]"
        -- Only a command client's commands finish.
        CommandsFinished _ -> putStrLn "[exited]"
        RestartRequested target hist' -> restartClient path mconfig target hist'
        ServerDied -> do
            hPutStrLn stderr "hat: server connection lost"
            exitWith (ExitFailure 1)
        Rejected e -> do
            hPutStrLn stderr ("hat: " <> T.unpack e)
            exitWith (ExitFailure 1)

-- | Re-exec @hat@ in place to reattach: the server told us to restart, so
-- replace this client image with a fresh @hat attach@ against the same socket
-- and config. Resolving @hat@ on @PATH@ (not @\/proc\/self\/exe@) is what lets
-- the restart pick up a just-installed build — the point of restart-client —
-- mirroring 'Hat.Server.resolveReloadTarget'. The attach targets the session
-- the farewell named, so each client rejoins its own; a nameless farewell (an
-- older server) falls back to a bare attach. The farewell's MRU session
-- history rides back as @--reattach-hist@ flags so @switch-client -l@ survives.
restartClient :: FilePath -> Maybe FilePath -> Maybe T.Text -> [T.Text] -> IO ()
restartClient path mconfig msess hist = do
    onPath <- findExecutable "hat"
    target <- maybe getExecutablePath pure onPath
    let argv = ["-S", path] <> maybe [] (\c -> ["-f", c]) mconfig
            <> ["attach"] <> maybe [] (\s -> ["-t", T.unpack s]) msess
            <> concatMap (\n -> ["--reattach-hist", T.unpack n]) hist
    executeFile target False argv Nothing

control :: FilePath -> Maybe FilePath -> IO () -> [[T.Text]] -> IO ()
control path mconfig answered cmds = do
    -- A list holding any session-creating command starts the server, like
    -- tmux (so @set -g … \; new -d@ works); anything else against a dead
    -- server is an error.
    let starters = ["new-session", "new", "start-server", "start",
                    "attach-session", "attach"] :: [T.Text]
        starts = \case
            (w : _) -> w `elem` starters
            [] -> False
    msock <- if any starts cmds
        then Right <$> connectOrStart path mconfig
        else fmap (, Joined) <$> connectTo path
    case msock of
        Left NoServer -> do
            hPutStrLn stderr "hat: no server running"
            exitFailure
        Left (ConnectFailed why) -> unreachable path why
        Right (sock, origin) -> do
            reason <- runControl sock origin answered cmds
            case reason of
                -- Bare, like tmux: scripts match on the exact error text
                -- (e.g. @duplicate session: NAME@), so no @hat: @ prefix.
                Rejected e -> do
                    hPutStrLn stderr (T.unpack e)
                    exitFailure
                ServerDied -> do
                    hPutStrLn stderr "hat: server exited unexpectedly"
                    exitFailure
                CommandsFinished n | n /= 0 -> exitWith (ExitFailure n)
                _ -> pure ()

-- | Connect, spawning the server first if none is running. The origin says
-- which happened: an 'Autostarted' invocation raced its own server's config
-- load for the first session, so the server holds its commands until startup
-- lands (see 'Hat.Server.startupGate').
connectOrStart :: FilePath -> Maybe FilePath -> IO (Socket, Autostart)
connectOrStart path mconfig = do
    msock <- connectTo path
    case msock of
        Right sock -> pure (sock, Joined)
        Left (ConnectFailed why) -> unreachable path why
        Left NoServer -> do
            server <- startServer path mconfig
            -- 10s of headroom: the fresh server forks, sets up its socket
            -- dir/lock, and loads config before it starts listening, which
            -- can take a while under heavy load.
            (, Autostarted) <$> waitForServer path mconfig server 1_000

-- | tmux's error for a socket path that cannot be connected to at all.
unreachable :: FilePath -> String -> IO a
unreachable path why = do
    hPutStrLn stderr ("error connecting to " <> path <> " (" <> why <> ")")
    exitFailure

-- Spawn the detached server via 'createProcess' rather than a manual
-- 'forkProcess'/'executeFile': createProcess does the fork+exec entirely
-- in C, so no Haskell runs in the child between them. The old approach
-- juggled fds in the forked child, which under parallel load could stall
-- before exec (the threaded-RTS fork hazard) — the server then never came
-- up and the client gave up with "server failed to start". @new_session@
-- setsid()s the daemon off the controlling tty.
startServer :: FilePath -> Maybe FilePath -> IO ProcessHandle
startServer path mconfig = do
    self <- getExecutablePath
    devIn <- openFile "/dev/null" ReadMode
    devOut <- openFile "/dev/null" WriteMode
    devErr <- openFile "/dev/null" WriteMode
    (_, _, _, ph) <- createProcess
        (proc self (["--server", path] <> maybe [] (: []) mconfig))
            { std_in = UseHandle devIn
            , std_out = UseHandle devOut
            , std_err = UseHandle devErr
            , new_session = True
            , close_fds = True
            }
    pure ph

-- | Poll until a server listens at @path@, keeping one of ours spawned until
-- one does.
waitForServer :: FilePath -> Maybe FilePath -> ProcessHandle -> Int -> IO Socket
waitForServer path mconfig server attempts
    | attempts <= 0 = do
        hPutStrLn stderr "hat: server failed to start"
        exitFailure
    | otherwise = do
        msock <- connectTo path
        case msock of
            Right sock -> pure sock
            Left (ConnectFailed why) -> unreachable path why
            Left NoServer -> do
                server' <- getProcessExitCode server >>= \case
                    Just _ -> startServer path mconfig
                    Nothing -> pure server
                threadDelay 10_000
                waitForServer path mconfig server' (attempts - 1)
