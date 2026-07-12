module Main (main) where

import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import Network.Socket (Socket)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO
import System.Process
    (CreateProcess (..), StdStream (..), createProcess, proc)

import Hat.Client
import Hat.Command.Parser (parseArgv)
import Hat.Server (runServer)
import Hat.Socket (connectTo, defaultSocketPath)

data Cli = Cli
    { socketName :: String
    , socketPathOverride :: Maybe FilePath
    , configFile :: Maybe FilePath
    , command :: [String]
    }

-- Default config: ~/.config/hat/hat.conf, if it exists.
resolveConfig :: Cli -> IO Cli
resolveConfig cli = case cli.configFile of
    Just _ -> pure cli
    Nothing -> do
        mhome <- lookupEnv "HOME"
        case mhome of
            Nothing -> pure cli
            Just home -> do
                let p = home <> "/.config/hat/hat.conf"
                exists <- doesFileExist p
                pure cli { configFile = if exists then Just p else Nothing }

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--server" : path : rest) ->
            runServer path (case rest of
                [cfg] -> Just cfg
                _ -> Nothing)
        _ -> clientMain =<< resolveConfig (parseCli args)

parseCli :: [String] -> Cli
parseCli = go Cli
    { socketName = "default"
    , socketPathOverride = Nothing
    , configFile = Nothing
    , command = []
    }
  where
    go cli = \case
        [] -> cli
        ("-L" : name : rest) -> go cli { socketName = name } rest
        ("-S" : path : rest) -> go cli { socketPathOverride = Just path } rest
        ("-f" : path : rest) -> go cli { configFile = Just path } rest
        (arg : rest)
            | Just v <- attached "-L" arg -> go cli { socketName = v } rest
            | Just v <- attached "-S" arg ->
                go cli { socketPathOverride = Just v } rest
            | Just v <- attached "-f" arg -> go cli { configFile = Just v } rest
        rest -> cli { command = rest }
    attached flag arg = case splitAt 2 arg of
        (pre, v) | pre == flag && not (null v) -> Just v
        _ -> Nothing

clientMain :: Cli -> IO ()
clientMain cli = do
    path <- maybe (defaultSocketPath cli.socketName) pure cli.socketPathOverride
    let cmds = parseArgv cli.command
    case cli.command of
        [] -> attach path cli.configFile []
        ["attach"] -> attach path cli.configFile []
        ["attach-session"] -> attach path cli.configFile []
        _ | attachesTerminal cmds -> attach path cli.configFile cmds
          | otherwise -> control path cli.configFile cmds

-- | Verbs that should grab this terminal and render, running server-side
-- first to establish the session: @new-session@/@new@ (unless detached
-- with @-d@) and @attach-session@/@attach@ with an explicit target. A
-- bare @new@ still attaches; a @new -d@ stays a fire-and-exit command.
attachesTerminal :: [[T.Text]] -> Bool
attachesTerminal [cmd@(verb : _)]
    | verb `elem` ["new-session", "new"] = "-d" `notElem` cmd
    | verb `elem` ["attach-session", "attach"] = True
attachesTerminal _ = False

attach :: FilePath -> Maybe FilePath -> [[T.Text]] -> IO ()
attach path mconfig setup = do
    sock <- connectOrStart path mconfig
    reason <- runClient sock setup
    case reason of
        Detached -> putStrLn "[detached]"
        SessionEnded -> putStrLn "[exited]"
        ServerDied -> do
            hPutStrLn stderr "hat: server connection lost"
            exitWith (ExitFailure 1)
        Rejected e -> do
            hPutStrLn stderr ("hat: " <> T.unpack e)
            exitWith (ExitFailure 1)

control :: FilePath -> Maybe FilePath -> [[T.Text]] -> IO ()
control path mconfig cmds = do
    -- Session-creating commands start the server, like tmux; anything
    -- else against a dead server is an error.
    let starters = ["new-session", "new", "start-server", "start",
                    "attach-session", "attach"] :: [T.Text]
        firstWord = case cmds of
            ((w : _) : _) -> w
            _ -> ""
    msock <- if firstWord `elem` starters
        then Just <$> connectOrStart path mconfig
        else connectTo path
    case msock of
        Nothing -> do
            hPutStrLn stderr "hat: no server running"
            exitFailure
        Just sock -> do
            reason <- runControl sock cmds
            case reason of
                Rejected e -> do
                    hPutStrLn stderr ("hat: " <> T.unpack e)
                    exitFailure
                _ -> pure ()

connectOrStart :: FilePath -> Maybe FilePath -> IO Socket
connectOrStart path mconfig = do
    msock <- connectTo path
    case msock of
        Just sock -> pure sock
        Nothing -> do
            startServer path mconfig
            -- 10s of headroom: the fresh server forks, sets up its socket
            -- dir/lock, and loads config before it starts listening, which
            -- can take a while under heavy load.
            waitForServer path 100

-- Spawn the detached server via 'createProcess' rather than a manual
-- 'forkProcess'/'executeFile': createProcess does the fork+exec entirely
-- in C, so no Haskell runs in the child between them. The old approach
-- juggled fds in the forked child, which under parallel load could stall
-- before exec (the threaded-RTS fork hazard) — the server then never came
-- up and the client gave up with "server failed to start". @new_session@
-- setsid()s the daemon off the controlling tty.
startServer :: FilePath -> Maybe FilePath -> IO ()
startServer path mconfig = do
    self <- getExecutablePath
    devIn <- openFile "/dev/null" ReadMode
    devOut <- openFile "/dev/null" WriteMode
    devErr <- openFile "/dev/null" WriteMode
    _ <- createProcess
        (proc self (["--server", path] <> maybe [] (: []) mconfig))
            { std_in = UseHandle devIn
            , std_out = UseHandle devOut
            , std_err = UseHandle devErr
            , new_session = True
            , close_fds = True
            }
    pure ()

waitForServer :: FilePath -> Int -> IO Socket
waitForServer path attempts
    | attempts <= 0 = do
        hPutStrLn stderr "hat: server failed to start"
        exitFailure
    | otherwise = do
        msock <- connectTo path
        case msock of
            Just sock -> pure sock
            Nothing -> do
                threadDelay 100_000
                waitForServer path (attempts - 1)
