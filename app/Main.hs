module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import qualified Data.Text as T
import Network.Socket (Socket)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO
import System.Posix.IO
    (OpenMode (..), closeFd, defaultFileFlags, dupTo, openFd, stdError,
     stdInput, stdOutput)
import System.Posix.Process (createSession, executeFile, forkProcess)

import Hat.Client
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
        rest -> cli { command = rest }

clientMain :: Cli -> IO ()
clientMain cli = do
    path <- maybe (defaultSocketPath cli.socketName) pure cli.socketPathOverride
    case cli.command of
        [] -> attach path cli.configFile
        ["attach"] -> attach path cli.configFile
        ["attach-session"] -> attach path cli.configFile
        ws -> control path (T.pack (unwords ws))

attach :: FilePath -> Maybe FilePath -> IO ()
attach path mconfig = do
    sock <- connectOrStart path mconfig
    reason <- runClient sock
    case reason of
        Detached -> putStrLn "[detached]"
        SessionEnded -> putStrLn "[exited]"
        ServerDied -> do
            hPutStrLn stderr "hat: server connection lost"
            exitWith (ExitFailure 1)
        Rejected e -> do
            hPutStrLn stderr ("hat: " <> T.unpack e)
            exitWith (ExitFailure 1)

control :: FilePath -> T.Text -> IO ()
control path cmdline = do
    msock <- connectTo path
    case msock of
        Nothing -> do
            hPutStrLn stderr "hat: no server running"
            exitFailure
        Just sock -> do
            reason <- runControl sock cmdline
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
            waitForServer path 50

startServer :: FilePath -> Maybe FilePath -> IO ()
startServer path mconfig = do
    self <- getExecutablePath
    void . forkProcess $ do
        _ <- createSession
        devNull <- openFd "/dev/null" ReadWrite defaultFileFlags
        _ <- dupTo devNull stdInput
        _ <- dupTo devNull stdOutput
        _ <- dupTo devNull stdError
        closeFd devNull `catch` \(_ :: SomeException) -> pure ()
        executeFile self False
            (["--server", path] <> maybe [] (: []) mconfig) Nothing

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
