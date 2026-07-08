module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import qualified Data.Text as T
import Network.Socket (Socket)
import System.Environment (getArgs, getExecutablePath)
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
    , command :: [String]
    }

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--server", path] -> runServer path
        _ -> clientMain (parseCli args)

parseCli :: [String] -> Cli
parseCli = go Cli { socketName = "default", socketPathOverride = Nothing, command = [] }
  where
    go cli = \case
        [] -> cli
        ("-L" : name : rest) -> go cli { socketName = name } rest
        ("-S" : path : rest) -> go cli { socketPathOverride = Just path } rest
        rest -> cli { command = rest }

clientMain :: Cli -> IO ()
clientMain cli = do
    path <- maybe (defaultSocketPath cli.socketName) pure cli.socketPathOverride
    case cli.command of
        [] -> attach path
        ["attach"] -> attach path
        ["attach-session"] -> attach path
        ws -> control path (T.pack (unwords ws))

attach :: FilePath -> IO ()
attach path = do
    sock <- connectOrStart path
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

connectOrStart :: FilePath -> IO Socket
connectOrStart path = do
    msock <- connectTo path
    case msock of
        Just sock -> pure sock
        Nothing -> do
            startServer path
            waitForServer path 50

startServer :: FilePath -> IO ()
startServer path = do
    self <- getExecutablePath
    void . forkProcess $ do
        _ <- createSession
        devNull <- openFd "/dev/null" ReadWrite defaultFileFlags
        _ <- dupTo devNull stdInput
        _ <- dupTo devNull stdOutput
        _ <- dupTo devNull stdError
        closeFd devNull `catch` \(_ :: SomeException) -> pure ()
        executeFile self False ["--server", path] Nothing

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
