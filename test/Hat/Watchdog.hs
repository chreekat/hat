-- | A deadline on the whole test run that turns a wedged run into a
-- diagnosis: which items were running, and what every live thread was
-- blocked on.
module Hat.Watchdog (withWatchdog, labelItems) where

import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay, throwTo)
import Control.Exception (Exception, bracket)
import Control.Monad (forM_, void)
import GHC.Conc.Sync (ThreadStatus (..), labelThread, listThreads, threadLabel, threadStatus)
import System.Exit (ExitCode (..))
import System.IO (hFlush, hPutStrLn, stderr)
import System.Posix.Process (exitImmediately)
import System.Timeout (timeout)
import Test.Hspec (SpecWith)
import Test.Hspec.Core.Spec (Item (..), Location (..), mapSpecItem_)

data Stuck = Stuck

instance Show Stuck where
    show Stuck = "watchdog: the run outlived its deadline"

instance Exception Stuck

-- | Run @act@ (the main thread's whole run). Past @secs@, dump every live
-- thread's label and status to stderr, then interrupt the main thread so
-- brackets still release their resources, and exit outright if that
-- doesn't finish the run within 'graceSecs'.
withWatchdog :: Int -> IO a -> IO a
withWatchdog secs act = do
    mainT <- myThreadId
    bracket (forkIO (bark mainT)) killThread (const act)
  where
    bark mainT = do
        threadDelay (secs * 1_000_000)
        -- Neither a wedged stderr nor a main thread stuck in a foreign call
        -- may keep the run alive.
        void (timeout 5_000_000 dumpThreads)
        void (forkIO (throwTo mainT Stuck))
        threadDelay (graceSecs * 1_000_000)
        exitImmediately (ExitFailure 124)
    dumpThreads = do
        hPutStrLn stderr ("watchdog: run still going after " <> show secs <> "s; live threads:")
        ts <- listThreads
        forM_ ts $ \t -> do
            s <- threadStatus t
            case s of
                ThreadFinished -> pure ()
                ThreadDied -> pure ()
                _ -> do
                    l <- threadLabel t
                    hPutStrLn stderr ("  " <> show t <> " " <> maybe "-" show l <> " " <> show s)
        hFlush stderr

graceSecs :: Int
graceSecs = 10

-- | Label the thread running each item with the item's location and
-- name, for 'withWatchdog''s dump.
labelItems :: SpecWith a -> SpecWith a
labelItems = mapSpecItem_ $ \item -> item
    { itemExample = \params act progress -> do
        t <- myThreadId
        labelThread t (where_ item.itemLocation <> item.itemRequirement)
        item.itemExample params act progress
    }
  where
    where_ = maybe "" (\l -> l.locationFile <> ":" <> show l.locationLine <> " ")
