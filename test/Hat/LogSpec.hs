{-# LANGUAGE ScopedTypeVariables #-}

module Hat.LogSpec (spec) where

import Control.Exception (IOException, bracket, catch)
import Data.List (isInfixOf)
import System.Directory (removePathForcibly)
import System.IO (readFile')
import System.Posix.Temp (mkdtemp)
import Test.Hspec

import Hat.Log

-- An item-private log path that dies with the item, so parallel items
-- never share a file.
withLogPath :: (FilePath -> IO a) -> IO a
withLogPath act =
    bracket (mkdtemp "/tmp/hat-logspec-") removePathForcibly $ \dir ->
        act (dir <> "/hat.log")

spec :: Spec
spec = do
    -- The bracketed lifetime flushes queued events and tears the drain thread
    -- + handle down on scope exit, structurally — no hand-called close. Reading
    -- the file back proves the queued line reached disk before the scope ended.
    it "withLogger flushes queued events on scope exit" $
        withLogPath $ \path -> do
            withLogger path $ \lg ->
                logEvent lg ServerCrash { err = "with-marker-42" }
            contents <- readFile' path
            contents `shouldSatisfy` isInfixOf "with-marker-42"

    -- The teardown must run even when the body throws, so a crash mid-scope
    -- still leaves its queued events on disk rather than losing them to an
    -- abandoned drain thread.
    it "withLogger flushes even when the body throws" $
        withLogPath $ \path -> do
            (withLogger path $ \lg -> do
                logEvent lg ServerCrash { err = "throw-marker-7" }
                ioError (userError "boom"))
                `catch` \(_ :: IOException) -> pure ()
            contents <- readFile' path
            contents `shouldSatisfy` isInfixOf "throw-marker-7"

    -- The explicit flush is what lets the fatal trace and the restart-server
    -- self-exec drain the queue before the process is replaced. A flush that
    -- returned early would leave the marker unwritten when the scope exits.
    it "flushLogger drains the queue before it returns" $
        withLogPath $ \path -> do
            withLogger path $ \lg -> do
                logEvent lg ServerCrash { err = "flush-marker-99" }
                flushLogger lg
            contents <- readFile' path
            contents `shouldSatisfy` isInfixOf "flush-marker-99"
