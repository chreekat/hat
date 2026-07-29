{-# LANGUAGE ScopedTypeVariables #-}

module Hat.LogSpec (spec) where

import Control.Exception (IOException, catch)
import Data.List (isInfixOf)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (readFile')
import Test.Hspec

import Hat.Log

spec :: Spec
spec = do
    -- The bracketed lifetime flushes queued events and tears the drain thread
    -- + handle down on scope exit, structurally — no hand-called close. Reading
    -- the file back proves the queued line reached disk before the scope ended.
    it "withLogger flushes queued events on scope exit" $ do
        dir <- getTemporaryDirectory
        let path = dir <> "/hat-logspec-with.log"
        removeFile path `catch` \(_ :: IOException) -> pure ()
        withLogger path $ \lg ->
            logEvent lg ServerCrash { err = "with-marker-42" }
        contents <- readFile' path
        contents `shouldSatisfy` isInfixOf "with-marker-42"

    -- The teardown must run even when the body throws, so a crash mid-scope
    -- still leaves its queued events on disk rather than losing them to an
    -- abandoned drain thread.
    it "withLogger flushes even when the body throws" $ do
        dir <- getTemporaryDirectory
        let path = dir <> "/hat-logspec-throw.log"
        removeFile path `catch` \(_ :: IOException) -> pure ()
        (withLogger path $ \lg -> do
            logEvent lg ServerCrash { err = "throw-marker-7" }
            ioError (userError "boom"))
            `catch` \(_ :: IOException) -> pure ()
        contents <- readFile' path
        contents `shouldSatisfy` isInfixOf "throw-marker-7"

    -- The explicit flush is what lets the fatal trace and the restart-server
    -- self-exec drain the queue before the process is replaced. A flush that
    -- returned early would leave the marker unwritten when the scope exits.
    it "flushLogger drains the queue before it returns" $ do
        dir <- getTemporaryDirectory
        let path = dir <> "/hat-logspec-flush.log"
        removeFile path `catch` \(_ :: IOException) -> pure ()
        withLogger path $ \lg -> do
            logEvent lg ServerCrash { err = "flush-marker-99" }
            flushLogger lg
        contents <- readFile' path
        contents `shouldSatisfy` isInfixOf "flush-marker-99"
