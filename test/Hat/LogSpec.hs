{-# LANGUAGE ScopedTypeVariables #-}

module Hat.LogSpec (spec) where

import Control.Exception (IOException, catch)
import Data.List (isInfixOf)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (readFile')
import Test.Hspec

import Hat.Log

spec :: Spec
spec =
    -- The fatal path logs the crash cause then flushes before the process
    -- dies; without a real flush the async writer would lose it. This is what
    -- makes a server death show up in the log instead of vanishing to stderr.
    it "flushLogger writes queued events to disk before returning" $ do
        dir <- getTemporaryDirectory
        let path = dir <> "/hat-logspec.log"
        removeFile path `catch` \(_ :: IOException) -> pure ()
        lg <- newLogger path
        logEvent lg ServerFatal { err = "boom-marker-42" }
        closeLogger lg  -- flushes, then releases the writer's file lock
        contents <- readFile' path
        contents `shouldSatisfy` isInfixOf "boom-marker-42"
