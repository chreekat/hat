-- | The paste-buffer commands ('Hat.Server.Command.Buffer') against a real
-- server state and a real temp file.
module Hat.Server.BufferSpec (spec) where

import Control.Exception (bracket)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (removeDirectoryRecursive)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model (newServerState)
import Hat.Server.Command.Buffer (cmdSaveBuffer, cmdSetBuffer)

spec :: Spec
spec = describe "save-buffer" $
    it "writes the top buffer to the given path, and -a appends" $
        bracket (mkdtemp "/tmp/hat-buffer-") removeDirectoryRecursive $ \dir -> do
            lg <- newLogger "/dev/null"
            st <- newServerState Map.empty lg "/tmp/hat-bufferspec.sock" Nothing
            let path = dir <> "/out.txt"
            [] <- cmdSetBuffer st Nothing ["alpha"]
            [] <- cmdSaveBuffer st Nothing [T.pack path]
            readFile path `shouldReturn` "alpha"
            -- a fresh set-buffer pushes a new top buffer; -a appends it.
            [] <- cmdSetBuffer st Nothing ["beta"]
            [] <- cmdSaveBuffer st Nothing ["-a", T.pack path]
            readFile path `shouldReturn` "alphabeta"
