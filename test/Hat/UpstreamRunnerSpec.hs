module Hat.UpstreamRunnerSpec (spec) where

import Control.Exception (bracket)
import Data.List (isInfixOf)
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.Posix.Temp (mkdtemp)
import System.Process (proc, readCreateProcessWithExitCode)
import Test.Hspec

-- | Run @tools/run-upstream-tests.sh@ over a fake tmux tree whose regress
-- scripts exit with the given codes.
runFake :: [(String, Int)] -> IO (ExitCode, String)
runFake scripts =
    bracket (mkdtemp "/tmp/hat-upstream-") removeDirectoryRecursive $ \src -> do
        createDirectory (src <> "/regress")
        mapM_ (\(n, c) -> writeFile (src <> "/regress/" <> n) ("exit " <> show c <> "\n"))
            scripts
        (code, out, err) <- readCreateProcessWithExitCode
            (proc "env" ["HAT_BIN=/bin/sh", "tools/run-upstream-tests.sh", src, "-j", "2"])
            ""
        pure (code, out <> err)

spec :: Spec
spec = describe "run-upstream-tests.sh" $
    it "reports a racy script's pass or fail without failing the suite" $ do
        (code, out) <- runFake [("input-keys.sh", 1), ("pipe-pane.sh", 0)]
        code `shouldBe` ExitSuccess
        out `shouldSatisfy` ("RFAIL" `isInfixOf`)
        out `shouldSatisfy` ("RPASS" `isInfixOf`)
        out `shouldSatisfy` ("racy: pass=1 fail=1" `isInfixOf`)
