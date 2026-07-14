module Hat.Server.ConfigSpec (spec) where

import Control.Exception (finally)
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.Directory (removeDirectoryRecursive)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

import Hat.Server (readConfigUtf8)

spec :: Spec
spec =
    -- The user's config threw mid-read under a non-UTF-8 locale because
    -- TIO.readFile decodes with the locale; a config with a `·` (U+00B7,
    -- bytes C2 B7) aborted startup. readConfigUtf8 must decode UTF-8
    -- regardless of locale and never throw on a malformed byte.
    describe "readConfigUtf8" $
        it "decodes UTF-8 locale-independently and survives bad bytes" $ do
            dir <- mkdtemp "/tmp/hat-config-"
            flip finally (removeDirectoryRecursive dir) $ do
                let p = dir <> "/c"
                -- ".·" then a lone 0xFF (invalid UTF-8) then newline.
                B.writeFile p (B.pack [0x2e, 0xc2, 0xb7, 0xff, 0x0a])
                t <- readConfigUtf8 p
                -- the middle dot round-trips …
                t `shouldSatisfy` T.isInfixOf (T.singleton '\x00b7')
                -- … and the invalid byte is replaced, not thrown on.
                t `shouldSatisfy` T.isInfixOf (T.singleton '\xfffd')
