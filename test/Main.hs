import Test.Hspec

import qualified Hat.PtySpec
import qualified Hat.SocketSpec

main :: IO ()
main = hspec $ do
    describe "Hat.Socket" Hat.SocketSpec.spec
    describe "Hat.Pty" Hat.PtySpec.spec
