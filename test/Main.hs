import Test.Hspec

import qualified Hat.PtySpec
import qualified Hat.SocketSpec
import qualified Hat.Term.EmulatorSpec
import qualified Hat.Term.GoldenSpec

main :: IO ()
main = hspec $ do
    describe "Hat.Socket" Hat.SocketSpec.spec
    describe "Hat.Pty" Hat.PtySpec.spec
    describe "Hat.Term.Emulator" Hat.Term.EmulatorSpec.spec
    describe "Hat.Term golden" Hat.Term.GoldenSpec.spec
