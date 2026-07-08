import Test.Hspec

import qualified Hat.IntegrationSpec
import qualified Hat.PtySpec
import qualified Hat.Server.InputSpec
import qualified Hat.Server.RenderSpec
import qualified Hat.SocketSpec
import qualified Hat.Term.EmulatorSpec
import qualified Hat.Term.GoldenSpec
import qualified Hat.WireSpec

main :: IO ()
main = hspec $ do
    describe "Hat.Socket" Hat.SocketSpec.spec
    describe "Hat.Pty" Hat.PtySpec.spec
    describe "Hat.Term.Emulator" Hat.Term.EmulatorSpec.spec
    describe "Hat.Term golden" Hat.Term.GoldenSpec.spec
    describe "Hat.Wire" Hat.WireSpec.spec
    describe "Hat.Server.Render" Hat.Server.RenderSpec.spec
    describe "Hat.Server.Input" Hat.Server.InputSpec.spec
    describe "integration" Hat.IntegrationSpec.spec
