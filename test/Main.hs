import Test.Hspec

import qualified Hat.SocketSpec

main :: IO ()
main = hspec $ do
    describe "Hat.Socket" Hat.SocketSpec.spec
