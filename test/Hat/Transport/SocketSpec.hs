module Hat.Transport.SocketSpec (spec) where

import Test.Hspec

import Hat.Transport.Socket

spec :: Spec
spec = do
    describe "socketDir" $ do
        it "defaults to /tmp/hat-UID" $
            socketDir Nothing 1000 `shouldBe` "/tmp/hat-1000"
        it "honors TMUX_TMPDIR" $
            socketDir (Just "/run/user/1000") 1000
                `shouldBe` "/run/user/1000/hat-1000"
    describe "socketPath" $ do
        it "appends the socket name" $
            socketPath Nothing 1000 "default" `shouldBe` "/tmp/hat-1000/default"
