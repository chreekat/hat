module Hat.Client.RexSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec

import Hat.Client.Rex
import Hat.Client.RexArgs

spec :: Spec
spec = do
    describe "attach --rex arguments" $ do
        let target sock sess = Just (Right RexTarget { socket = sock, session = sess })

        it "takes a socket path" $
            rexAttachTarget ["attach", "--rex", "/run/rex/server.sock"]
                `shouldBe` target "/run/rex/server.sock" Nothing

        it "takes a unix:// endpoint" $
            rexAttachTarget ["attach-session", "--rex", "unix:///run/rex/server.sock"]
                `shouldBe` target "/run/rex/server.sock" Nothing

        it "takes a session with -t, before or after, apart or joined" $ do
            rexAttachTarget ["attach", "-t", "work", "--rex", "s.sock"] `shouldBe` target "s.sock" (Just "work")
            rexAttachTarget ["attach", "--rex", "s.sock", "-twork"] `shouldBe` target "s.sock" (Just "work")

        it "leaves every other command line to hat's own server" $ do
            rexAttachTarget ["attach", "-t", "work"] `shouldBe` Nothing
            rexAttachTarget ["send-keys", "--rex", "x"] `shouldBe` Nothing
            rexAttachTarget [] `shouldBe` Nothing

        it "refuses --rex with no endpoint" $ do
            rexAttachTarget ["attach", "--rex"] `shouldSatisfy` maybe False isLeft
            rexAttachTarget ["attach", "--rex", "-t", "work"] `shouldSatisfy` maybe False isLeft
            rexAttachTarget ["attach", "--rex", "unix://"] `shouldSatisfy` maybe False isLeft

        it "refuses an endpoint that is not a Unix socket" $
            rexAttachTarget ["attach", "--rex", "https://host.example/"] `shouldBe` Just
                (Left "only Unix socket endpoints are supported, not https://host.example/")

        it "refuses arguments it does not implement" $ do
            rexAttachTarget ["attach", "--rex", "s.sock", "-d"]
                `shouldBe` Just (Left "attach --rex does not take -d")
            rexAttachTarget ["attach", "--rex", "a.sock", "--rex", "b.sock"]
                `shouldSatisfy` maybe False isLeft

    describe "detach keys" $ do
        it "detaches on C-b d, in one read or two" $ do
            detachKeys AwaitingKey "\x02\&d" `shouldBe` Nothing
            (detachKeys AwaitingKey "\x02" >>= \st -> detachKeys st "d") `shouldBe` Nothing

        it "detaches on a bare q" $
            detachKeys AwaitingKey "abq" `shouldBe` Nothing

        it "takes a d without the prefix, or a prefix without the d, as nothing" $ do
            detachKeys AwaitingKey "d" `shouldBe` Just AwaitingKey
            detachKeys AwaitingKey "\x02x" `shouldBe` Just AwaitingKey
            detachKeys AwaitingKey "\x02xd" `shouldBe` Just AwaitingKey

        it "does not take the key after the prefix as a bare q" $
            detachKeys AwaitingKey "\x02q" `shouldBe` Just AwaitingKey
