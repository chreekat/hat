module Hat.Rex.LogSpec (spec) where

import Test.Hspec

import Hat.Rex.Log

spec :: Spec
spec = describe "Rex attach log" $ do
    it "records a message with its direction, one per line" $
        controlLine ClientToServer "{\"type\":\"request\",\"id\":\"1\",\"method\":\"session.list\"}"
            `shouldBe` "{\"dir\":\"c2s\",\"msg\":{\"id\":\"1\",\"method\":\"session.list\",\"type\":\"request\"}}\n"

    it "never records a reconnect token or a data ticket" $ do
        controlLine ServerToClient
            "{\"type\":\"welcome\",\"client\":{\"id\":\"client:1\",\"reconnect_token\":\"s3cret\"}}"
            `shouldBe` "{\"dir\":\"s2c\",\"msg\":{\"client\":{\"id\":\"client:1\",\"reconnect_token\":\"<redacted>\"},\"type\":\"welcome\"}}\n"
        controlLine ServerToClient
            "{\"type\":\"response\",\"id\":\"3\",\"seq\":1,\"payload\":{\"data_tickets\":[{\"ticket\":\"AAECAw\"}]}}"
            `shouldBe` "{\"dir\":\"s2c\",\"msg\":{\"id\":\"3\",\"payload\":{\"data_tickets\":[{\"ticket\":\"<redacted>\"}]},\"seq\":1,\"type\":\"response\"}}\n"

    it "records what is not JSON as text" $
        controlLine ServerToClient "oops" `shouldBe` "{\"dir\":\"s2c\",\"msg\":\"oops\"}\n"
