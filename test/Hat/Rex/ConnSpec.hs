module Hat.Rex.ConnSpec (spec) where

import Test.Hspec

import Hat.Rex.Conn

spec :: Spec
spec = describe "a refused WebSocket upgrade" $ do
    it "is worded from the JSON body's error" $
        refusalText 403 "{\"error\": \"user not allowed\"}\n"
            `shouldBe` "HTTP 403: user not allowed"

    it "quotes a body that is not that JSON" $
        refusalText 404 "404 page not found\n" `shouldBe` "HTTP 404: 404 page not found"

    it "is the bare status when there is no body" $
        refusalText 502 "" `shouldBe` "HTTP 502"
