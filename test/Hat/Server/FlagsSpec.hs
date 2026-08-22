module Hat.Server.FlagsSpec (spec) where

import Test.Hspec

import Hat.Server (WindowFlagState (..), windowFlags)

noFlags :: WindowFlagState
noFlags = WindowFlagState
    { flagCurrent = False
    , flagLast = False
    , flagBell = False
    , flagActivity = False
    , flagSilence = False
    , flagZoomed = False
    }

spec :: Spec
spec = do
    it "renders no flags for a plain window" $
        windowFlags noFlags `shouldBe` ""

    it "renders * for the current window" $
        windowFlags noFlags { flagCurrent = True } `shouldBe` "*"

    it "renders Z for a zoomed window" $
        windowFlags noFlags { flagZoomed = True } `shouldBe` "Z"

    it "appends Z after the other flags, matching tmux" $
        windowFlags noFlags { flagCurrent = True, flagZoomed = True }
            `shouldBe` "*Z"

    it "combines every flag in tmux order" $
        windowFlags WindowFlagState
            { flagCurrent = True
            , flagLast = False
            , flagBell = True
            , flagActivity = True
            , flagSilence = True
            , flagZoomed = True
            } `shouldBe` "*!#~Z"

    it "does not render Z when the window is not zoomed" $
        windowFlags noFlags { flagCurrent = True } `shouldBe` "*"
