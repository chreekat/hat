module Hat.Rex.SessionSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec

import Hat.Rex.Conn (Close (..))
import Hat.Rex.Control
import Hat.Rex.Session
import Hat.Rex.View

spec :: Spec
spec = do
    let entry sid label = SessionEntry { sessionId = SessionId sid, label }
        sessions = [entry "session:1" "main", entry "session:2" "work", entry "session:3" "work"]

    describe "choosing a session" $ do
        it "takes the first listed when none is asked for" $
            chooseSession Nothing sessions `shouldBe` Right (entry "session:1" "main")

        it "takes the first with the label asked for" $
            chooseSession (Just "work") sessions `shouldBe` Right (entry "session:2" "work")

        it "takes one by its id" $
            chooseSession (Just "session:3") sessions `shouldBe` Right (entry "session:3" "work")

        it "refuses a session that is not there, and a server with none" $ do
            chooseSession (Just "play") sessions `shouldBe` Left "can't find session: play"
            chooseSession Nothing [] `shouldSatisfy` isLeft

    describe "the pane to show" $ do
        let block n creator = Block { blockId = BlockId n, creatorName = creator }
            terminal n = block n (Just "com.superlogical.terminal")
            window n focused blocks = Window
                { windowId = WindowId n, label = "", focusedBlock = BlockId <$> focused, blocks }
            view active windows = View
                { revision = 1, sessionId = SessionId "session:1", label = "main"
                , activeWindow = WindowId <$> active, windows }

        it "is the active window's focused block" $
            focusedPane (view (Just "w2")
                    [ window "w1" (Just "b1") [terminal "b1"]
                    , window "w2" (Just "b3") [terminal "b2", terminal "b3"] ])
                `shouldBe` Right (BlockId "b3")

        it "is refused when the block is not a terminal" $ do
            focusedPane (view (Just "w1") [window "w1" (Just "b1") [block "b1" (Just "com.superlogical.tunnel")]])
                `shouldBe` Left "the focused pane is not a terminal (its creator is com.superlogical.tunnel)"
            focusedPane (view (Just "w1") [window "w1" (Just "b1") [block "b1" Nothing]])
                `shouldSatisfy` isLeft

        it "is refused when there is no window or no focus" $ do
            focusedPane (view Nothing []) `shouldSatisfy` isLeft
            focusedPane (view (Just "w1") [window "w1" Nothing [terminal "b1"]]) `shouldSatisfy` isLeft

    describe "the end of an attach" $ do
        let sid = SessionId "session:1"
            bid = BlockId "b1"

        it "comes with the session's destruction or the pane's close" $ do
            endsAttach sid bid (SessionDestroyed sid) `shouldBe` True
            endsAttach sid bid (SessionEvent sid (BlockClosed bid)) `shouldBe` True

        it "does not come with another session's or pane's" $ do
            endsAttach sid bid (SessionDestroyed (SessionId "session:2")) `shouldBe` False
            endsAttach sid bid (SessionEvent sid (BlockClosed (BlockId "b2"))) `shouldBe` False
            endsAttach sid bid (SessionEvent sid (UnknownSessionEvent "title_changed")) `shouldBe` False

        it "comes quietly with a closed block's or a finished stream's close" $ do
            streamEnd Close { code = 4000, reason = "block closed" } `shouldBe` PaneGone
            streamEnd Close { code = 1000, reason = "" } `shouldBe` PaneGone

        it "comes with the server's reason for any other close" $
            streamEnd Close { code = 1013, reason = "data reconnect required" }
                `shouldBe` StreamBroken "the pane's stream closed: data reconnect required (status 1013)"
