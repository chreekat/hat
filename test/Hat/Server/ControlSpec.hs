{-# LANGUAGE OverloadedStrings #-}

module Hat.Server.ControlSpec (spec) where

import Control.Concurrent.STM
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Test.Hspec

import Hat.Geometry
import Hat.Model.Ids
import Hat.Server.Control
import Hat.Server.ControlQueue
import Hat.Server.ControlTypes
import Hat.Server.Dispatch (resolveCommand)
import Hat.Transport.Wire (ExitWait (..), ServerToClient (..))

lines' :: ControlState -> IO [String]
lines' cs = do
    q <- readTVarIO cs.queue
    pure [ show l | ControlLine l <- toList q ]

spec :: Spec
spec = do
    describe "escapeOutput" $ do
        let esc = BL.toStrict . BB.toLazyByteString . escapeOutput
        it "octal-escapes control bytes and backslash, nothing else" $
            esc "a\ESC[1m\\b\r\n\200" `shouldBe` "a\\033[1m\\134b\\015\\012\200"

    describe "guardLine" $
        it "carries time, number, and the origin's flag" $ do
            guardLine "begin" 1363006971 2 ClientLine
                `shouldBe` "%begin 1363006971 2 1"
            guardLine "end" 5 1 SetupCommand `shouldBe` "%end 5 1 0"

    describe "splitInputLines" $
        it "keeps the unterminated tail, and empty lines as lines" $
            splitInputLines "a\n\nb" `shouldBe` (["a", ""], "b")

    describe "resolveCommand" $ do
        it "takes a unique prefix of a command name" $
            resolveCommand "a" `shouldBe` Right "attach-session"
        it "lists the candidates of an ambiguous prefix" $
            resolveCommand "li" `shouldSatisfy` either (const True) (const False)
        it "never abbreviates to a hat-only command" $
            resolveCommand "restore" `shouldBe` Left "unknown command: restore"

    describe "refresh-client -f" $ do
        it "sets and clears the control flags it names, skipping others" $
            applyControlFlags
                (parseControlFlags "no-output,wait-exit,read-only,pause-after=2")
                defaultControlFlags
                `shouldBe` ControlFlags
                    { output = NoOutput, exitWait = AwaitEmptyLine
                    , pauseAfter = Just 2000 }
        it "clears with !" $
            applyControlFlags (parseControlFlags "!wait-exit,!pause-after")
                defaultControlFlags { exitWait = AwaitEmptyLine, pauseAfter = Just 0 }
                `shouldBe` defaultControlFlags

    describe "refresh-client -C" $ do
        it "reads a client size as WxH or W,H" $ do
            parseControlSize "100,50" `shouldBe` Right (ClientSize (Size 50 100))
            parseControlSize "200x200" `shouldBe` Right (ClientSize (Size 200 200))
        it "reads a window size, and its clearing" $ do
            parseControlSize "@3:40x10"
                `shouldBe` Right (WindowSize (WindowId 3) (Size 10 40))
            parseControlSize "@3:" `shouldBe` Right (ClearWindowSize (WindowId 3))
        it "rejects sizes out of tmux's bounds and garbage" $ do
            parseControlSize "0x5" `shouldBe` Left "size too small or too big"
            parseControlSize "big" `shouldBe` Left "bad size argument"

    describe "the outbound queue" $ do
        it "holds notifications until the open command block closes" $ do
            cs <- newControlState
            atomically $ do
                openBlock cs
                controlEmit cs "%begin"
                controlNotify cs "%window-renamed @0 x"
                controlEmit cs "%end"
                closeBlock cs
            lines' cs `shouldReturn`
                map show ["%begin", "%end", "%window-renamed @0 x" :: String]
        it "drops new notifications and queued output once exiting, not reply lines" $ do
            cs <- newControlState
            atomically $ do
                controlOutput cs (PaneId 1) 0 "out"
                controlNotify cs "%sessions-changed"
                requestExit cs Nothing
                controlNotify cs "%window-renamed @0 late"
                controlEmit cs "%end 1 1 1"
            lines' cs `shouldReturn` map show ["%sessions-changed", "%end 1 1 1" :: String]
            n <- length . toList <$> readTVarIO cs.queue
            n `shouldBe` 2
        it "coalesces a pane's consecutive output" $ do
            cs <- newControlState
            atomically $ do
                controlOutput cs (PaneId 1) 5 "ab"
                controlOutput cs (PaneId 1) 9 "cd"
            q <- toList <$> readTVarIO cs.queue
            [ (p, t, b) | ControlOutput p t b <- q ]
                `shouldBe` [(PaneId 1, 5, "abcd")]
            readTVarIO cs.outputBytes `shouldReturn` 4
        it "turns a detach into an exit with no reason" $ do
            cs <- newControlState
            atomically (controlSend cs DetachOk)
            ex <- readTVarIO cs.exiting
            case ex of
                Exiting Nothing -> pure ()
                _ -> expectationFailure "not exiting without a reason"

