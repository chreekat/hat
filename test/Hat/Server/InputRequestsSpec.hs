module Hat.Server.InputRequestsSpec (spec) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Test.Hspec

import Hat.Model.Ids (ClientId (..), PaneId (..))
import Hat.Server.InputRequests
import Hat.Term.HostProtocol (OscTerm (..))

pane1, pane2 :: PaneId
pane1 = PaneId 1
pane2 = PaneId 2

alice, bob :: ClientId
alice = ClientId 1
bob = ClientId 2

-- | Replies a pane makes, in turn, at time 0: what was written at once.
replies :: PaneId -> [ByteString] -> InputRequests -> (InputRequests, [ByteString])
replies pane bs rs0 = foldl step (rs0, []) bs
  where
    step (rs, out) b = let (rs', w) = reply 0 pane b rs in (rs', out <> maybe [] pure w)

spec :: Spec
spec = do
    describe "a pane's replies" $ do
        it "are written at once while nothing is asked" $
            snd (replies pane1 ["a", "b"] noRequests) `shouldBe` ["a", "b"]
        it "wait behind an ask, then follow its answer in order" $ do
            let rs0 = ask 0 pane1 alice (AskPalette 9) TermSt noRequests
                (rs1, now) = replies pane1 ["da"] rs0
                rs2 = ask 0 pane1 alice AskClipboard TermBel rs1
                (rs3, _) = replies pane1 ["cpr"] rs2
                ans = PaletteAnswer 9 (1, 2, 3)
                (rs4, released) = answered alice ans rs3
            now `shouldBe` []
            released `shouldBe` Just (pane1, [Reply TermSt ans, Write "da"])
            map shape <$> Map.lookup pane1 rs4.panes
                `shouldBe` Just [Left AskClipboard, Right "cpr"]
        it "of another pane are not held" $
            snd (replies pane2 ["x"] (ask 0 pane1 alice AskClipboard TermSt noRequests))
                `shouldBe` ["x"]

    describe "an answer" $ do
        it "goes to its client's oldest request of its kind, dropping older others" $ do
            let rs = ask 0 pane1 alice (AskPalette 1) TermSt
                   $ ask 0 pane2 alice AskClipboard TermBel noRequests
                ans = PaletteAnswer 1 (0, 0, 0)
            answered alice ans rs `shouldBe`
                (noRequests { next = 2 }, Just (pane1, [Reply TermSt ans]))
        it "of another palette entry drops the request" $ do
            let rs = ask 0 pane1 alice (AskPalette 1) TermSt noRequests
            answered alice (PaletteAnswer 2 (0, 0, 0)) rs
                `shouldBe` (noRequests { next = 1 }, Nothing)
        it "from another client's terminal is not this one's" $ do
            let rs = ask 0 pane1 alice AskClipboard TermSt noRequests
            answered bob (ClipboardAnswer 'c' "x") rs `shouldBe` (rs, Nothing)

    describe "a request unanswered" $ do
        it "lapses after the timeout, releasing the replies held behind it" $ do
            let rs = fst (replies pane1 ["da"] (ask 0 pane1 alice AskClipboard TermSt noRequests))
            expire requestTimeout pane1 rs `shouldBe` (rs, [])
            expire (requestTimeout + 1) pane1 rs `shouldBe` (rs { panes = mempty }, ["da"])
        it "goes with its client, the replies held behind it staying" $ do
            let rs = fst (replies pane1 ["da"] (ask 0 pane1 alice AskClipboard TermSt noRequests))
            waiting (forgetClient alice rs) `shouldBe` True
            clientWaiting alice (forgetClient alice rs) `shouldBe` False

    describe "a terminal's reply in its input" $ do
        it "is an OSC 52 or OSC 4 sequence up to its BEL or ST" $ do
            scanReply "]52;c;SGk=\aZ" `shouldBe` ReplyComplete 11
            scanReply "]52;c;SGk=\ESC\\Z" `shouldBe` ReplyComplete 12
            scanReply "]4;99;rgb:0101/0202/0303\ESC\\" `shouldBe` ReplyComplete 26
        it "waits for the rest of a sequence cut short" $ do
            scanReply "]5" `shouldBe` ReplyCut
            scanReply "]52;c;SGk" `shouldBe` ReplyCut
            scanReply "]4;99;rgb:01" `shouldBe` ReplyCut
        it "is no reply when it is another sequence, or names no palette entry" $ do
            scanReply "]10;rgb:0/0/0\a" `shouldBe` NotAReply
            scanReply "]4;300;red\a" `shouldBe` NotAReply
            scanReply "]4;x\a" `shouldBe` NotAReply
            scanReply "x" `shouldBe` NotAReply
        it "carries the clipboard's selection and contents, or a palette colour" $ do
            parseAnswer "]52;c;SGk=\a" `shouldBe` Just (ClipboardAnswer 'c' "Hi")
            parseAnswer "]52;;SGk=\ESC\\" `shouldBe` Just (ClipboardAnswer '\NUL' "Hi")
            parseAnswer "]4;99;rgb:0101/0202/0303\ESC\\"
                `shouldBe` Just (PaletteAnswer 99 (1, 2, 3))
        it "carries nothing when its contents do not parse" $ do
            parseAnswer "]52;c;\a" `shouldBe` Nothing
            parseAnswer "]52;c;S*k=\a" `shouldBe` Nothing
            parseAnswer "]4;99;nocolour\a" `shouldBe` Nothing
  where
    shape = \case
        Asking a -> Left a.asked
        Holding h -> Right h.bytes
