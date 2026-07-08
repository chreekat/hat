module Hat.Server.InputSpec (spec) where

import qualified Data.ByteString as B
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Server.Input

prefix :: Word8
prefix = 0x02  -- C-b

route :: KeyState -> B.ByteString -> (KeyState, [InputAction])
route = routeInput prefix

spec :: Spec
spec = do
    it "passes plain bytes through" $
        route Normal "hello" `shouldBe` (Normal, [ToPane "hello"])

    it "swallows the prefix and waits" $
        route Normal "ab\x02" `shouldBe` (AwaitingPrefixed, [ToPane "ab"])

    it "emits the prefixed key" $
        route AwaitingPrefixed "d" `shouldBe` (Normal, [Prefixed 0x64])

    it "handles prefix and key split across chunks" $ do
        let (st1, acts1) = route Normal "\x02"
            (st2, acts2) = route st1 "d"
        (st2, acts1 <> acts2) `shouldBe` route Normal "\x02\&d"

    it "resumes passthrough after the prefixed key" $
        route Normal "\x02\&dxy" `shouldBe` (Normal, [Prefixed 0x64, ToPane "xy"])

    it "emits prefix-prefix as a prefixed key too" $
        route Normal "\x02\x02" `shouldBe` (Normal, [Prefixed 0x02])

    prop "prefix-free input is untouched" $ \bytes ->
        let clean = B.pack (filter (/= prefix) bytes)
        in not (B.null clean) ==>
            route Normal clean === (Normal, [ToPane clean])

    prop "splitting a stream anywhere gives the same actions" $ \bytes k ->
        let bs = B.pack bytes
            i = if B.null bs then 0 else k `mod` B.length bs
            (a, b) = B.splitAt i bs
            runAll st chunks = foldl step (st, []) chunks
            step (st, acc) chunk =
                let (st', acts) = route st chunk in (st', acc <> acts)
        in normalize (snd (runAll Normal [a, b]))
            === normalize (snd (runAll Normal [bs]))

-- Adjacent ToPane chunks merge; empty ones drop.
normalize :: [InputAction] -> [InputAction]
normalize = foldr merge []
  where
    merge (ToPane a) rest | B.null a = rest
    merge (ToPane a) (ToPane b : rest) = ToPane (a <> b) : rest
    merge x rest = x : rest
