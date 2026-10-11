-- | The paste-buffer store's naming, recency and eviction rules, and the
-- @vis(3)@ renderings of buffer data.
module Hat.Model.PasteBuffersSpec (spec) where

import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.QuickCheck (Positive (..), property)

import Hat.Model.PasteBuffers

spec :: Spec
spec = do
    describe "pasteChanges" $ do
        it "reports evictions as deleted, then the new buffer as changed" $
            pasteChanges (adds ["a", "b"]) (pasteAdd 2 "buffer" 0 "c" (adds ["a", "b"]))
                `shouldBe` [(BufferDeleted, "buffer0"), (BufferChanged, "buffer2")]
        it "reports a rename as the old name deleted and the new one changed" $
            pasteChanges (adds ["a"])
                (fromRight (adds []) (pasteRename "buffer0" "kept" (adds ["a"])))
                `shouldBe` [(BufferDeleted, "buffer0"), (BufferChanged, "kept")]
        it "reports nothing when nothing changed" $
            pasteChanges (adds ["a"]) (adds ["a"]) `shouldBe` []
    describe "pasteAdd" $ do
        it "names buffers bufferN, most recent first" $ do
            let s = adds ["one", "two"]
            listing s `shouldBe` [("buffer1", "two"), ("buffer0", "one")]
            ((.name) <$> pasteTop s) `shouldBe` Just "buffer1"
        it "ignores empty data" $
            listing (adds ["", "x"]) `shouldBe` [("buffer0", "x")]
        it "evicts the oldest automatic buffers past the limit, never named ones" $ do
            let s0 = set (Just "keep") "precious" emptyPasteStore
                s = foldl (flip (pasteAdd 3 "buffer" 0)) s0 ["a1", "a2", "a3", "a4"]
            map snd (listing s) `shouldBe` ["a4", "a3", "a2", "precious"]
        it "keeps counting automatic names after deletions" $ do
            let s = pasteFree "buffer0" (adds ["one"])
            listing (pasteAdd defaultBufferLimit "buffer" 0 "two" s)
                `shouldBe` [("buffer1", "two")]
        it "holds at most limit automatic buffers" $
            property $ \(Positive limit) (named :: [Bool]) ->
                let step s (i, isNamed)
                        | isNamed = set (Just (T.pack ("n" <> show i))) "x" s
                        | otherwise = pasteAdd limit "buffer" 0 "x" s
                    s' = foldl step emptyPasteStore (zip [0 :: Int ..] named)
                    autos = [ pb | pb <- pasteWalk s', pb.origin == Automatic ]
                in length autos <= limit
                    && length (pasteWalk s') - length autos == length (filter id named)

    describe "pasteSet" $ do
        it "replaces a named buffer and moves it to the front" $ do
            let s = set (Just "named") "xyz"
                    (adds ["one"] `with` set (Just "named") "abc")
            listing s `shouldBe` [("named", "xyz"), ("buffer0", "one")]
        it "makes named buffers ineligible as the top" $
            ((.name) <$> pasteTop (set (Just "n") "v" (adds ["one"])))
                `shouldBe` Just "buffer0"
        it "rejects an empty or unprintable name" $ do
            pasteSet 50 (Just "") 0 "x" emptyPasteStore
                `shouldBe` Left "empty buffer name"
            pasteSet 50 (Just "bad\tname") 0 "x" emptyPasteStore
                `shouldBe` Left "invalid buffer name: bad\tname"

    describe "pasteRename" $ do
        it "renames, making the buffer named" $ do
            let s = fromRight emptyPasteStore (pasteRename "buffer0" "other" (adds ["v"]))
            listing s `shouldBe` [("other", "v")]
            pasteTop s `shouldBe` Nothing
        it "reports an unknown source" $
            pasteRename "nosuch" "foo" emptyPasteStore `shouldBe` Left "no buffer nosuch"

    describe "pasteSample" $ do
        it "renders control characters C-style and other bytes in octal" $
            sample "a\tb\nc\\d\ESC\128\195\169"
                `shouldBe` "a\\tb\\nc\\\\d\\033\\200\233"
        it "elides past 200 bytes" $
            sample (mconcat (replicate 201 "x"))
                `shouldBe` T.replicate 200 "x" <> "..."

    describe "pasteVisSafe" $
        it "passes printable text and harmless controls, carets the rest" $
            pasteVisSafe "a\tb\r\ESC[1m\DEL\255\195\169"
                `shouldBe` "a\tb\r^[[1m^?M^?\195\169"
  where
    adds = foldl (flip (pasteAdd defaultBufferLimit "buffer" 0)) emptyPasteStore
    set :: Maybe Text -> ByteString -> PasteStore -> PasteStore
    set mname bytes s = fromRight s (pasteSet defaultBufferLimit mname 0 bytes s)
    with s f = f s
    listing s = [ (pb.name, pb.body) | pb <- pasteWalk s ]
    sample bytes = pasteSample PasteBuffer
        { name = "b", body = bytes, origin = Named, order = 0, created = 0 }
