module Hat.PathSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Hat.Path

spec :: Spec
spec = do
    describe "hatPath" $ do
        it "renders a plain path unchanged" $
            render (hatPath "/tmp/hat-1000") `shouldBe` "/tmp/hat-1000"
        it "normalizes redundant separators" $
            render (hatPath "/tmp//hat-1000/") `shouldBe` "/tmp/hat-1000"
        it "is idempotent through render" $
            property $ \p ->
                render (hatPath (render (hatPath p)))
                    `shouldBe` render (hatPath p)

    describe "</:>" $ do
        it "joins with a single separator regardless of trailing slash" $ do
            render (hatPath "/tmp" </:> "default")
                `shouldBe` "/tmp/default"
            render (hatPath "/tmp/" </:> "default")
                `shouldBe` "/tmp/default"
        it "matches System.FilePath's </> semantics for the built path" $
            render (hatPath "/a/b" </:> "c" </:> "d")
                `shouldBe` "/a/b/c/d"

    describe "expandTildeWith" $ do
        it "expands a ~/ prefix against the given home" $
            expandTildeWith (Just "/home/b") "~/notes.txt"
                `shouldBe` "/home/b/notes.txt"
        it "joins with a single separator (no double slash)" $
            expandTildeWith (Just "/home/b/") "~/notes.txt"
                `shouldBe` "/home/b/notes.txt"
        it "leaves the path untouched when home is unknown" $
            expandTildeWith Nothing "~/notes.txt"
                `shouldBe` "~/notes.txt"
        it "leaves a bare ~ (not ~/) untouched" $
            expandTildeWith (Just "/home/b") "~backup"
                `shouldBe` "~backup"
        it "leaves an absolute path untouched" $
            expandTildeWith (Just "/home/b") "/etc/passwd"
                `shouldBe` "/etc/passwd"
        it "is idempotent for an absolute home" $
            property $ \(AbsHome home) p ->
                let e = expandTildeWith home p
                in expandTildeWith home e `shouldBe` e

-- | A @HOME@ value that is either unset or an absolute path, mirroring how
-- the environment actually presents it.
newtype AbsHome = AbsHome (Maybe FilePath)
    deriving (Show)

instance Arbitrary AbsHome where
    arbitrary = AbsHome <$> oneof
        [ pure Nothing
        , Just . ('/' :) <$> arbitrary
        ]
    shrink (AbsHome Nothing) = []
    shrink (AbsHome (Just h)) =
        AbsHome Nothing : [ AbsHome (Just ('/' : h')) | h' <- shrink (drop 1 h) ]
