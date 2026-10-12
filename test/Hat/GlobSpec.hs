module Hat.GlobSpec (spec) where

import Control.Exception (bracket)
import System.Directory (createDirectory, removeDirectoryRecursive)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

import Hat.Glob (globMatch, globPaths)

spec :: Spec
spec = do
    describe "globPaths" $
        it "names the existing paths a pattern matches, in order, dotfiles only explicitly" $
            bracket (mkdtemp "/tmp/hat-glob-") removeDirectoryRecursive $ \dir -> do
                createDirectory (dir <> "/d")
                mapM_ (\f -> writeFile (dir <> f) "") ["/d/b.conf", "/d/a.conf", "/d/.h.conf", "/d/x"]
                globPaths (dir <> "/d/*.conf") `shouldReturn` [dir <> "/d/a.conf", dir <> "/d/b.conf"]
                globPaths (dir <> "/*/.h*") `shouldReturn` [dir <> "/d/.h.conf"]
                globPaths (dir <> "/d/x") `shouldReturn` [dir <> "/d/x"]
                globPaths (dir <> "/d/none") `shouldReturn` []

    describe "literals" $ do
        it "a literal pattern matches only itself" $ do
            globMatch False "MYVAR" "MYVAR" `shouldBe` True
            globMatch False "MYVAR" "MYVAR2" `shouldBe` False
        it "requires a full match" $
            globMatch False "al" "alpha" `shouldBe` False

    describe "wildcards" $ do
        it "* matches any run including empty" $ do
            globMatch False "al*" "alpha" `shouldBe` True
            globMatch False "TEST_*" "TEST_GLOB" `shouldBe` True
            globMatch False "TEST_*" "TEST_" `shouldBe` True
            globMatch False "al*" "beta" `shouldBe` False
            globMatch False "TEST_*" "OTHER" `shouldBe` False
        it "? matches exactly one character" $ do
            globMatch False "a?pha" "alpha" `shouldBe` True
            globMatch False "A?C" "ABC" `shouldBe` True
            globMatch False "A?C" "AC" `shouldBe` False

    describe "character classes" $ do
        it "matches a listed character" $ do
            globMatch False "grp[12]" "grp1" `shouldBe` True
            globMatch False "grp[12]" "grp3" `shouldBe` False
        it "negates via ! and ^" $ do
            globMatch False "grp[!12]" "grp3" `shouldBe` True
            globMatch False "grp[!12]" "grp1" `shouldBe` False
            globMatch False "grp[^12]" "grp3" `shouldBe` True
            globMatch False "grp[^12]" "grp1" `shouldBe` False
        it "matches ranges" $ do
            globMatch False "[a-c]" "b" `shouldBe` True
            globMatch False "[a-c]" "d" `shouldBe` False
            globMatch False "[!a-c]" "d" `shouldBe` True
        it "] first in a class is a member, not the close" $ do
            globMatch False "[]a]" "]" `shouldBe` True
            globMatch False "[]a]" "a" `shouldBe` True
            globMatch False "[]a]" "b" `shouldBe` False
        it "an unmatched [ is a literal" $ do
            globMatch False "a[b" "a[b" `shouldBe` True
            globMatch False "a[b" "ab" `shouldBe` False

    describe "escapes" $ do
        it "a backslash makes a wildcard literal" $ do
            globMatch False "\\*" "*" `shouldBe` True
            globMatch False "\\*" "x" `shouldBe` False
            globMatch False "a\\?c" "a?c" `shouldBe` True
            globMatch False "a\\?c" "abc" `shouldBe` False

    describe "case folding" $ do
        it "matches case-insensitively only when asked" $ do
            globMatch True "ABC" "abc" `shouldBe` True
            globMatch True "a?c" "AbC" `shouldBe` True
            globMatch False "ABC" "abc" `shouldBe` False
