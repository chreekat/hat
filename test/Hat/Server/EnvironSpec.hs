-- | The environment engine: entry states (valued, cleared, hidden), the
-- show-environment output forms, glob matching, and scope merging.
module Hat.Server.EnvironSpec (spec) where

import Test.Hspec

import Hat.Server.Environ

spec :: Spec
spec = do
    describe "entry operations" $ do
        it "set then find returns the value" $ do
            let env = environSet EnvVisible "FOO" "bar" emptyEnviron
            environFind "FOO" env
                `shouldBe` Just (EnvEntry (Just "bar") EnvVisible)

        it "set overwrites value and visibility" $ do
            let env = environSet EnvVisible "FOO" "baz"
                    (environSet EnvHidden "FOO" "bar" emptyEnviron)
            environFind "FOO" env
                `shouldBe` Just (EnvEntry (Just "baz") EnvVisible)

        it "clear keeps the entry but drops the value" $ do
            let env = environClear "FOO"
                    (environSet EnvVisible "FOO" "bar" emptyEnviron)
            environFind "FOO" env
                `shouldBe` Just (EnvEntry Nothing EnvVisible)

        it "clear preserves a hidden entry's visibility" $ do
            let env = environClear "SEC"
                    (environSet EnvHidden "SEC" "s" emptyEnviron)
            environFind "SEC" env
                `shouldBe` Just (EnvEntry Nothing EnvHidden)

        it "clear of an absent name creates a cleared entry" $
            environFind "GONE" (environClear "GONE" emptyEnviron)
                `shouldBe` Just (EnvEntry Nothing EnvVisible)

        it "unset removes the entry entirely" $ do
            let env = environUnset "FOO"
                    (environSet EnvVisible "FOO" "bar" emptyEnviron)
            environFind "FOO" env `shouldBe` Nothing

    describe "spawn-facing pairs" $ do
        it "drops hidden and cleared entries" $ do
            let env = environSet EnvVisible "A" "1"
                    . environSet EnvHidden "SEC" "s"
                    . environClear "GONE"
                    $ environFromPairs [("B", "2")]
            environPairs env `shouldBe` [("A", "1"), ("B", "2")]

        it "a cleared session entry masks the global variable" $ do
            let global = environFromPairs [("KEEP", "g"), ("MASKED", "g")]
                sess = environClear "MASKED" emptyEnviron
            environPairs (environMerge global sess)
                `shouldBe` [("KEEP", "g")]

        it "a session value shadows the global one" $ do
            let global = environFromPairs [("X", "old")]
                sess = environFromPairs [("X", "new")]
            environPairs (environMerge global sess)
                `shouldBe` [("X", "new")]

    describe "renderEnvLine" $ do
        it "plain: NAME=value" $
            renderEnvLine EnvPlain "FOO" (EnvEntry (Just "bar") EnvVisible)
                `shouldBe` "FOO=bar"
        it "plain: a cleared entry prints -NAME" $
            renderEnvLine EnvPlain "FOO" (EnvEntry Nothing EnvVisible)
                `shouldBe` "-FOO"
        it "shell: quoted assignment plus export" $
            renderEnvLine EnvShellExport "FOO" (EnvEntry (Just "bar") EnvVisible)
                `shouldBe` "FOO=\"bar\"; export FOO;"
        it "shell: escapes $ ` \" and backslash" $
            renderEnvLine EnvShellExport "ESC"
                (EnvEntry (Just "a$b`c\"d\\e") EnvVisible)
                `shouldBe` "ESC=\"a\\$b\\`c\\\"d\\\\e\"; export ESC;"
        it "shell: a cleared entry prints unset NAME;" $
            renderEnvLine EnvShellExport "FOO" (EnvEntry Nothing EnvVisible)
                `shouldBe` "unset FOO;"

    describe "globMatch" $ do
        it "a literal pattern matches only itself" $ do
            globMatch "MYVAR" "MYVAR" `shouldBe` True
            globMatch "MYVAR" "MYVAR2" `shouldBe` False
        it "* matches any run including empty" $ do
            globMatch "TEST_*" "TEST_GLOB" `shouldBe` True
            globMatch "TEST_*" "TEST_" `shouldBe` True
            globMatch "TEST_*" "OTHER" `shouldBe` False
        it "? matches exactly one character" $ do
            globMatch "A?C" "ABC" `shouldBe` True
            globMatch "A?C" "AC" `shouldBe` False
