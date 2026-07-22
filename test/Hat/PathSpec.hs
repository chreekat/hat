module Hat.PathSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import System.Posix.User (getRealUserID, getUserEntryForID, userName, homeDirectory)

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
            expandTildeWith noUser (Just "/home/b") "~/notes.txt"
                `shouldBe` "/home/b/notes.txt"
        it "joins with a single separator (no double slash)" $
            expandTildeWith noUser (Just "/home/b/") "~/notes.txt"
                `shouldBe` "/home/b/notes.txt"
        it "leaves the path untouched when home is unknown" $
            expandTildeWith noUser Nothing "~/notes.txt"
                `shouldBe` "~/notes.txt"
        it "leaves an absolute path untouched" $
            expandTildeWith noUser (Just "/home/b") "/etc/passwd"
                `shouldBe` "/etc/passwd"
        it "expands a bare ~user to that user's home" $
            expandTildeWith (byUser [("backup", "/var/backups")]) (Just "/home/b") "~backup"
                `shouldBe` "/var/backups"
        it "expands ~user/subpath under that user's home" $
            expandTildeWith (byUser [("backup", "/var/backups")]) (Just "/home/b") "~backup/db"
                `shouldBe` "/var/backups/db"
        it "leaves ~user untouched when the user is unknown" $
            expandTildeWith noUser (Just "/home/b") "~nosuchuser/db"
                `shouldBe` "~nosuchuser/db"
        it "is idempotent for an absolute home" $
            property $ \(AbsHome home) p ->
                let e = expandTildeWith noUser home p
                in expandTildeWith noUser home e `shouldBe` e

    describe "expandTilde" $
        it "expands ~<current-user> to the current user's home" $ do
            entry <- getUserEntryForID =<< getRealUserID
            let name = userName entry
            expanded <- expandTilde ('~' : name)
            expanded `shouldBe` homeDirectory entry

-- | A named-user resolver that knows no users, for cases exercising only the
-- @~@/@~/@ paths.
noUser :: String -> Maybe FilePath
noUser _ = Nothing

-- | A named-user resolver backed by a fixed table.
byUser :: [(String, FilePath)] -> String -> Maybe FilePath
byUser table name = lookup name table

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
