module Hat.PathSpec (spec) where

import Test.Hspec

import System.Posix.User (getRealUserID, getUserEntryForID, userName, homeDirectory)

import Hat.Path

spec :: Spec
spec = do
    describe "hatPath" $ do
        it "renders a plain path unchanged" $
            render (hatPath "/tmp/hat-1000") `shouldBe` "/tmp/hat-1000"
        it "normalizes redundant separators" $
            render (hatPath "/tmp//hat-1000/") `shouldBe` "/tmp/hat-1000"

    describe "fitsSocketAddr" $ do
        it "accepts a path a sockaddr_un holds" $
            fitsSocketAddr (replicate 107 'x') `shouldBe` True
        it "rejects one that fills sun_path with no room to terminate it" $
            fitsSocketAddr (replicate 108 'x') `shouldBe` False
        it "counts the bytes the path encodes to, not its characters" $ do
            fitsSocketAddr (replicate 54 '\x00e9') `shouldBe` False
            fitsSocketAddr (replicate 53 '\x00e9') `shouldBe` True
            fitsSocketAddr (replicate 107 '\xdcff') `shouldBe` True

    describe "</:>" $ do
        it "joins with a single separator regardless of trailing slash" $ do
            render (hatPath "/tmp" </:> "default")
                `shouldBe` "/tmp/default"
            render (hatPath "/tmp/" </:> "default")
                `shouldBe` "/tmp/default"

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

    describe "expandTilde" $
        it "expands ~<current-user> to the current user's home" $ do
            entry <- getUserEntryForID =<< getRealUserID
            let name = userName entry
            expanded <- expandTilde Nothing ('~' : name)
            expanded `shouldBe` homeDirectory entry

-- | A named-user resolver that knows no users, for cases exercising only the
-- @~@/@~/@ paths.
noUser :: String -> Maybe FilePath
noUser _ = Nothing

-- | A named-user resolver backed by a fixed table.
byUser :: [(String, FilePath)] -> String -> Maybe FilePath
byUser table name = lookup name table
