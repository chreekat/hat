module Hat.DebugSpec (spec) where

import Test.Hspec

import Hat.Debug

spec :: Spec
spec = do
    describe "debugSocketPath" $ do
        it "names the socket after the server's own socket" $
            debugSocketPath "/home/b/.local/share" "/tmp/hat-1000/default"
                `shouldBe`
                "/home/b/.local/share/ghc-debug/debuggee/sockets/hat-default"
        it "puts it in the directory ghc-debug-brick discovers" $
            debugSocketPath "/x" "/tmp/hat-1000/work"
                `shouldBe` debugSocketDir "/x" <> "/hat-work"
        it "joins with a single separator" $
            debugSocketPath "/x/" "/tmp/hat-1000//default"
                `shouldBe` "/x/ghc-debug/debuggee/sockets/hat-default"

    describe "fitsSocketAddr" $ do
        it "accepts a path a sockaddr_un holds" $
            fitsSocketAddr (replicate 107 'x') `shouldBe` True
        it "rejects one that fills sun_path with no room to terminate it" $
            fitsSocketAddr (replicate 108 'x') `shouldBe` False

    describe "boundSocketInodes" $ do
        it "picks out the sockets bound under the directory" $
            boundSocketInodes "/x/sockets" procNetUnix `shouldBe` [17625936, 17761356]
        it "finds every generation stranded at one path" $
            -- an exec'd-through listener keeps its name after the unlink
            boundSocketInodes "/x/sockets" (procNetUnix <> repeatLine)
                `shouldBe` [17625936, 17761356, 99]
        it "ignores sockets bound elsewhere" $
            boundSocketInodes "/nowhere" procNetUnix `shouldBe` []
        it "does not mistake a sibling directory for the directory" $
            boundSocketInodes "/x/socket" procNetUnix `shouldBe` []
        it "reads a path that contains a space" $
            boundSocketInodes "/x y/sockets"
                ("0000000000000000: 00000002 00000000 00010000 0001 01 7 "
                    <> "/x y/sockets/hat-default\n")
                `shouldBe` [7]
        it "ignores unnamed sockets" $
            boundSocketInodes "/x/sockets" unnamedOnly `shouldBe` []
        it "reads an empty table" $
            boundSocketInodes "/x/sockets" "" `shouldBe` []

    describe "fdSocketInode" $ do
        it "reads the inode out of a socket link" $
            fdSocketInode "socket:[17625936]" `shouldBe` Just 17625936
        it "ignores a non-socket fd" $ do
            fdSocketInode "/dev/pts/3" `shouldBe` Nothing
            fdSocketInode "anon_inode:[eventfd]" `shouldBe` Nothing

-- A @\/proc\/net\/unix@ table: the header, an unnamed socket, hat's own
-- listening socket, and two ghc-debug sockets sharing one path.
procNetUnix :: String
procNetUnix = unlines
    [ "Num       RefCount Protocol Flags    Type St Inode Path"
    , "0000000000000000: 00000002 00000000 00000000 0001 03 12345"
    , "0000000000000000: 00000003 00000000 00000000 0001 03 85048826 /tmp/hat-1000/default"
    , "0000000000000000: 00000002 00000000 00010000 0001 01 17625936 /x/sockets/hat-default"
    , "0000000000000000: 00000002 00000000 00010000 0001 01 17761356 /x/sockets/hat-default"
    , "0000000000000000: 00000002 00000000 00000000 0001 03 55555 /x/sockets-old/hat-default"
    ]

repeatLine :: String
repeatLine =
    "0000000000000000: 00000002 00000000 00010000 0001 01 99 /x/sockets/hat-default\n"

unnamedOnly :: String
unnamedOnly = unlines
    [ "Num       RefCount Protocol Flags    Type St Inode Path"
    , "0000000000000000: 00000002 00000000 00000000 0001 03 12345"
    ]
