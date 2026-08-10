{-# OPTIONS_GHC -Wno-orphans #-}
module Hat.Server.PersistSpec (spec) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, close, execute_, open)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Server.Persist

-- Characters SQLite TEXT stores faithfully: anything but embedded NUL
-- (truncates via the C API) and lone surrogates (not valid UTF-8).
validChar :: Char -> Bool
validChar c = c /= '\NUL' && (c < '\xD800' || c > '\xDFFF')

genText :: Gen Text
genText = T.pack . filter validChar <$> arbitrary

shrinkText :: Text -> [Text]
shrinkText t = [ T.pack (filter validChar s) | s <- shrink (T.unpack t) ]

-- Strictly-ascending distinct indices, matching the (session_seq, ix)
-- uniqueness the schema enforces on sibling windows.
distinctAscending :: Int -> Gen [Int]
distinctAscending n = do
    start <- choose (0, 2)
    gaps  <- vectorOf n (choose (1, 3))
    pure (drop 1 (scanl (+) start gaps))

genWindow :: Int -> Gen WindowSnap
genWindow wix = do
    nm    <- genText
    lay   <- genText
    npane <- choose (1, 3)
    ps    <- vectorOf npane arbitrary
    act   <- choose (0, npane - 1)
    hist  <- sublistOf (filter (/= act) [0 .. npane - 1])
    auto  <- arbitrary
    pure WindowSnap
        { ix = wix, name = nm, layout = lay, active = act
        , paneHist = hist, autoRename = auto, panes = ps }

instance Arbitrary Snapshot where
    arbitrary = do
        n <- choose (0, 4)
        Snapshot <$> vectorOf n arbitrary <*> genMaybeName
    shrink s =
        [ Snapshot ss s.lastActiveSession | ss <- shrinkList shrink s.sessions ]
        ++ [ Snapshot s.sessions la | la <- shrinkMaybeName s.lastActiveSession ]

-- The last-active session name, if any. The codec treats the empty string
-- as \"none\", so a present name is always non-empty (it round-trips as
-- 'Just'; \"\" would come back 'Nothing').
genMaybeName :: Gen (Maybe Text)
genMaybeName = oneof [pure Nothing, Just . T.cons 's' <$> genText]

shrinkMaybeName :: Maybe Text -> [Maybe Text]
shrinkMaybeName Nothing  = []
shrinkMaybeName (Just t) = Nothing : [ Just t' | t' <- shrinkText t, not (T.null t') ]

instance Arbitrary SessionSnap where
    arbitrary = do
        nm    <- genText
        cwd0  <- genText
        nwin  <- choose (1, 3)
        ixs   <- distinctAscending nwin
        ws    <- mapM genWindow ixs
        curIx <- elements (map (.ix) ws)
        winHist <- sublistOf (filter (/= curIx) (map (.ix) ws))
        pure SessionSnap
            { name = nm, startCwd = cwd0, currentIx = curIx
            , windowHist = winHist, windows = ws }
    shrink s =
        [ s { windows = ws } | ws <- shrinkList shrink s.windows, not (null ws) ]
        ++ [ s { windowHist = h } | h <- shrinkList (const []) s.windowHist ]
        ++ [ s { startCwd = c } | c <- shrinkText s.startCwd ]

instance Arbitrary WindowSnap where
    arbitrary = choose (0, 9) >>= genWindow
    -- ix and active stay fixed: they carry no constraint that shrinking
    -- them would preserve, and holding them keeps sibling ix distinct.
    shrink w =
        [ w { panes = ps } | ps <- shrinkList shrink w.panes, not (null ps) ]
        ++ [ w { paneHist = h } | h <- shrinkList (const []) w.paneHist ]
        ++ [ w { autoRename = False } | w.autoRename ]
        ++ [ w { layout = l } | l <- shrinkText w.layout ]

instance Arbitrary PaneSnap where
    arbitrary = PaneSnap <$> genText <*> genMaybeArgv <*> arbitrary
    shrink p =
        [ p { cwd = c } | c <- shrinkText p.cwd ]
        ++ [ p { command = mc } | mc <- shrinkMaybeArgv p.command ]
        ++ [ p { shellSpawned = False } | p.shellSpawned ]

-- A captured command is either absent or a non-empty argv (argv[0] is the
-- program); the empty list is not a value capture ever produces.
genMaybeArgv :: Gen (Maybe [Text])
genMaybeArgv = oneof [pure Nothing, Just <$> genArgv]

genArgv :: Gen [Text]
genArgv = choose (1, 3) >>= \n -> vectorOf n genText

shrinkMaybeArgv :: Maybe [Text] -> [Maybe [Text]]
shrinkMaybeArgv Nothing     = []
shrinkMaybeArgv (Just argv)  =
    Nothing : [ Just a | a <- shrinkList shrinkText argv, not (null a) ]

spec :: Spec
spec = do
    it "a fresh store loads an empty snapshot" $
        withStore ":memory:" loadSnapshot `shouldReturn` Snapshot { sessions = [], lastActiveSession = Nothing }

    prop "a snapshot round-trips through the store" $ \snap ->
        ioProperty $ do
            got <- withStore ":memory:" $ \conn -> do
                saveSnapshot conn snap
                loadSnapshot conn
            pure (got === snap)

    prop "re-saving overwrites rather than appends" $ \s1 s2 ->
        ioProperty $ do
            got <- withStore ":memory:" $ \conn -> do
                saveSnapshot conn s1
                saveSnapshot conn s2
                loadSnapshot conn
            pure (got === (s2 :: Snapshot))

    -- Forward/backward compatibility: the reader keys off core columns
    -- only, so it reads a store from an older or newer binary. This block is
    -- the store's version corpus (cf. the reload handover's corpus): each case
    -- pins that current code still reads a shape an older or newer binary
    -- wrote. Extend it — never relax it — when the schema evolves.
    describe "schema compatibility" $ do
        it "reads rows written before the extra column existed" $ do
            got <- withRaw $ \conn -> do
                execute_ conn "CREATE TABLE session (seq INTEGER PRIMARY KEY, \
                    \name TEXT, start_cwd TEXT, current_ix INTEGER)"
                execute_ conn "CREATE TABLE window (session_seq INTEGER, \
                    \ix INTEGER, name TEXT, layout TEXT, active INTEGER)"
                execute_ conn "CREATE TABLE pane (session_seq INTEGER, \
                    \window_ix INTEGER, ordinal INTEGER, cwd TEXT)"
                execute_ conn "INSERT INTO session VALUES (0, 'old', '/home', 0)"
                execute_ conn "INSERT INTO window VALUES (0, 0, 'w', 'lay', 0)"
                execute_ conn "INSERT INTO pane VALUES (0, 0, 0, '/home/x')"
                bootstrap conn   -- additively adds the extra columns
                loadSnapshot conn
            got `shouldBe` oneSession "old" "/home" "w" "lay" "/home/x"

        it "ignores unknown columns, tables, and meta keys" $ do
            got <- withRaw $ \conn -> do
                execute_ conn "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT)"
                execute_ conn "INSERT INTO meta VALUES ('schema_version', '999')"
                execute_ conn "INSERT INTO meta VALUES ('future_key', 'x')"
                execute_ conn "CREATE TABLE session (seq INTEGER PRIMARY KEY, \
                    \name TEXT, start_cwd TEXT, current_ix INTEGER, \
                    \extra TEXT, future_col TEXT)"
                execute_ conn "CREATE TABLE window (session_seq INTEGER, \
                    \ix INTEGER, name TEXT, layout TEXT, active INTEGER, extra TEXT)"
                execute_ conn "CREATE TABLE pane (session_seq INTEGER, \
                    \window_ix INTEGER, ordinal INTEGER, cwd TEXT, extra TEXT)"
                execute_ conn "CREATE TABLE future_table (x INTEGER)"
                execute_ conn "INSERT INTO future_table VALUES (1)"
                execute_ conn "INSERT INTO session \
                    \VALUES (0, 'nu', '/w', 0, '{}', 'surprise')"
                execute_ conn "INSERT INTO window VALUES (0, 0, 'win', 'lay', 0, '{}')"
                execute_ conn "INSERT INTO pane VALUES (0, 0, 0, '/w/p', '{}')"
                loadSnapshot conn
            got `shouldBe` oneSession "nu" "/w" "win" "lay" "/w/p"

        it "reads a legacy string command as a single-element argv" $ do
            got <- withRaw $ \conn -> do
                bootstrap conn
                execute_ conn "INSERT INTO session VALUES (0, 's', '/h', 0, '{}')"
                execute_ conn "INSERT INTO window VALUES (0, 0, 'w', 'lay', 0, '{}')"
                execute_ conn "INSERT INTO pane \
                    \VALUES (0, 0, 0, '/h', '{\"command\":\"vim\"}')"
                loadSnapshot conn
            paneCommands got `shouldBe` [Just ["vim"]]

        -- d: a pane extra with no shell_spawned key (written before the field
        -- existed) defaults to a directly-launched program.
        it "defaults a command with no shell_spawned key to directly-launched" $ do
            got <- withRaw $ \conn -> do
                bootstrap conn
                execute_ conn "INSERT INTO session VALUES (0, 's', '/h', 0, '{}')"
                execute_ conn "INSERT INTO window VALUES (0, 0, 'w', 'lay', 0, '{}')"
                execute_ conn "INSERT INTO pane \
                    \VALUES (0, 0, 0, '/h', '{\"command\":[\"vim\"]}')"
                loadSnapshot conn
            paneShellSpawned got `shouldBe` [False]

        -- d: a shell-spawned program round-trips through the extra JSON.
        it "reads a shell_spawned command as shell-spawned" $ do
            got <- withRaw $ \conn -> do
                bootstrap conn
                execute_ conn "INSERT INTO session VALUES (0, 's', '/h', 0, '{}')"
                execute_ conn "INSERT INTO window VALUES (0, 0, 'w', 'lay', 0, '{}')"
                execute_ conn "INSERT INTO pane VALUES (0, 0, 0, '/h', \
                    \'{\"command\":[\"vim\"],\"shell_spawned\":true}')"
                loadSnapshot conn
            paneShellSpawned got `shouldBe` [True]

        -- A store written before the MRU stack existed carries only the
        -- single last_ix/last_active keys; each lifts to a one-deep history.
        it "lifts a legacy single last-* key into a one-deep history" $ do
            snap <- withRaw $ \conn -> do
                bootstrap conn
                execute_ conn
                    "INSERT INTO session VALUES (0, 's', '/h', 0, '{\"last_ix\":2}')"
                execute_ conn "INSERT INTO window \
                    \VALUES (0, 0, 'w', 'lay', 0, '{\"last_active\":1}')"
                execute_ conn "INSERT INTO window VALUES (0, 2, 'w2', 'lay', 0, '{}')"
                execute_ conn "INSERT INTO pane VALUES (0, 0, 0, '/h', '{}')"
                execute_ conn "INSERT INTO pane VALUES (0, 0, 1, '/h', '{}')"
                execute_ conn "INSERT INTO pane VALUES (0, 2, 0, '/h', '{}')"
                loadSnapshot conn
            case snap.sessions of
                [s] -> do
                    s.windowHist `shouldBe` [2]
                    map (.paneHist) s.windows `shouldBe` [[1], []]
                _ -> expectationFailure "expected exactly one restored session"

        -- bb: a store written before the snapshot table existed opens with
        -- an empty, working history.
        it "reads a store written before the snapshot table existed" $ do
            (live, hist) <- withRaw $ \conn -> do
                execute_ conn "CREATE TABLE session (seq INTEGER PRIMARY KEY, \
                    \name TEXT, start_cwd TEXT, current_ix INTEGER, extra TEXT)"
                execute_ conn "CREATE TABLE window (session_seq INTEGER, \
                    \ix INTEGER, name TEXT, layout TEXT, active INTEGER, extra TEXT)"
                execute_ conn "CREATE TABLE pane (session_seq INTEGER, \
                    \window_ix INTEGER, ordinal INTEGER, cwd TEXT, extra TEXT)"
                execute_ conn "INSERT INTO session VALUES (0, 'pre', '/h', 0, '{}')"
                execute_ conn "INSERT INTO window VALUES (0, 0, 'w', 'lay', 0, '{}')"
                execute_ conn "INSERT INTO pane VALUES (0, 0, 0, '/h/x', '{}')"
                bootstrap conn   -- additively adds the snapshot table
                (,) <$> loadSnapshot conn <*> listArchived conn
            live `shouldBe` oneSession "pre" "/h" "w" "lay" "/h/x"
            hist `shouldBe` []

        -- bb: the live tables, not the history, are what a plain load
        -- reads, so a reader that predates the archive restores the newest.
        it "keeps the live tables authoritative when history rows exist" $ do
            got <- withStore ":memory:" $ \conn -> do
                saveSnapshot conn (oneSession "old" "/h" "w" "lay" "/h/o")
                archiveSnapshot conn 10 (oneSession "old" "/h" "w" "lay" "/h/o")
                saveSnapshot conn (oneSession "new" "/h" "w" "lay" "/h/n")
                loadSnapshot conn
            got `shouldBe` oneSession "new" "/h" "w" "lay" "/h/n"

        -- bb: a history row with unknown keys still decodes, defaulting
        -- the absent fields.
        it "reads a history row with unknown keys and absent fields" $ do
            got <- withRaw $ \conn -> do
                bootstrap conn
                execute_ conn "INSERT INTO snapshot (saved_at, data, extra) \
                    \VALUES ('2026-01-01T00:00:00Z', \
                    \'{\"sessions\":[{\"name\":\"s\",\"start_cwd\":\"/h\",\
                    \\"current_ix\":0,\"future_sess\":1,\"windows\":[{\"ix\":0,\
                    \\"name\":\"w\",\"layout\":\"lay\",\"active\":0,\
                    \\"future_win\":2,\"panes\":[{\"cwd\":\"/h/x\",\
                    \\"future_pane\":3}]}]}],\"future\":4}', '{\"future\":5}')"
                listArchived conn
            map (.savedAt) got `shouldBe` ["2026-01-01T00:00:00Z"]
            map (.snapshot) got `shouldBe` [oneSession "s" "/h" "w" "lay" "/h/x"]

    -- bb: pruned snapshot history, so a bad overwrite can be rolled back.
    describe "snapshot history" $ do
        let snapOf nm = oneSession nm "/h" "w" "lay" ("/h/" <> nm)

        prop "a non-empty snapshot round-trips through the archive" $ \snap ->
            not (null (snap :: Snapshot).sessions) ==> ioProperty $ do
                got <- withStore ":memory:" $ \conn -> do
                    saveSnapshot conn snap
                    archiveSnapshot conn 10 snap
                    listArchived conn
                pure (map (.snapshot) got === [snap])

        it "archiving lists newest first and prunes to the limit" $ do
            got <- withStore ":memory:" $ \conn -> do
                forM_ ["a", "b", "c", "d"] $ \nm -> do
                    saveSnapshot conn (snapOf nm)
                    archiveSnapshot conn 3 (snapOf nm)
                listArchived conn
            map (.gen) got `shouldBe` [4, 3, 2]
            map (.snapshot) got `shouldBe` map snapOf ["d", "c", "b"]

        it "re-archiving an unchanged tree adds no generation" $ do
            got <- withStore ":memory:" $ \conn -> do
                saveSnapshot conn (snapOf "a")
                archiveSnapshot conn 10 (snapOf "a")
                archiveSnapshot conn 10 (snapOf "a")
                listArchived conn
            map (.gen) got `shouldBe` [1]

        it "an empty tree is never archived" $ do
            got <- withStore ":memory:" $ \conn -> do
                archiveSnapshot conn 10
                    (Snapshot { sessions = [], lastActiveSession = Nothing })
                listArchived conn
            got `shouldBe` []

        it "a limit of zero disables history" $ do
            got <- withStore ":memory:" $ \conn -> do
                saveSnapshot conn (snapOf "a")
                archiveSnapshot conn 0 (snapOf "a")
                listArchived conn
            got `shouldBe` []

        it "loads a generation by id, or Nothing for an unknown one" $ do
            (a, missing) <- withStore ":memory:" $ \conn -> do
                forM_ ["a", "b"] $ \nm -> do
                    saveSnapshot conn (snapOf nm)
                    archiveSnapshot conn 10 (snapOf nm)
                (,) <$> loadArchived conn 1 <*> loadArchived conn 99
            a `shouldBe` Just (snapOf "a")
            missing `shouldBe` Nothing

        it "clearing the live tree keeps the history" $ do
            (live, hist) <- withStore ":memory:" $ \conn -> do
                saveSnapshot conn (snapOf "a")
                archiveSnapshot conn 10 (snapOf "a")
                clearLive conn
                (,) <$> loadSnapshot conn <*> listArchived conn
            live `shouldBe` Snapshot { sessions = [], lastActiveSession = Nothing }
            map (.snapshot) hist `shouldBe` [snapOf "a"]

    -- b7: restore must preserve the last-active window and pane, not just
    -- the current ones. These fields used to be dropped by the codec.
    it "round-trips the last-active window and pane" $ do
        let snap = Snapshot
                { lastActiveSession = Nothing, sessions =
                    [ SessionSnap { name = "s", startCwd = "/h", currentIx = 2
                        , windowHist = [0]
                        , windows =
                            [ WindowSnap { ix = 0, name = "a", layout = "l0"
                                , active = 0, paneHist = []
                                , autoRename = False
                                , panes = [PaneSnap { cwd = "/h", command = Nothing, shellSpawned = False }] }
                            , WindowSnap { ix = 2, name = "b", layout = "l2"
                                , active = 1, paneHist = [0]
                                , autoRename = False
                                , panes =
                                    [ PaneSnap { cwd = "/h", command = Nothing, shellSpawned = False }
                                    , PaneSnap { cwd = "/h", command = Nothing, shellSpawned = False } ] }
                            ] } ] }
        got <- withStore ":memory:" $ \conn ->
            saveSnapshot conn snap >> loadSnapshot conn
        got `shouldBe` snap

    -- b7: restore must preserve each window's automatic-rename status, so an
    -- auto-renaming window keeps tracking its active pane and a manually-named
    -- window keeps its pinned name. The codec used to drop this flag.
    it "round-trips each window's automatic-rename status" $ do
        let win wix auto = WindowSnap { ix = wix, name = "w", layout = "l"
                , active = 0, paneHist = [], autoRename = auto
                , panes = [PaneSnap { cwd = "/h", command = Nothing, shellSpawned = False }] }
            snap = Snapshot
                { lastActiveSession = Nothing, sessions =
                    [ SessionSnap { name = "s", startCwd = "/h", currentIx = 0
                        , windowHist = []
                        , windows = [win 0 True, win 1 False] } ] }
        got <- withStore ":memory:" $ \conn ->
            saveSnapshot conn snap >> loadSnapshot conn
        [ w.autoRename | s <- got.sessions, w <- s.windows ]
            `shouldBe` [True, False]

    it "round-trips an argv whose argument contains spaces" $ do
        let snap = Snapshot
                { lastActiveSession = Nothing, sessions =
                    [ SessionSnap { name = "s", startCwd = "/h", currentIx = 0
                        , windowHist = []
                        , windows =
                            [ WindowSnap { ix = 0, name = "w", layout = "l"
                                , active = 0, paneHist = []
                                , autoRename = False
                                , panes = [ PaneSnap { cwd = "/h"
                                    , command = Just ["vim", "Foo Bar.txt"]
                                    , shellSpawned = False } ] } ] } ] }
        got <- withStore ":memory:" $ \conn ->
            saveSnapshot conn snap >> loadSnapshot conn
        got `shouldBe` snap

    -- d: whether a program was shell-spawned survives a save/load, so a
    -- restore knows to relaunch it through the pane's shell.
    it "round-trips a pane's shell-spawned flag" $ do
        let pane sp = PaneSnap { cwd = "/h", command = Just ["vim"], shellSpawned = sp }
            snap = Snapshot
                { lastActiveSession = Nothing, sessions =
                    [ SessionSnap { name = "s", startCwd = "/h", currentIx = 0
                        , windowHist = []
                        , windows =
                            [ WindowSnap { ix = 0, name = "w", layout = "l"
                                , active = 0, paneHist = []
                                , autoRename = False
                                , panes = [pane True, pane False] } ] } ] }
        got <- withStore ":memory:" $ \conn ->
            saveSnapshot conn snap >> loadSnapshot conn
        paneShellSpawned got `shouldBe` [True, False]

-- A one-session, one-window, one-pane snapshot with every optional field
-- at its default.
oneSession :: Text -> Text -> Text -> Text -> Text -> Snapshot
oneSession nm cwd0 wnm lay pcwd = Snapshot
    { lastActiveSession = Nothing, sessions =
        [ SessionSnap
            { name = nm, startCwd = cwd0, currentIx = 0
            , windowHist = []
            , windows =
                [ WindowSnap { ix = 0, name = wnm, layout = lay
                    , active = 0, paneHist = []
                    , autoRename = False
                    , panes = [PaneSnap { cwd = pcwd, command = Nothing, shellSpawned = False }] }
                ] } ] }

-- Every pane's captured command, in load order.
paneCommands :: Snapshot -> [Maybe [Text]]
paneCommands snap =
    [ p.command | s <- snap.sessions, w <- s.windows, p <- w.panes ]

-- Every pane's shell-spawned flag, in load order.
paneShellSpawned :: Snapshot -> [Bool]
paneShellSpawned snap =
    [ p.shellSpawned | s <- snap.sessions, w <- s.windows, p <- w.panes ]

-- Run an action against a fresh in-memory database.
withRaw :: (Connection -> IO a) -> IO a
withRaw act = do
    conn <- open ":memory:"
    act conn `finally` close conn
