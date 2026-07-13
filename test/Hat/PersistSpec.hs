module Hat.PersistSpec (spec) where

import Control.Exception (finally)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, close, execute_, open)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Persist

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
    pure WindowSnap { ix = wix, name = nm, layout = lay, active = act, panes = ps }

instance Arbitrary Snapshot where
    arbitrary = do
        n <- choose (0, 4)
        Snapshot <$> vectorOf n arbitrary
    shrink s = Snapshot <$> shrinkList shrink s.sessions

instance Arbitrary SessionSnap where
    arbitrary = do
        nm    <- genText
        cwd0  <- genText
        nwin  <- choose (1, 3)
        ixs   <- distinctAscending nwin
        ws    <- mapM genWindow ixs
        curIx <- elements (map (.ix) ws)
        pure SessionSnap
            { name = nm, startCwd = cwd0, currentIx = curIx, windows = ws }
    shrink s =
        [ s { windows = ws } | ws <- shrinkList shrink s.windows, not (null ws) ]
        ++ [ s { startCwd = c } | c <- shrinkText s.startCwd ]

instance Arbitrary WindowSnap where
    arbitrary = choose (0, 9) >>= genWindow
    -- ix and active stay fixed: they carry no constraint that shrinking
    -- them would preserve, and holding them keeps sibling ix distinct.
    shrink w =
        [ w { panes = ps } | ps <- shrinkList shrink w.panes, not (null ps) ]
        ++ [ w { layout = l } | l <- shrinkText w.layout ]

instance Arbitrary PaneSnap where
    arbitrary = PaneSnap <$> genText
    shrink p = PaneSnap <$> shrinkText p.cwd

spec :: Spec
spec = do
    it "a fresh store loads an empty snapshot" $
        withStore ":memory:" loadSnapshot `shouldReturn` Snapshot { sessions = [] }

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
    -- only, so it reads a store from an older or newer binary.
    describe "schema compatibility" $ do
        let oneSession nm cwd0 wnm lay pcwd = Snapshot
                { sessions =
                    [ SessionSnap
                        { name = nm, startCwd = cwd0, currentIx = 0
                        , windows =
                            [ WindowSnap { ix = 0, name = wnm, layout = lay
                                , active = 0, panes = [PaneSnap { cwd = pcwd }] }
                            ] } ] }

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

-- Run an action against a fresh in-memory database.
withRaw :: (Connection -> IO a) -> IO a
withRaw act = do
    conn <- open ":memory:"
    act conn `finally` close conn
