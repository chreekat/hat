{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}  -- Arbitrary instances for the reload types live with their test; the library must not depend on QuickCheck

module Hat.Server.ReloadSpec (spec) where

import Codec.Serialise (encode, serialise)
import Codec.Serialise.Encoding (encodeListLen, encodeWord)
import Codec.CBOR.Write (toStrictByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Model.PaneKind (PaneKind (..))
import Hat.Server.Persist
import Hat.Server.PersistSpec ()  -- the tree generators, shared with the store
import Hat.Server.Reload
import Hat.Term.Cell
import Hat.Term.Emulator (carriedDecModes)
import Hat.Term.HostProtocol (CursorModel (..), CursorStyle (..), defaultCursorModel)

genText :: Gen T.Text
genText = T.pack <$> listOf (chooseEnum (' ', '~'))

genMaybeText :: Gen (Maybe T.Text)
genMaybeText = oneof [pure Nothing, Just <$> genText]

instance Arbitrary ReloadCleanup where
    arbitrary = ReloadCleanup <$> arbitrary <*> shortList
    shrink c =
        [ ReloadCleanup fd lv | (fd, lv) <- shrink (c.listenFd, c.live) ]

-- A real cleanup core holds one entry per pane, so keep generated ones small.
shortList :: (Arbitrary a) => Gen [a]
shortList = sized $ \n -> do
    k <- choose (0, min 3 n)
    vectorOf k arbitrary

-- The tree is generated as the store's own 'Snapshot' — the exact shape the
-- handover carries — with a 'HotPane' hung off every captured pane.
instance Arbitrary ReloadTree where
    arbitrary = do
        snap <- arbitrary :: Gen Snapshot
        ss   <- mapM hotSessionOf snap.sessions
        lst  <- genMaybeText
        pure ReloadTree
            { sessions = ss
            , currentSession = snap.lastActiveSession
            , lastSession = lst }
    shrink t =
        [ ReloadTree ss t.currentSession t.lastSession
        | ss <- shrinkList shrink t.sessions ]
        ++ [ ReloadTree t.sessions Nothing t.lastSession
           | isJust t.currentSession ]
        ++ [ ReloadTree t.sessions t.currentSession Nothing
           | isJust t.lastSession ]

instance Arbitrary HotSession where
    arbitrary = hotSessionOf =<< arbitrary
    shrink s =
        [ HotSession s.name s.startCwd s.currentIx s.windowHist
            s.createdAt s.restores s.group ws
        | ws <- shrinkList shrink s.windows, not (null ws) ]
        ++ [ HotSession s.name s.startCwd s.currentIx h
                s.createdAt s.restores s.group s.windows
           | h <- shrinkList (const []) s.windowHist ]
        ++ [ HotSession s.name s.startCwd s.currentIx s.windowHist
                Nothing s.restores s.group s.windows
           | isJust s.createdAt ]
        ++ [ HotSession s.name s.startCwd s.currentIx s.windowHist
                s.createdAt 0 s.group s.windows
           | s.restores /= 0 ]

hotSessionOf :: SessionSnap -> Gen HotSession
hotSessionOf s = do
    ws <- mapM hotWindowOf s.windows
    pure HotSession
        { name = s.name, startCwd = s.startCwd, currentIx = s.currentIx
        , windowHist = s.windowHist, createdAt = s.createdAt
        , restores = s.restores, group = s.group, windows = ws }

instance Arbitrary HotWindow where
    arbitrary = hotWindowOf =<< arbitrary
    shrink w =
        [ hw w.paneHist w.autoRename w.key w.linkOf w.modal ps
        | ps <- shrinkList shrinkHot w.panes, not (null ps) ]
        ++ [ hw h w.autoRename w.key w.linkOf w.modal w.panes
           | h <- shrinkList (const []) w.paneHist ]
        ++ [ hw w.paneHist False w.key w.linkOf w.modal w.panes | w.autoRename ]
        ++ [ hw w.paneHist w.autoRename w.key w.linkOf Nothing w.panes | isJust w.modal ]
      where
        hw = HotWindow w.ix w.name w.layout w.active
        shrinkHot (p, hp) =
            [ (p', hp) | p' <- shrink p ] ++ [ (p, hp') | hp' <- shrink hp ]

hotWindowOf :: WindowSnap -> Gen HotWindow
hotWindowOf w = do
    ps <- mapM (\p -> (,) p <$> arbitrary) w.panes
    pure HotWindow
        { ix = w.ix, name = w.name, layout = w.layout, active = w.active
        , paneHist = w.paneHist, autoRename = w.autoRename
        , key = w.key, linkOf = w.linkOf, modal = w.modal, panes = ps }

instance Arbitrary HotPane where
    arbitrary = HotPane <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
    shrink p =
        [ HotPane fd cpid ms sc start death
        | (fd, cpid, ms, sc, (start, death)) <-
            shrink (p.masterFd, p.childPid, p.modes, p.screen
                   , (p.start, p.death)) ]

instance Arbitrary ReloadStart where
    arbitrary = oneof
        [ pure StartShell
        , StartCommand <$> genText
        , StartArgv <$> listOf genText ]
    shrink = \case
        StartShell -> []
        StartCommand t -> StartShell : (StartCommand . T.pack <$> shrink (T.unpack t))
        StartArgv argv -> StartShell
            : (StartArgv . map T.pack <$> shrink (map T.unpack argv))

instance Arbitrary ReloadDeath where
    arbitrary = ReloadDeath <$> arbitrary <*> arbitrary <*> arbitrary
    shrink d =
        [ ReloadDeath st sg at
        | (st, sg, at) <- shrink (d.status, d.signal, d.diedAt) ]

instance Arbitrary ReloadModes where
    arbitrary = ReloadModes
        <$> arbitrary <*> arbitrary <*> choose (0, 3)
        <*> arbitrary <*> choose (0, 31) <*> choose (0, 2)
        <*> sublistOf carriedDecModes
    shrink m =
        [ ReloadModes cr fr mo mok kf ek ds
        | (cr, fr, mo, mok, kf, ek, ds) <-
            shrink (m.colorReport, m.focusReport, m.mouse
                   , m.modifyOtherKeys, m.kittyFlags, m.extendedKeys, m.decModes) ]

instance Arbitrary ReloadScreen where
    arbitrary = ReloadScreen
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> choose (0, 5) <*> shortLines <*> shortLines <*> arbitrary
        <*> arbitrary
    shrink s =
        [ ReloadScreen alt cr cc cv co rs sb pen cur
        | (alt, cr, cc, (cv, co, rs, sb, pen), cur) <- shrink
            ( s.altScreen, s.cursorRow, s.cursorCol
            , (s.cursorVisible, s.cols, s.rows, s.scrollback, s.pen), s.cursor ) ]

instance Arbitrary CursorModel where
    arbitrary = CursorModel
        <$> elements [StyleDefault, StyleBlock, StyleUnderline, StyleBar]
        <*> arbitrary <*> arbitrary
    shrink m =
        [ CursorModel StyleDefault m.blinking m.blinkingSet | m.style /= StyleDefault ]
        <> [ CursorModel m.style False m.blinkingSet | m.blinking ]
        <> [ CursorModel m.style m.blinking False | m.blinkingSet ]

-- Short painted lines keep the round-trip cheap; the bytes only need to
-- survive the CBOR codec, not mean anything to the emulator.
shortLines :: Gen [B.ByteString]
shortLines = do
    r <- choose (0, 2)
    vectorOf r (B.pack <$> (choose (0, 4) >>= (`vectorOf` arbitrary)))

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary
    shrink = map B.pack . shrink . B.unpack

instance Arbitrary Style where
    arbitrary = Style
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary Color where
    arbitrary = oneof
        [ pure DefaultColor
        , Indexed <$> arbitrary
        , RGB <$> arbitrary <*> arbitrary <*> arbitrary ]
    shrink DefaultColor = []
    shrink (Indexed n)  = DefaultColor : (Indexed <$> shrink n)
    shrink (RGB r g b)  =
        DefaultColor : [ RGB r' g' b' | (r', g', b') <- shrink (r, g, b) ]

-- The payload a capture writes for a given tree: the tree as the store's
-- snapshot JSON, its panes' hot state flattened in tree order. Mirrors
-- 'captureReload', whose two halves come from one walk.
hotOf :: ReloadTree -> ReloadHot
hotOf t = ReloadHot
    { tree = encodeSnapshotJson Snapshot
        { sessions = map sessionOf t.sessions
        , lastActiveSession = t.currentSession }
    , hot = [ hp | s <- t.sessions, w <- s.windows, (_, hp) <- w.panes ]
    , lastSession = t.lastSession }
  where
    sessionOf s = SessionSnap
        { name = s.name, startCwd = s.startCwd, currentIx = s.currentIx
        , windowHist = s.windowHist, createdAt = s.createdAt
        , restores = s.restores, group = s.group
        , windows = map windowOf s.windows }
    windowOf w = WindowSnap
        { ix = w.ix, name = w.name, layout = w.layout, active = w.active
        , paneHist = w.paneHist, autoRename = w.autoRename
        , key = w.key, linkOf = w.linkOf, modal = w.modal, panes = map fst w.panes }

-- Re-encode a handover at an arbitrary era, to exercise the era gate. Mirrors
-- 'encodeHandover' exactly except for the era field; the golden-byte test
-- below pins the real encoder, so a format change surfaces there.
encodeAtEra :: Int -> ReloadCleanup -> ReloadHot -> B.ByteString
encodeAtEra era c h = toStrictByteString $
       encodeListLen 5
    <> encodeWord 0x48415452
    <> encode era
    <> encode c.listenFd
    <> encode c.live
    <> encode (BL.toStrict (serialise h))

hexOf :: B.ByteString -> String
hexOf = concatMap byte . B.unpack
  where
    byte w = [d (w `div` 16), d (w `mod` 16)]
    d n = "0123456789abcdef" !! fromIntegral (n :: Word8)

unHex :: String -> B.ByteString
unHex = B.pack . go
  where
    go :: String -> [Word8]
    go (h : l : rest) = fromIntegral (v h * 16 + v l) : go rest
    go _              = []
    v :: Char -> Int
    v c = maybe 0 id (lookup c (zip "0123456789abcdef" [0 ..]))

fixedCleanup :: ReloadCleanup
fixedCleanup = ReloadCleanup { listenFd = 3, live = [(7, 100)] }

-- A one-pane tree carrying the given mode subscriptions and screen on its pane
-- and the given alternate session.
treeWith :: Maybe T.Text -> ReloadModes -> ReloadScreen -> ReloadTree
treeWith mlast ms sc = ReloadTree
    { sessions =
        [ HotSession
            { name = "work", startCwd = "/home", currentIx = 0, windowHist = []
            , createdAt = Nothing, restores = 0
            , group = Nothing
            , windows =
                [ HotWindow
                    { ix = 0, name = "w", layout = "L", active = 0
                    , paneHist = [], autoRename = True
                    , key = Nothing, linkOf = Nothing, modal = Nothing
                    , panes =
                        [ ( PaneSnap
                              { cwd = "/tmp", command = Nothing
                              , shellSpawned = False, kind = ProgramPane, inputOff = False, title = Nothing }
                          , HotPane
                              { masterFd = 7, childPid = 100
                              , modes = ms, screen = sc
                              , start = StartShell, death = Nothing } ) ] } ] } ]
    , currentSession = Just "work"
    , lastSession = mlast }

-- The current-era representative: 'fixedTree' with a launch command on its
-- pane plus a second, dead pane, to pin era 11's appended fields.
currentTree :: ReloadTree
currentTree = ReloadTree
    { sessions =
        [ HotSession
            { name = "work", startCwd = "/home", currentIx = 0, windowHist = []
            , createdAt = Nothing, restores = 0, group = Nothing
            , windows =
                [ HotWindow
                    { ix = 0, name = "w", layout = "L", active = 0
                    , paneHist = [], autoRename = True
                    , key = Nothing, linkOf = Nothing, modal = Nothing
                    , panes =
                        [ ( snap
                          , HotPane
                              { masterFd = 7, childPid = 100
                              , modes = fixedModes, screen = fixedScreen
                              , start = StartCommand "make -j2"
                              , death = Nothing } )
                        , ( snap
                          , HotPane
                              { masterFd = -1, childPid = 101
                              , modes = fixedModes, screen = fixedScreen
                              , start = StartArgv ["vim", "a b"]
                              , death = Just ReloadDeath
                                  { status = Just 3, signal = Nothing
                                  , diedAt = 1700000000 } } ) ] } ] } ]
    , currentSession = Just "work"
    , lastSession = Just "prev" }
  where
    snap = PaneSnap { cwd = "/tmp", command = Nothing, shellSpawned = False, kind = ProgramPane, inputOff = False, title = Nothing }

-- The current-era representative: 'currentTree' with a third, empty pane —
-- no fd or child, a stored launch command, its input off and a title — to
-- pin era 12.
emptyPaneTree :: ReloadTree
emptyPaneTree = ReloadTree
    { sessions =
        [ HotSession
            { name = "work", startCwd = "/home", currentIx = 0, windowHist = []
            , createdAt = Nothing, restores = 0, group = Nothing
            , windows =
                [ HotWindow
                    { ix = 0, name = "w", layout = "L", active = 0
                    , paneHist = [], autoRename = True
                    , key = Nothing, linkOf = Nothing, modal = Nothing
                    , panes = currentPanes <> [(emptySnap, emptyHot)] } ] } ]
    , currentSession = Just "work"
    , lastSession = Just "prev" }
  where
    currentPanes = concat
        [ w.panes | se <- currentTree.sessions, w <- se.windows ]
    emptySnap = (plainPane "/srv")
        { kind = EmptyPane, inputOff = True, title = Just "idle" }
    emptyHot = HotPane
        { masterFd = -1, childPid = 0
        , modes = fixedModes, screen = fixedScreen
        , start = StartCommand "pwd", death = Nothing }

-- The era-13 representative: 'emptyPaneTree' with its first pane in
-- extended-key mode 1, which only era 13 can carry: the era-13 row.
extKeysTree :: ReloadTree
extKeysTree = case emptyPaneTree of
    ReloadTree {..} -> ReloadTree { sessions = map session sessions, .. }
  where
    session HotSession {..} = HotSession { windows = map window windows, .. }
    window HotWindow {..} = HotWindow { panes = mode1 panes, .. }
    mode1 = \case
        (snap, HotPane {..}) : rest ->
            (snap, HotPane { modes = fixedModes { modifyOtherKeys = False, extendedKeys = 1 }, .. })
                : rest
        [] -> []

-- The era-14 representative: 'extKeysTree' with its first pane in SGR
-- mouse and bracketed paste modes, which only era 14 can carry.
decModesTree :: ReloadTree
decModesTree = case extKeysTree of
    ReloadTree {..} -> ReloadTree { sessions = map session sessions, .. }
  where
    session HotSession {..} = HotSession { windows = map window windows, .. }
    window HotWindow {..} = HotWindow { panes = withModes panes, .. }
    withModes = \case
        (snap, HotPane {..}) : rest ->
            (snap, HotPane { modes = modes { decModes = [1006, 2004] }, .. }) : rest
        [] -> []

-- The current-era representative: 'decModesTree' with its first pane's
-- program showing a steady bar cursor it has said blinks no more, which only
-- era 15 can carry.
cursorTree :: ReloadTree
cursorTree = case decModesTree of
    ReloadTree {..} -> ReloadTree { sessions = map session sessions, .. }
  where
    session HotSession {..} = HotSession { windows = map window windows, .. }
    window HotWindow {..} = HotWindow { panes = withCursor panes, .. }
    withCursor = \case
        (snap, HotPane {..}) : rest ->
            (snap, HotPane { screen = screen { cursor = bar }, .. }) : rest
        [] -> []
    bar = CursorModel { style = StyleBar, blinking = False, blinkingSet = True }

-- A non-trivial captured screen, to pin the current era's encoding: an
-- alt-screen pane with one styled painted row and one blank scrollback line.
-- Old corpus rows carry the same screen as cells (one red @x@, one blank
-- line), so this is also what their migration must paint them into.
fixedScreen :: ReloadScreen
fixedScreen = ReloadScreen
    { altScreen = True, cursorRow = 1, cursorCol = 2, cursorVisible = True
    , cols = 1
    , rows = ["\ESC[0;31mx"]
    , scrollback = [""], pen = defaultStyle, cursor = defaultCursorModel }

-- The current-era representative, with a non-trivial mode set, screen, and
-- alternate session to pin its encoding.
fixedTree :: ReloadTree
fixedTree = treeWith (Just "prev") fixedModes fixedScreen

fixedModes :: ReloadModes
fixedModes = ReloadModes
    { colorReport = True, focusReport = False, mouse = 2
    , modifyOtherKeys = True, kittyFlags = 3, extendedKeys = 2, decModes = [] }

-- What an era 4-8 row migrates to: those payloads predate the key protocols,
-- so their panes come back with none enabled.
preKeyProtocolTree :: ReloadTree
preKeyProtocolTree = treeWith (Just "prev")
    fixedModes { modifyOtherKeys = False, kittyFlags = 0, extendedKeys = 0 } fixedScreen

-- The reload corpus: one committed encoding per era. A build MUST decode every
-- vector here into the current tree (armor-style backward-compat enforcement).
-- When 'reloadEra' is bumped, DO NOT edit an existing row's bytes — append a new
-- one. A row's target tree is what its bytes migrate to in THIS build, so it
-- tracks the current shape: the pre-screen era-1 and era-2 rows migrate to a
-- blank ('emptyReloadScreen') pane, and every pre-era-8 row to a pane with no
-- captured command.
corpus :: [(Int, String, ReloadCleanup, ReloadTree)]
corpus =
    [ ( 1
      , "851a4841545201039f82071864ff583183009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8400642f746d70\
        \071864ffffff8164776f726b"
      , fixedCleanup
      , treeWith Nothing (ReloadModes False False 0 False 0 0 []) emptyReloadScreen )
    , ( 2
      , "851a4841545202039f82071864ff583683009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8500642f746d70\
        \0718648400f5f402ffffff8164776f726b"
      , fixedCleanup
      , treeWith Nothing (ReloadModes True False 2 False 0 0 []) emptyReloadScreen )
    , ( 3
      , "851a4841545203039f82071864ff586783009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8600642f746d70\
        \0718648400f5f4028700f50102f59f9f840061780189008201018100\
        \f4f4f4f4f4f4ffff9f9f8400612001890081008100f4f4f4f4f4f4ff\
        \ffffffff8164776f726b"
      , fixedCleanup
      , treeWith Nothing (ReloadModes True False 2 False 0 0 []) fixedScreen )
    , ( 4
      , "851a4841545204039f82071864ff586d84009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8600642f746d70\
        \0718648400f5f4028700f50102f59f9f840061780189008201018100\
        \f4f4f4f4f4f4ffff9f9f8400612001890081008100f4f4f4f4f4f4ff\
        \ffffffff8164776f726b816470726576"
      , fixedCleanup, preKeyProtocolTree )
    , ( 5
      , "851a4841545205039f82071864ff586f84009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8600642f746d70\
        \0718648400f5f4028700f50102f59f9f84006178018a0082010181\
        \00f4f4f4f4f4f4f4ffff9f9f84006120018a0081008100f4f4f4f4\
        \f4f4f4ffffffffff8164776f726b816470726576"
      , fixedCleanup, preKeyProtocolTree )
    , ( 6
      , "851a4841545206039f82071864ff587c84009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8600642f746d70\
        \0718648400f5f4028800f50102f59f9f84006178018a0082010181\
        \00f4f4f4f4f4f4f4ffff9f9f84006120018a0081008100f4f4f4f4\
        \f4f4f4ffff8a0081008100f4f4f4f4f4f4f4ffffff8164776f726b\
        \816470726576"
      , fixedCleanup, preKeyProtocolTree )
    , ( 7
      , "851a4841545207039f82071864ff587c84009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8600642f746d70\
        \0718648400f5f4028800f50102f59f9f84006178018a0082010181\
        \00f4f4f4f4f4f4f4ffff9f9f84006120018a0081008100f4f4f4f4\
        \f4f4f4ffff8a0081008100f4f4f4f4f4f4f4ffffff8164776f726b\
        \816470726576"
      , fixedCleanup, preKeyProtocolTree )
    , ( 8
      , "851a4841545208039f82071864ff590118840078c27b226c6173\
        \745f6163746976655f73657373696f6e223a22776f726b222c22\
        \73657373696f6e73223a5b7b2263757272656e745f6978223a30\
        \2c226e616d65223a22776f726b222c2273746172745f63776422\
        \3a222f686f6d65222c2277696e646f7773223a5b7b2261637469\
        \7665223a302c226175746f5f72656e616d65223a747275652c22\
        \6978223a302c226c61796f7574223a224c222c226e616d65223a\
        \2277222c2270616e6573223a5b7b22637764223a222f746d7022\
        \7d5d7d5d7d5d7d9f85000718648400f5f4028800f50102f59f9f\
        \84006178018a008201018100f4f4f4f4f4f4f4ffff9f9f840061\
        \20018a0081008100f4f4f4f4f4f4f4ffff8a0081008100f4f4f4\
        \f4f4f4f4ff816470726576"
      , fixedCleanup, preKeyProtocolTree )
    , ( 9
      , "851a4841545209039f82071864ff59011a840078c27b226c6173\
        \745f6163746976655f73657373696f6e223a22776f726b222c22\
        \73657373696f6e73223a5b7b2263757272656e745f6978223a30\
        \2c226e616d65223a22776f726b222c2273746172745f63776422\
        \3a222f686f6d65222c2277696e646f7773223a5b7b2261637469\
        \7665223a302c226175746f5f72656e616d65223a747275652c22\
        \6978223a302c226c61796f7574223a224c222c226e616d65223a\
        \2277222c2270616e6573223a5b7b22637764223a222f746d7022\
        \7d5d7d5d7d5d7d9f85000718648600f5f402f5038800f50102f5\
        \9f9f84006178018a008201018100f4f4f4f4f4f4f4ffff9f9f84\
        \006120018a0081008100f4f4f4f4f4f4f4ffff8a0081008100f4\
        \f4f4f4f4f4f4ff816470726576"
      , fixedCleanup, fixedTree )
    , ( 10
      , "851a484154520a039f82071864ff58fc840078c27b226c6173\
        \745f6163746976655f73657373696f6e223a22776f726b222c22\
        \73657373696f6e73223a5b7b2263757272656e745f6978223a30\
        \2c226e616d65223a22776f726b222c2273746172745f63776422\
        \3a222f686f6d65222c2277696e646f7773223a5b7b2261637469\
        \7665223a302c226175746f5f72656e616d65223a747275652c22\
        \6978223a302c226c61796f7574223a224c222c226e616d65223a\
        \2277222c2270616e6573223a5b7b22637764223a222f746d7022\
        \7d5d7d5d7d5d7d9f85000718648600f5f402f5038900f50102f5\
        \019f481b5b303b33316d78ff9f40ff8a0081008100f4f4f4f4f4\
        \f4f4ff816470726576"
      , fixedCleanup, fixedTree )
    , ( 11
      , "851a484154520b039f82071864ff59015c840078d17b226c61\
        \73745f6163746976655f73657373696f6e223a22776f726b222c\
        \2273657373696f6e73223a5b7b2263757272656e745f6978223a\
        \302c226e616d65223a22776f726b222c2273746172745f637764\
        \223a222f686f6d65222c2277696e646f7773223a5b7b22616374\
        \697665223a302c226175746f5f72656e616d65223a747275652c\
        \226978223a302c226c61796f7574223a224c222c226e616d6522\
        \3a2277222c2270616e6573223a5b7b22637764223a222f746d70\
        \227d2c7b22637764223a222f746d70227d5d7d5d7d5d7d9f8700\
        \0718648600f5f402f5038900f50102f5019f481b5b303b33316d\
        \78ff9f40ff8a0081008100f4f4f4f4f4f4f48201686d616b6520\
        \2d6a328087002018658600f5f402f5038900f50102f5019f481b\
        \5b303b33316d78ff9f40ff8a0081008100f4f4f4f4f4f4f48202\
        \9f6376696d63612062ff8184008103801a6553f100ff81647072\
        \6576"
      , fixedCleanup, currentTree )
    , ( 12
      , "851a484154520c039f82071864ff5901cd840079010d7b226c61\
        \73745f6163746976655f73657373696f6e223a22776f726b222c\
        \2273657373696f6e73223a5b7b2263757272656e745f6978223a\
        \302c226e616d65223a22776f726b222c2273746172745f637764\
        \223a222f686f6d65222c2277696e646f7773223a5b7b22616374\
        \697665223a302c226175746f5f72656e616d65223a747275652c\
        \226978223a302c226c61796f7574223a224c222c226e616d6522\
        \3a2277222c2270616e6573223a5b7b22637764223a222f746d70\
        \227d2c7b22637764223a222f746d70227d2c7b22637764223a22\
        \2f737276222c22656d707479223a747275652c22696e7075745f\
        \6f6666223a747275652c227469746c65223a2269646c65227d5d\
        \7d5d7d5d7d9f87000718648600f5f402f5038900f50102f5019f\
        \481b5b303b33316d78ff9f40ff8a0081008100f4f4f4f4f4f4f4\
        \8201686d616b65202d6a328087002018658600f5f402f5038900\
        \f50102f5019f481b5b303b33316d78ff9f40ff8a0081008100f4\
        \f4f4f4f4f4f482029f6376696d63612062ff8184008103801a65\
        \53f100870020008600f5f402f5038900f50102f5019f481b5b30\
        \3b33316d78ff9f40ff8a0081008100f4f4f4f4f4f4f482016370\
        \776480ff816470726576"
      , fixedCleanup, emptyPaneTree )
    , ( 13
      , "851a484154520d039f82071864ff5901d0840079010d7b226c6173\
        \745f6163746976655f73657373696f6e223a22776f726b222c2273\
        \657373696f6e73223a5b7b2263757272656e745f6978223a302c22\
        \6e616d65223a22776f726b222c2273746172745f637764223a222f\
        \686f6d65222c2277696e646f7773223a5b7b22616374697665223a\
        \302c226175746f5f72656e616d65223a747275652c226978223a30\
        \2c226c61796f7574223a224c222c226e616d65223a2277222c2270\
        \616e6573223a5b7b22637764223a222f746d70227d2c7b22637764\
        \223a222f746d70227d2c7b22637764223a222f737276222c22656d\
        \707479223a747275652c22696e7075745f6f6666223a747275652c\
        \227469746c65223a2269646c65227d5d7d5d7d5d7d9f8700071864\
        \8700f5f402f403018900f50102f5019f481b5b303b33316d78ff9f\
        \40ff8a0081008100f4f4f4f4f4f4f48201686d616b65202d6a3280\
        \87002018658700f5f402f503028900f50102f5019f481b5b303b33\
        \316d78ff9f40ff8a0081008100f4f4f4f4f4f4f482029f6376696d\
        \63612062ff8184008103801a6553f100870020008700f5f402f503\
        \028900f50102f5019f481b5b303b33316d78ff9f40ff8a00810081\
        \00f4f4f4f4f4f4f482016370776480ff816470726576"
      , fixedCleanup, extKeysTree )
    , ( 14
      , "851a484154520e039f82071864ff5901da840079010d7b226c6173\
        \745f6163746976655f73657373696f6e223a22776f726b222c2273\
        \657373696f6e73223a5b7b2263757272656e745f6978223a302c22\
        \6e616d65223a22776f726b222c2273746172745f637764223a222f\
        \686f6d65222c2277696e646f7773223a5b7b22616374697665223a\
        \302c226175746f5f72656e616d65223a747275652c226978223a30\
        \2c226c61796f7574223a224c222c226e616d65223a2277222c2270\
        \616e6573223a5b7b22637764223a222f746d70227d2c7b22637764\
        \223a222f746d70227d2c7b22637764223a222f737276222c22656d\
        \707479223a747275652c22696e7075745f6f6666223a747275652c\
        \227469746c65223a2269646c65227d5d7d5d7d5d7d9f8700071864\
        \8800f5f402f403019f1903ee1907d4ff8900f50102f5019f481b5b\
        \303b33316d78ff9f40ff8a0081008100f4f4f4f4f4f4f48201686d\
        \616b65202d6a328087002018658800f5f402f50302808900f50102\
        \f5019f481b5b303b33316d78ff9f40ff8a0081008100f4f4f4f4f4\
        \f4f482029f6376696d63612062ff8184008103801a6553f1008700\
        \20008800f5f402f50302808900f50102f5019f481b5b303b33316d\
        \78ff9f40ff8a0081008100f4f4f4f4f4f4f482016370776480ff81\
        \6470726576"
      , fixedCleanup, decModesTree )
    , ( 15
      , "851a484154520f039f82071864ff5901e9840079010d7b226c61\
        \73745f6163746976655f73657373696f6e223a22776f726b222c22\
        \73657373696f6e73223a5b7b2263757272656e745f6978223a302c\
        \226e616d65223a22776f726b222c2273746172745f637764223a22\
        \2f686f6d65222c2277696e646f7773223a5b7b2261637469766522\
        \3a302c226175746f5f72656e616d65223a747275652c226978223a\
        \302c226c61796f7574223a224c222c226e616d65223a2277222c22\
        \70616e6573223a5b7b22637764223a222f746d70227d2c7b226377\
        \64223a222f746d70227d2c7b22637764223a222f737276222c2265\
        \6d707479223a747275652c22696e7075745f6f6666223a74727565\
        \2c227469746c65223a2269646c65227d5d7d5d7d5d7d9f87000718\
        \648800f5f402f403019f1903ee1907d4ff8a00f50102f5019f481b\
        \5b303b33316d78ff9f40ff8a0081008100f4f4f4f4f4f4f4840003\
        \f4f58201686d616b65202d6a328087002018658800f5f402f50302\
        \808a00f50102f5019f481b5b303b33316d78ff9f40ff8a00810081\
        \00f4f4f4f4f4f4f4840000f4f482029f6376696d63612062ff8184\
        \008103801a6553f100870020008800f5f402f50302808a00f50102\
        \f5019f481b5b303b33316d78ff9f40ff8a0081008100f4f4f4f4f4\
        \f4f4840000f4f482016370776480ff816470726576"
      , fixedCleanup, cursorTree )
    ]

spec :: Spec
spec = describe "reload handover" $ do
    prop "round-trips a matching-era handover" $ \c t ->
        decodeHandover (encodeHandover c (hotOf t))
            === Right (Handover c (Right (t :: ReloadTree)))

    -- The safety contract: an incompatible payload is NOT adopted, but the
    -- version-independent cleanup core is still recovered, so the incoming
    -- image can hang up the inherited processes instead of orphaning them.
    it "gates a newer-era payload yet still recovers the cleanup core" $
        case decodeHandover
                (encodeAtEra (reloadEra + 1) fixedCleanup (hotOf fixedTree)) of
            Right h -> do
                h.cleanup `shouldBe` fixedCleanup
                h.tree `shouldSatisfy` isLeft
            Left e -> expectationFailure ("envelope should decode: " <> show e)

    it "rejects a foreign or corrupt blob outright" $
        decodeHandover (B.pack [0, 1, 2, 3]) `shouldSatisfy` isLeft

    -- A same-era payload the build still cannot trust: the tree and the hot
    -- list come from one capture walk, so any disagreement means it must not
    -- be adopted. The caller hangs the inherited handles up instead.
    it "refuses an unusable same-era payload yet recovers the cleanup core" $ do
        let full = hotOf fixedTree
            unusable =
                [ ReloadHot   -- tree that is not a tree at all
                    { tree = "}{", hot = full.hot, lastSession = Nothing }
                , ReloadHot   -- hot state that does not match the tree's panes
                    { tree = full.tree, hot = [], lastSession = Nothing } ]
        forM_ unusable $ \payload ->
            case decodeHandover (encodeHandover fixedCleanup payload) of
                Right h -> do
                    h.cleanup `shouldBe` fixedCleanup
                    h.tree `shouldSatisfy` isLeft
                Left e ->
                    expectationFailure ("envelope should decode: " <> show e)

    -- Backward compatibility: this build decodes every historical era's bytes.
    -- Failure means a payload change broke an old format — migrate it, don't
    -- edit the vector.
    it "decodes every era in the corpus" $
        mapM_ decodesToTree corpus

    -- Golden bytes are the format contract for the CURRENT era. An intended
    -- shape change bumps 'reloadEra' and appends a corpus row; an unintended
    -- one is a compat bug.
    it "encodes the current era to stable bytes" $
        hexOf (encodeHandover fixedCleanup (hotOf cursorTree))
            `shouldBe` currentGolden
  where
    decodesToTree (e, hex, cl, t) =
        (e, decodeHandover (unHex hex)) `shouldBe` (e, Right (Handover cl (Right t)))
    currentGolden = case [ hex | (e, hex, _, _) <- corpus, e == reloadEra ] of
        (hex : _) -> hex
        _         -> ""
