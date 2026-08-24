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

import Hat.Server.Persist
import Hat.Server.PersistSpec ()  -- the tree generators, shared with the store
import Hat.Server.Reload
import Hat.Term.Cell

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
        [ HotSession s.name s.startCwd s.currentIx s.windowHist ws
        | ws <- shrinkList shrink s.windows, not (null ws) ]
        ++ [ HotSession s.name s.startCwd s.currentIx h s.windows
           | h <- shrinkList (const []) s.windowHist ]

hotSessionOf :: SessionSnap -> Gen HotSession
hotSessionOf s = do
    ws <- mapM hotWindowOf s.windows
    pure HotSession
        { name = s.name, startCwd = s.startCwd, currentIx = s.currentIx
        , windowHist = s.windowHist, windows = ws }

instance Arbitrary HotWindow where
    arbitrary = hotWindowOf =<< arbitrary
    shrink w =
        [ hw w.paneHist w.autoRename ps
        | ps <- shrinkList shrinkHot w.panes, not (null ps) ]
        ++ [ hw h w.autoRename w.panes | h <- shrinkList (const []) w.paneHist ]
        ++ [ hw w.paneHist False w.panes | w.autoRename ]
      where
        hw = HotWindow w.ix w.name w.layout w.active
        shrinkHot (p, hp) =
            [ (p', hp) | p' <- shrink p ] ++ [ (p, hp') | hp' <- shrink hp ]

hotWindowOf :: WindowSnap -> Gen HotWindow
hotWindowOf w = do
    ps <- mapM (\p -> (,) p <$> arbitrary) w.panes
    pure HotWindow
        { ix = w.ix, name = w.name, layout = w.layout, active = w.active
        , paneHist = w.paneHist, autoRename = w.autoRename, panes = ps }

instance Arbitrary HotPane where
    arbitrary = HotPane <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink p =
        [ HotPane fd cpid ms sc
        | (fd, cpid, ms, sc) <-
            shrink (p.masterFd, p.childPid, p.modes, p.screen) ]

instance Arbitrary ReloadModes where
    arbitrary = ReloadModes <$> arbitrary <*> arbitrary <*> choose (0, 3)
    shrink m =
        [ ReloadModes cr fr mo
        | (cr, fr, mo) <- shrink (m.colorReport, m.focusReport, m.mouse) ]

instance Arbitrary ReloadScreen where
    arbitrary = ReloadScreen
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> shortGrid <*> shortGrid <*> arbitrary
    shrink = genericShrink

-- Small grids keep the round-trip cheap; the cells only need to survive the
-- CBOR codec, not mean anything to the emulator.
shortGrid :: Gen [[Cell]]
shortGrid = do
    r <- choose (0, 2)
    vectorOf r (choose (0, 3) >>= (`vectorOf` arbitrary))

instance Arbitrary Cell where
    arbitrary = Cell
        <$> (T.pack <$> vectorOf 1 (chooseEnum ('a', '~')))
        <*> elements [1, 2] <*> arbitrary
    shrink c =
        [ Cell (T.pack t) w s
        | (t, w, s) <- shrink (T.unpack c.text, c.width, c.style) ]

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
        , windowHist = s.windowHist, windows = map windowOf s.windows }
    windowOf w = WindowSnap
        { ix = w.ix, name = w.name, layout = w.layout, active = w.active
        , paneHist = w.paneHist, autoRename = w.autoRename
        , panes = map fst w.panes }

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
            , windows =
                [ HotWindow
                    { ix = 0, name = "w", layout = "L", active = 0
                    , paneHist = [], autoRename = True
                    , panes =
                        [ ( PaneSnap
                              { cwd = "/tmp", command = Nothing
                              , shellSpawned = False }
                          , HotPane
                              { masterFd = 7, childPid = 100
                              , modes = ms, screen = sc } ) ] } ] } ]
    , currentSession = Just "work"
    , lastSession = mlast }

-- A non-trivial captured screen, to pin the current era's encoding: an
-- alt-screen pane with one styled live cell and one scrollback cell.
fixedScreen :: ReloadScreen
fixedScreen = ReloadScreen
    { altScreen = True, cursorRow = 1, cursorCol = 2, cursorVisible = True
    , rows = [[ Cell { text = "x", width = 1
                     , style = defaultStyle { fg = Indexed 1 } } ]]
    , scrollback = [[ blankCell ]], pen = defaultStyle }

-- The current-era representative, with a non-trivial mode set, screen, and
-- alternate session to pin its encoding.
fixedTree :: ReloadTree
fixedTree = treeWith (Just "prev")
    ReloadModes { colorReport = True, focusReport = False, mouse = 2 }
    fixedScreen

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
      , treeWith Nothing (ReloadModes False False 0) emptyReloadScreen )
    , ( 2
      , "851a4841545202039f82071864ff583683009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8500642f746d70\
        \0718648400f5f402ffffff8164776f726b"
      , fixedCleanup
      , treeWith Nothing (ReloadModes True False 2) emptyReloadScreen )
    , ( 3
      , "851a4841545203039f82071864ff586783009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8600642f746d70\
        \0718648400f5f4028700f50102f59f9f840061780189008201018100\
        \f4f4f4f4f4f4ffff9f9f8400612001890081008100f4f4f4f4f4f4ff\
        \ffffffff8164776f726b"
      , fixedCleanup
      , treeWith Nothing (ReloadModes True False 2) fixedScreen )
    , ( 4
      , "851a4841545204039f82071864ff586d84009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8600642f746d70\
        \0718648400f5f4028700f50102f59f9f840061780189008201018100\
        \f4f4f4f4f4f4ffff9f9f8400612001890081008100f4f4f4f4f4f4ff\
        \ffffffff8164776f726b816470726576"
      , fixedCleanup, fixedTree )
    , ( 5
      , "851a4841545205039f82071864ff586f84009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8600642f746d70\
        \0718648400f5f4028700f50102f59f9f84006178018a0082010181\
        \00f4f4f4f4f4f4f4ffff9f9f84006120018a0081008100f4f4f4f4\
        \f4f4f4ffffffffff8164776f726b816470726576"
      , fixedCleanup, fixedTree )
    , ( 6
      , "851a4841545206039f82071864ff587c84009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8600642f746d70\
        \0718648400f5f4028800f50102f59f9f84006178018a0082010181\
        \00f4f4f4f4f4f4f4ffff9f9f84006120018a0081008100f4f4f4f4\
        \f4f4f4ffff8a0081008100f4f4f4f4f4f4f4ffffff8164776f726b\
        \816470726576"
      , fixedCleanup, fixedTree )
    , ( 7
      , "851a4841545207039f82071864ff587c84009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8600642f746d70\
        \0718648400f5f4028800f50102f59f9f84006178018a0082010181\
        \00f4f4f4f4f4f4f4ffff9f9f84006120018a0081008100f4f4f4f4\
        \f4f4f4ffff8a0081008100f4f4f4f4f4f4f4ffffff8164776f726b\
        \816470726576"
      , fixedCleanup, fixedTree )
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
      , fixedCleanup, fixedTree )
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
        hexOf (encodeHandover fixedCleanup (hotOf fixedTree))
            `shouldBe` currentGolden
  where
    decodesToTree (e, hex, cl, t) =
        (e, decodeHandover (unHex hex)) `shouldBe` (e, Right (Handover cl (Right t)))
    currentGolden = case [ hex | (e, hex, _, _) <- corpus, e == reloadEra ] of
        (hex : _) -> hex
        _         -> ""
