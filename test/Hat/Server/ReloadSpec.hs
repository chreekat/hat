{-# OPTIONS_GHC -Wno-orphans #-}  -- Arbitrary instances for the reload types live with their test; the library must not depend on QuickCheck

module Hat.Server.ReloadSpec (spec) where

import Codec.Serialise (encode, serialise)
import Codec.Serialise.Encoding (encodeListLen, encodeWord)
import Codec.CBOR.Write (toStrictByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.Word (Word8)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Server.Reload
import Hat.Term.Cell

genText :: Gen T.Text
genText = T.pack <$> listOf (chooseEnum (' ', '~'))

genMaybeText :: Gen (Maybe T.Text)
genMaybeText = oneof [pure Nothing, Just <$> genText]

-- The tree nests three list levels (sessions ⊃ windows ⊃ panes); unbounded
-- lengths multiply into a huge structure that makes the round-trip crawl. A
-- small per-level bound keeps generation cheap while still exercising nesting.
shortList :: (Arbitrary a) => Gen [a]
shortList = sized $ \n -> do
    k <- choose (0, min 3 n)
    vectorOf k arbitrary

instance Arbitrary ReloadCleanup where
    arbitrary = ReloadCleanup <$> arbitrary <*> shortList
    shrink c =
        [ ReloadCleanup fd lv | (fd, lv) <- shrink (c.listenFd, c.live) ]

instance Arbitrary ReloadState where
    arbitrary = ReloadState <$> shortList <*> genMaybeText <*> genMaybeText
    shrink rs =
        [ ReloadState ss (T.pack <$> cs) (T.pack <$> ls)
        | (ss, cs, ls) <- shrink
            ( rs.sessions
            , T.unpack <$> rs.currentSession
            , T.unpack <$> rs.lastSession ) ]

instance Arbitrary ReloadSession where
    arbitrary = ReloadSession
        <$> genText <*> genText <*> arbitrary <*> arbitrary <*> shortList
    shrink s =
        [ ReloadSession (T.pack nm) (T.pack c) i li ws
        | (nm, c, i, li, ws) <-
            shrink (T.unpack s.name, T.unpack s.startCwd, s.currentIx, s.windowHist, s.windows) ]

instance Arbitrary ReloadWindow where
    arbitrary = ReloadWindow
        <$> arbitrary <*> genText <*> genText
        <*> arbitrary <*> arbitrary <*> arbitrary <*> shortList
    shrink w =
        [ ReloadWindow i (T.pack nm) (T.pack lay) act la auto ps
        | ((i, nm, lay), (act, la, auto, ps)) <-
            shrink ( (w.ix, T.unpack w.name, T.unpack w.layout)
                   , (w.active, w.paneHist, w.autoRename, w.panes) ) ]

instance Arbitrary ReloadPane where
    arbitrary = ReloadPane
        <$> genText <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink p =
        [ ReloadPane (T.pack c) m pid ms sc
        | (c, m, pid, (ms, sc)) <-
            shrink (T.unpack p.cwd, p.masterFd, p.childPid, (p.modes, p.screen)) ]

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

-- Re-encode a handover at an arbitrary era, to exercise the era gate. Mirrors
-- 'encodeHandover' exactly except for the era field; the golden-byte test
-- below pins the real encoder, so a format change surfaces there.
encodeAtEra :: Int -> ReloadCleanup -> ReloadState -> B.ByteString
encodeAtEra era c t = toStrictByteString $
       encodeListLen 5
    <> encodeWord 0x48415452
    <> encode era
    <> encode c.listenFd
    <> encode c.live
    <> encode (BL.toStrict (serialise t))

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
treeWith :: Maybe T.Text -> ReloadModes -> ReloadScreen -> ReloadState
treeWith mlast ms sc = ReloadState
    { sessions =
        [ ReloadSession
            { name = "work", startCwd = "/home", currentIx = 0, windowHist = []
            , windows =
                [ ReloadWindow
                    { ix = 0, name = "w", layout = "L", active = 0
                    , paneHist = [], autoRename = True
                    , panes =
                        [ ReloadPane
                            { cwd = "/tmp", masterFd = 7, childPid = 100
                            , modes = ms, screen = sc } ] } ] } ]
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
fixedTree :: ReloadState
fixedTree = treeWith (Just "prev")
    ReloadModes { colorReport = True, focusReport = False, mouse = 2 }
    fixedScreen

-- The reload corpus: one committed encoding per era. A build MUST decode every
-- vector here into the current tree (armor-style backward-compat enforcement).
-- When 'reloadEra' is bumped, DO NOT edit an existing row's bytes — append a new
-- one. A row's target tree is what its bytes migrate to in THIS build, so it
-- tracks the current shape: the pre-screen era-1 and era-2 rows migrate to a
-- blank ('emptyReloadScreen') pane.
corpus :: [(Int, String, ReloadCleanup, ReloadState)]
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
    ]

spec :: Spec
spec = describe "reload handover" $ do
    prop "round-trips a matching-era handover" $ \c t ->
        decodeHandover (encodeHandover c t)
            === Right (Handover c (Right (t :: ReloadState)))

    -- The safety contract: an incompatible payload is NOT adopted, but the
    -- version-independent cleanup core is still recovered, so the incoming
    -- image can hang up the inherited processes instead of orphaning them.
    it "gates a newer-era payload yet still recovers the cleanup core" $
        case decodeHandover (encodeAtEra (reloadEra + 1) fixedCleanup fixedTree) of
            Right h -> do
                h.cleanup `shouldBe` fixedCleanup
                h.tree `shouldSatisfy` isLeft
            Left e -> expectationFailure ("envelope should decode: " <> show e)

    it "rejects a foreign or corrupt blob outright" $
        decodeHandover (B.pack [0, 1, 2, 3]) `shouldSatisfy` isLeft

    -- Backward compatibility: this build decodes every historical era's bytes.
    -- Failure means a payload change broke an old format — migrate it, don't
    -- edit the vector.
    it "decodes every era in the corpus" $
        mapM_ decodesToTree corpus

    -- Golden bytes are the format contract for the CURRENT era. An intended
    -- shape change bumps 'reloadEra' and appends a corpus row; an unintended
    -- one is a compat bug.
    it "encodes the current era to stable bytes" $
        hexOf (encodeHandover fixedCleanup fixedTree)
            `shouldBe` currentGolden
  where
    decodesToTree (e, hex, cl, t) =
        (e, decodeHandover (unHex hex)) `shouldBe` (e, Right (Handover cl (Right t)))
    currentGolden = case [ hex | (e, hex, _, _) <- corpus, e == reloadEra ] of
        (hex : _) -> hex
        _         -> ""
