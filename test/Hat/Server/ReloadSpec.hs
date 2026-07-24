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
    arbitrary = ReloadState <$> shortList <*> genMaybeText
    shrink rs =
        [ ReloadState ss (T.pack <$> cs)
        | (ss, cs) <- shrink (rs.sessions, T.unpack <$> rs.currentSession) ]

instance Arbitrary ReloadSession where
    arbitrary = ReloadSession
        <$> genText <*> genText <*> arbitrary <*> arbitrary <*> shortList
    shrink s =
        [ ReloadSession (T.pack nm) (T.pack c) i li ws
        | (nm, c, i, li, ws) <-
            shrink (T.unpack s.name, T.unpack s.startCwd, s.currentIx, s.lastIx, s.windows) ]

instance Arbitrary ReloadWindow where
    arbitrary = ReloadWindow
        <$> arbitrary <*> genText <*> genText
        <*> arbitrary <*> arbitrary <*> arbitrary <*> shortList
    shrink w =
        [ ReloadWindow i (T.pack nm) (T.pack lay) act la auto ps
        | ((i, nm, lay), (act, la, auto, ps)) <-
            shrink ( (w.ix, T.unpack w.name, T.unpack w.layout)
                   , (w.active, w.lastActive, w.autoRename, w.panes) ) ]

instance Arbitrary ReloadPane where
    arbitrary = ReloadPane <$> genText <*> arbitrary <*> arbitrary <*> arbitrary
    shrink p =
        [ ReloadPane (T.pack c) m pid ms
        | (c, m, pid, ms) <-
            shrink (T.unpack p.cwd, p.masterFd, p.childPid, p.modes) ]

instance Arbitrary ReloadModes where
    arbitrary = ReloadModes <$> arbitrary <*> arbitrary <*> choose (0, 3)
    shrink m =
        [ ReloadModes cr fr mo
        | (cr, fr, mo) <- shrink (m.colorReport, m.focusReport, m.mouse) ]

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

-- A one-pane tree carrying the given mode subscriptions on its pane.
treeWith :: ReloadModes -> ReloadState
treeWith ms = ReloadState
    { sessions =
        [ ReloadSession
            { name = "work", startCwd = "/home", currentIx = 0, lastIx = Nothing
            , windows =
                [ ReloadWindow
                    { ix = 0, name = "w", layout = "L", active = 0
                    , lastActive = Nothing, autoRename = True
                    , panes =
                        [ ReloadPane
                            { cwd = "/tmp", masterFd = 7, childPid = 100
                            , modes = ms } ] } ] } ]
    , currentSession = Just "work" }

-- The current-era representative, with a non-trivial mode set to pin its
-- encoding.
fixedTree :: ReloadState
fixedTree = treeWith ReloadModes { colorReport = True, focusReport = False, mouse = 2 }

-- The reload corpus: one committed encoding per era. A build MUST decode every
-- vector here into the current tree (armor-style backward-compat enforcement).
-- When 'reloadEra' is bumped, DO NOT edit an existing row — append a new one
-- with the new era's bytes and the tree they should migrate to. The era-1 row
-- predates pane modes, so it migrates to an all-off mode set.
corpus :: [(Int, String, ReloadCleanup, ReloadState)]
corpus =
    [ ( 1
      , "851a4841545201039f82071864ff583183009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8400642f746d70\
        \071864ffffff8164776f726b"
      , fixedCleanup, treeWith (ReloadModes False False 0) )
    , ( 2
      , "851a4841545202039f82071864ff583683009f860064776f726b\
        \652f686f6d6500809f8800006177614c0080f59f8500642f746d70\
        \0718648400f5f402ffffff8164776f726b"
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
    decodesToTree (_, hex, cl, t) =
        decodeHandover (unHex hex) `shouldBe` Right (Handover cl (Right t))
    currentGolden = case [ hex | (e, hex, _, _) <- corpus, e == reloadEra ] of
        (hex : _) -> hex
        _         -> ""
