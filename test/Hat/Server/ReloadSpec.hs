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
    arbitrary = ReloadPane <$> genText <*> arbitrary <*> arbitrary
    shrink p =
        [ ReloadPane (T.pack c) m pid
        | (c, m, pid) <- shrink (T.unpack p.cwd, p.masterFd, p.childPid) ]

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

fixedCleanup :: ReloadCleanup
fixedCleanup = ReloadCleanup { listenFd = 3, live = [(7, 100)] }

fixedTree :: ReloadState
fixedTree = ReloadState
    { sessions =
        [ ReloadSession
            { name = "work", startCwd = "/home", currentIx = 0, lastIx = Nothing
            , windows =
                [ ReloadWindow
                    { ix = 0, name = "w", layout = "L", active = 0
                    , lastActive = Nothing, autoRename = True
                    , panes = [ ReloadPane { cwd = "/tmp", masterFd = 7, childPid = 100 } ] } ] } ]
    , currentSession = Just "work" }

spec :: Spec
spec = describe "reload handover" $ do
    prop "round-trips a matching-era handover" $ \c t ->
        decodeHandover (encodeHandover c t)
            === Right (Handover c (Just (t :: ReloadState)))

    -- The safety contract: an incompatible payload is NOT decoded, but the
    -- version-independent cleanup core is still recovered, so the incoming
    -- image can hang up the inherited processes instead of orphaning them.
    it "gates a stale-era payload yet still recovers the cleanup core" $
        decodeHandover (encodeAtEra (reloadEra + 1) fixedCleanup fixedTree)
            `shouldBe` Right (Handover fixedCleanup Nothing)

    it "rejects a foreign or corrupt blob outright" $
        decodeHandover (B.pack [0, 1, 2, 3]) `shouldSatisfy` isLeft

    -- Golden bytes are the format contract. If this fails from an intended
    -- change to ReloadState's shape, bump 'reloadEra' and update the golden;
    -- if you didn't mean to change the shape, you have a compat bug.
    it "encodes a fixed handover to stable bytes" $
        hexOf (encodeHandover fixedCleanup fixedTree) `shouldBe` golden
  where
    golden = "851a4841545201039f82071864ff583183009f860064776f726b\
             \652f686f6d6500809f8800006177614c0080f59f8400642f746d70\
             \071864ffffff8164776f726b"
