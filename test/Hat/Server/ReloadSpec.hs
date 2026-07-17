{-# OPTIONS_GHC -Wno-orphans #-}  -- Arbitrary instances for the reload types live with their test; the library must not depend on QuickCheck

module Hat.Server.ReloadSpec (spec) where

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

instance Arbitrary ReloadState where
    arbitrary = ReloadState <$> shortList <*> genMaybeText <*> arbitrary
    shrink rs =
        [ ReloadState ss (T.pack <$> cs) fd
        | (ss, cs, fd) <-
            shrink (rs.sessions, T.unpack <$> rs.currentSession, rs.listenFd) ]

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

spec :: Spec
spec =
    describe "reload handover codec" $
        prop "round-trips a ReloadState through CBOR" $ \rs ->
            decodeReload (encodeReload rs) === Right (rs :: ReloadState)
