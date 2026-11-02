{-# OPTIONS_GHC -Wno-orphans #-}  -- the library must not depend on QuickCheck

module Hat.Server.ReloadProbeSpec (spec) where

import Data.Either (isLeft)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Server.ReloadProbe

-- Ranges of small, positive eras, possibly overlapping or out of order.
instance Arbitrary ReloadEras where
    arbitrary = erasFromRanges <$> listOf1 range
      where
        range = do
            lo <- chooseInt (1, 40)
            len <- chooseInt (0, 5)
            pure (lo, lo + len)
    shrink es =
        [ erasFromRanges rs
        | rs <- shrinkList shrinkRange (erasRanges es), not (null rs) ]
      where
        shrinkRange (lo, hi) =
            [ (lo, hi') | hi' <- [lo .. hi - 1] ]
            <> [ (lo', hi) | lo' <- [1 .. lo - 1] ]

-- One committed answer line per probe format, with the eras it means. A build
-- MUST read every one; append a vector, never edit one.
answerCorpus :: [(String, ReloadEras)]
answerCorpus =
    [ ("hat-reload-eras 1 1-15", erasFromRanges [(1, 15)])
    , ("hat-reload-eras 1 1-10,12,14-16", erasFromRanges [(1, 10), (12, 12), (14, 16)])
    , ("hat-reload-eras 1 1-15 a-later-field", erasFromRanges [(1, 15)])
    , ("hat-reload-eras 1 1-15\n", erasFromRanges [(1, 15)])
    ]

refusal :: ProbeOutcome -> Maybe T.Text
refusal = judgeReloadTarget "restart-server" "/old/hat" 15

spec :: Spec
spec = describe "reload-era probe" $ do
    it "answers with a frozen line" $
        renderProbeAnswer (erasFromRanges [(1, 15)]) `shouldBe` "hat-reload-eras 1 1-15"

    it "reads every answer in the corpus" $
        mapM_ (\(line, es) -> (line, parseProbeAnswer (T.pack line)) `shouldBe` (line, Right es))
            answerCorpus

    prop "reads back what it renders" $ \es ->
        parseProbeAnswer (renderProbeAnswer es) === Right (es :: ReloadEras)

    it "treats an unknown format, or anything else, as no answer" $
        mapM_ (\out -> (out, parseProbeAnswer out) `shouldSatisfy` (isLeft . snd))
            [ "", "hat: no server running", "hat-reload-eras 2 1-15"
            , "hat-reload-eras 1", "hat-reload-eras 1 1-x", "hat-reload-eras 1 5-3"
            , "hat-reload-eras 1 0-3", "hat 0.1.0" ]

    it "proceeds when the target reads this server's era" $
        refusal (Answered (erasFromRanges [(1, 15)])) `shouldBe` Nothing

    it "refuses a target that reads only older eras (bug 52)" $
        refusal (Answered (erasFromRanges [(1, 10)])) `shouldBe` Just
            "restart-server: /old/hat cannot read this server's state \
            \(it reads eras 1-10, this is era 15); not restarting"

    it "refuses a target that gives no answer (bug 52)" $
        refusal (NoAnswer (ProbeExited 1)) `shouldBe` Just
            "restart-server: /old/hat cannot say which reload eras it reads \
            \(it exited 1 without an answer), so it may not read this server's \
            \state (era 15); not restarting"
