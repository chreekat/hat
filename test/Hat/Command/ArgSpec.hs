{-# OPTIONS_GHC -Wno-orphans #-}  -- Arbitrary Arg lives with its tests: the library must not depend on QuickCheck

module Hat.Command.ArgSpec (spec) where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Test.Hspec hiding (Arg)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Command.Arg

-- | Mostly text, sometimes stray bytes. A raw argument shrinks by dropping
-- bytes for as long as what is left stays invalid; a text one shrinks its
-- text.
instance Arbitrary Arg where
    arbitrary = argFromBytes . B.pack <$> oneof
        [ B.unpack . TE.encodeUtf8 . T.pack <$> arbitrary
        , arbitrary ]
    shrink = \case
        Utf8Arg t -> Utf8Arg . T.pack <$> shrink (T.unpack t)
        RawArg bytes ->
            [ RawArg b | b <- B.pack <$> shrinkList (const []) (B.unpack bytes)
                       , RawArg _ <- [argFromBytes b] ]

spec :: Spec
spec = do
    prop "UTF-8 bytes are text" $ \s ->
        let t = T.pack s in argFromBytes (TE.encodeUtf8 t) === Utf8Arg t
    prop "the lossy text is the lenient decoding" $ \arg ->
        argLossy arg === TE.decodeUtf8Lenient (argBytes arg)
    prop "bytes round-trip" $ \arg ->
        argFromBytes (argBytes arg) === arg
    it "is raw for bytes no strict decoder accepts" $
        mapM_ (\b -> argFromBytes b `shouldBe` RawArg b)
            ["a\xffz", "\xc2", "\xc0\x81", "\xed\xa0\x80", "\xf4\x90\x80\x80"]
