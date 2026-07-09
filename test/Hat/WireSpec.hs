module Hat.WireSpec (spec) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Geometry
import Hat.Term.Cell
import Hat.Wire

instance Arbitrary Size where
    arbitrary = Size <$> arbitrary <*> arbitrary
    shrink sz = [Size r c | (r, c) <- shrink (sz.rows, sz.cols)]

instance Arbitrary Pos where
    arbitrary = Pos <$> arbitrary <*> arbitrary
    shrink p = [Pos r c | (r, c) <- shrink (p.row, p.col)]

instance Arbitrary Color where
    arbitrary = oneof
        [ pure DefaultColor
        , Indexed <$> arbitrary
        , RGB <$> arbitrary <*> arbitrary <*> arbitrary
        ]
    shrink = \case
        DefaultColor -> []
        Indexed n -> DefaultColor : (Indexed <$> shrink n)
        RGB r g b -> DefaultColor : [RGB r' g' b' | (r', g', b') <- shrink (r, g, b)]

instance Arbitrary Style where
    arbitrary = Style <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink st =
        [ Style fg' bg' bo un it rv sk bl
        | (fg', bg', bo, un, it, rv, sk, bl) <-
            shrink (st.fg, st.bg, st.bold, st.underline, st.italic,
                    st.reverse, st.strike, st.blink)
        ]

genText :: Gen T.Text
genText = T.pack <$> listOf (chooseEnum (' ', '~'))

instance Arbitrary DrawOp where
    arbitrary = oneof
        [ Put <$> arbitrary <*> arbitrary <*> genText
        , pure ClearAll
        , CursorAt <$> arbitrary <*> arbitrary
        ]
    shrink = \case
        Put p st t -> ClearAll : [Put p' st' (T.pack t') | (p', st', t') <- shrink (p, st, T.unpack t)]
        ClearAll -> []
        CursorAt p v -> ClearAll : [CursorAt p' v' | (p', v') <- shrink (p, v)]

instance Arbitrary ClientToServer where
    arbitrary = oneof
        [ Hello <$> arbitrary <*> genText <*> listOf ((,) <$> genText <*> genText)
                <*> arbitrary <*> genText
                <*> elements [AttachIntent, ControlIntent]
        , Input . B.pack <$> arbitrary
        , Resize <$> arbitrary
        , Command <$> listOf (listOf genText)
        , pure Detach
        ]
    shrink _ = []

instance Arbitrary ServerToClient where
    arbitrary = oneof
        [ Welcome <$> genText
        , Draw <$> listOf arbitrary
        , SetTitle <$> genText
        , pure RingBell
        , Message <$> genText
        , pure DetachOk
        , pure CommandDone
        , ServerError <$> genText
        , pure Exited
        ]
    shrink _ = []

spec :: Spec
spec = do
    prop "client messages roundtrip" $ \(msg :: ClientToServer) ->
        decodeMessage (encodeMessage msg) === Right msg

    prop "server messages roundtrip" $ \(msg :: ServerToClient) ->
        decodeMessage (encodeMessage msg) === Right msg

    it "rejects garbage" $
        (decodeMessage "not cbor at all" :: Either String ServerToClient)
            `shouldSatisfy` \case
                Left _ -> True
                Right _ -> False
