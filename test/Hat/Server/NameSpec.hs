module Hat.Server.NameSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Either (isLeft, isRight)
import Test.Hspec hiding (Arg)

import Hat.Command.Arg
import Hat.Server.Command.Types (Reply (..), errQuoting, replyErrorBytes)
import Hat.Server.Name

spec :: Spec
spec = do
    describe "checkName" $ do
        forM_ refused $ \(what, bytes) ->
            it ("refuses " <> what) $
                checkName (argFromBytes bytes) `shouldSatisfy` isLeft
        forM_ accepted $ \(what, bytes) ->
            it ("accepts " <> what) $
                checkName (argFromBytes bytes) `shouldSatisfy` isRight
    describe "admitName" $ do
        it "quotes a refused name's bytes verbatim" $
            case admitName SessionName (argFromBytes "a\xffz") of
                Left r -> do
                    replyErrorBytes r `shouldBe` Just "invalid session name: a\xffz"
                    r `shouldBe` errQuoting "invalid session name: " (RawArg "a\xffz")
                Right _ -> expectationFailure "admitted"
        it "words a refused text name as text" $
            admitName SessionGroupName (Utf8Arg "a\tb")
                `shouldBe` Left (RErr "invalid session group name: a\tb")
  where
    refused :: [(String, ByteString)]
    refused =
        [ ("a lone lead byte", "\xc2"), ("a stray byte", "\xff")
        , ("an overlong form", "\xc0\x81"), ("a surrogate", "\xed\xa0\x80")
        , ("a code point past U+10FFFF", "\xf4\x90\x80\x80")
        , ("a tab", "a\tb"), ("DEL", "a\x7f") ]
    accepted =
        [ ("the empty name", ""), ("':' and '.'", "a:b.c"), ("'#('", "#(")
        , ("a C1 control", "\xc2\x80"), ("UTF-8 beyond ASCII", "caf\xc3\xa9") ]
