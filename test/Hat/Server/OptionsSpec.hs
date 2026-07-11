-- | The config-load burn-down: loading the author's real @~/.tmux.conf@
-- must reject exactly the options whose behavior hat has not yet
-- implemented — never silently accept them. As each milestone lands its
-- behavior, its option leaves 'expectedUnimplemented' and this list
-- shrinks toward empty.
module Hat.Server.OptionsSpec (spec) where

import Data.Either (lefts)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Hspec

import Hat.Command.Parser (parseConfig)
import Hat.Model.Options (Options (..), defaultOptions)
import Hat.Server (setOption)

-- Apply every top-level @set@ in a config, returning the errors it logs.
loadSetErrors :: [[Text]] -> [Text]
loadSetErrors cmds = lefts (snd (List.foldl' step (defaultOptions, []) cmds))
  where
    step (opts, errs) argv = case argvSet argv of
        Nothing -> (opts, errs)
        Just (append, name, value) -> case setOption append opts name value of
            Right opts' -> (opts', errs)
            Left err    -> (opts, Left err : errs)

-- Parse a @set-option@ argv into (append, name, value); 'Nothing' if the
-- command is not a set. Leading @-flags@ are options; @-a@ means append.
argvSet :: [Text] -> Maybe (Bool, Text, Text)
argvSet (cmd : rest)
    | cmd `elem` ["set-option", "set", "set-window-option", "setw"]
    , let (flags, pos) = span ("-" `T.isPrefixOf`) rest
    , (name : valueWords) <- pos =
        Just (any (T.isInfixOf "a") flags, name, T.unwords valueWords)
argvSet _ = Nothing

spec :: Spec
spec = do
    describe "setOption" $ do
        it "rejects an unknown option instead of silently storing it" $
            setOption False defaultOptions "no-such-option" "x"
                `shouldBe` Left "unimplemented option: no-such-option"

        it "stores @-options in the user map" $
            fmap (Map.lookup "@foo" . (.user))
                (setOption False defaultOptions "@foo" "bar")
                `shouldBe` Right (Just "bar")

        it "appends to a string option with -a" $ do
            let set app opts name v =
                    either (const opts) id (setOption app opts name v)
                opts0 = set False defaultOptions "status-right" "a"
                opts1 = set True opts0 "status-right" "b" :: Options
            (opts1.statusRight :: Text) `shouldBe` "ab"

    describe "config-load burn-down (real ~/.tmux.conf)" $
        it "rejects exactly the not-yet-implemented options" $ do
            contents <- TIO.readFile "test/fixtures/tmux.conf"
            case parseConfig contents of
                Left err -> expectationFailure (T.unpack err)
                Right cmds ->
                    List.sort (loadSetErrors cmds)
                        `shouldBe` expectedUnimplemented

-- The living burn-down list. Delete a line here the moment its
-- milestone implements the option's behavior.
expectedUnimplemented :: [Text]
expectedUnimplemented =
    [ "unimplemented option: main-pane-width" ]
