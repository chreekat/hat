-- | The config-load burn-down: loading the author's real @~/.tmux.conf@
-- must reject exactly the options whose behavior hat has not yet
-- implemented — never silently accept them. As each milestone lands its
-- behavior, its option leaves 'expectedUnimplemented' and this list
-- shrinks toward empty.
module Hat.Server.OptionsSpec (spec) where

import Data.Either (isLeft, lefts)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Hspec

import Hat.Command.Parser (parseConfig)
import Hat.Model.Options
    ( Options (..), defaultOptions
    , OptionName (..), OptionValue (..), ScopeClass (..)
    , emptyDelta, singletonDelta, mergeDeltas, resolveOptions
    , optionScopeClass, validateScope, resolveOptionName
    )
import Hat.Server
    ( SetMode (..), setOption, chooseScope, SetScope (..), SetDefault (..)
    , placeWindow
    )

-- Apply every top-level @set@ in a config, returning the errors it logs.
loadSetErrors :: [[Text]] -> [Text]
loadSetErrors cmds = lefts (snd (List.foldl' step (defaultOptions, []) cmds))
  where
    step (opts, errs) argv = case argvSet argv of
        Nothing -> (opts, errs)
        Just (mode, name, value) -> case setOption mode opts name value of
            Right opts' -> (opts', errs)
            Left err    -> (opts, Left err : errs)

-- Parse a @set-option@ argv into (mode, name, value); 'Nothing' if the
-- command is not a set. Leading @-flags@ are options; @-a@ means append.
argvSet :: [Text] -> Maybe (SetMode, Text, Text)
argvSet (cmd : rest)
    | cmd `elem` ["set-option", "set", "set-window-option", "setw"]
    , let (flags, pos) = span ("-" `T.isPrefixOf`) rest
    , (name : valueWords) <- pos =
        Just ( if any (T.isInfixOf "a") flags then Append else Assign
             , name, T.unwords valueWords )
argvSet _ = Nothing

spec :: Spec
spec = do
    describe "setOption" $ do
        it "rejects an unknown option instead of silently storing it" $
            setOption Assign defaultOptions "no-such-option" "x"
                `shouldBe` Left "invalid option: no-such-option"

        it "stores @-options in the user map" $
            fmap (Map.lookup "@foo" . (.user))
                (setOption Assign defaultOptions "@foo" "bar")
                `shouldBe` Right (Just "bar")

        it "appends to a string option with -a" $ do
            let set mode opts name v =
                    either (const opts) id (setOption mode opts name v)
                opts0 = set Assign defaultOptions "status-right" "a"
                opts1 = set Append opts0 "status-right" "b" :: Options
            (opts1.statusRight :: Text) `shouldBe` "ab"

    describe "resolveOptionName" $ do
        it "resolves an exact name" $
            resolveOptionName "status-left" `shouldBe` Right OptStatusLeft

        it "resolves a unique prefix" $
            resolveOptionName "hist" `shouldBe` Right OptHistoryLimit

        it "prefers an exact name over a longer collision" $
            -- "status-left" is itself a prefix of "status-left-length"
            resolveOptionName "status-left-l"
                `shouldBe` Right OptStatusLeftLength

        it "rejects an ambiguous prefix in tmux's words" $
            resolveOptionName "status-l"
                `shouldBe` Left "ambiguous option: status-l"

        it "rejects an unknown name in tmux's words" $
            resolveOptionName "no-such-option"
                `shouldBe` Left "invalid option: no-such-option"

        it "passes @-options through untouched" $
            resolveOptionName "@foo" `shouldBe` Right (OptUser "@foo")

    describe "option-scope overlays" $ do
        it "resolves an empty chain to the defaults" $
            resolveOptions [] `shouldBe` defaultOptions

        it "an empty delta changes nothing" $
            resolveOptions [emptyDelta] `shouldBe` defaultOptions

        it "a single delta overrides only its own field" $ do
            let opts = resolveOptions [singletonDelta OptPrefix (OVText "C-a")]
            opts.prefix `shouldBe` "C-a"
            opts.baseIndex `shouldBe` defaultOptions.baseIndex

        it "a more-specific delta wins field-by-field" $ do
            let global = mergeDeltas
                    [ singletonDelta OptPrefix (OVText "C-a")
                    , singletonDelta OptBaseIndex (OVInt 0) ]
                session = singletonDelta OptBaseIndex (OVInt 1)
                -- most-specific first: session shadows global
                opts = resolveOptions [session, global]
            opts.prefix `shouldBe` "C-a"      -- only set globally
            opts.baseIndex `shouldBe` 1       -- session overrides global

        it "window scope beats session beats global" $ do
            let g = singletonDelta OptHistoryLimit (OVInt 1)
                s = singletonDelta OptHistoryLimit (OVInt 2)
                w = singletonDelta OptHistoryLimit (OVInt 3)
            (resolveOptions [w, s, g]).historyLimit `shouldBe` 3
            (resolveOptions [s, g]).historyLimit `shouldBe` 2
            (resolveOptions [g]).historyLimit `shouldBe` 1

    describe "option-scope classification" $ do
        it "classifies prefix as a session option" $
            optionScopeClass OptPrefix `shouldBe` SessionOption

        it "classifies history-limit as a session option" $
            optionScopeClass OptHistoryLimit `shouldBe` SessionOption

        it "classifies mode-keys as a window option" $
            optionScopeClass OptModeKeys `shouldBe` WindowOption

        it "classifies monitor-activity as a window option" $
            optionScopeClass OptMonitorActivity `shouldBe` WindowOption

        it "accepts a session option at session scope" $
            validateScope SessionOption OptPrefix `shouldBe` Right ()

        it "rejects a session option set at window scope (setw prefix)" $
            validateScope WindowOption OptPrefix `shouldSatisfy` isLeft

        it "rejects a window option set at session scope" $
            validateScope SessionOption OptMonitorActivity `shouldSatisfy` isLeft

    describe "set-option scope routing" $ do
        it "bare set scopes a session option to the current session" $
            chooseScope DefaultSession [] OptPrefix
                `shouldBe` Right SetLocalSession

        it "set -g scopes a session option to the global session table" $
            chooseScope DefaultSession ["-g"] OptPrefix
                `shouldBe` Right SetGlobalSession

        it "set -g routes a window option to the global window table by class" $
            chooseScope DefaultSession ["-g"] OptModeKeys
                `shouldBe` Right SetGlobalWindow

        it "set -gw on a session option stays global-session (real-config case)" $
            -- the author's ~/.tmux.conf has `set -gw display-time 1500`
            chooseScope DefaultSession ["-g", "-w"] OptDisplayTime
                `shouldBe` Right SetGlobalSession

        it "setw routes a window option to the current window" $
            chooseScope DefaultWindow [] OptModeKeys
                `shouldBe` Right SetLocalWindow

        it "setw on a session option fails loud (like tmux's setw prefix)" $
            chooseScope DefaultWindow [] OptPrefix `shouldSatisfy` isLeft

        it "set -s scopes a server option to the server table" $
            chooseScope DefaultSession ["-s"] OptDefaultTerminal
                `shouldBe` Right SetServer

        it "set -s on a session option fails loud" $
            chooseScope DefaultSession ["-s"] OptPrefix `shouldSatisfy` isLeft

        it "set -s @user routes a user option to the server table" $
            -- @-prefixed user options are valid at every scope in tmux, so
            -- `set -s @done yes` (as if-shell-nested.sh does) must not be rejected.
            chooseScope DefaultSession ["-s"] (OptUser "@done")
                `shouldBe` Right SetServer

        it "setw @user routes a user option to the current window" $
            chooseScope DefaultWindow [] (OptUser "@x")
                `shouldBe` Right SetLocalWindow

    describe "new-window index placement" $ do
        let ws = Map.fromList [(100 :: Int, ())]
        it "numbers from the session-resolved base-index" $
            -- the base-index bug: with origin window 100 and base-index 200,
            -- the next window is 200, not 101 (upstream new-session-base-index.sh)
            placeWindow Nothing False 100 200 ws `shouldBe` 200
        it "falls to the next free slot when base-index itself is taken" $
            placeWindow Nothing False 100 100 ws `shouldBe` 101
        it "honors an explicit -t index verbatim" $
            placeWindow (Just 5) False 100 100 ws `shouldBe` 5
        it "-a places the window right after the current one" $
            placeWindow Nothing True 100 100 ws `shouldBe` 101
        it "a requested index that collides falls back to base-index" $
            placeWindow (Just 100) False 100 200 ws `shouldBe` 200

    describe "config-load burn-down (real ~/.tmux.conf)" $
        it "rejects exactly the not-yet-implemented options" $ do
            contents <- TIO.readFile "test/fixtures/tmux.conf"
            case parseConfig contents of
                Left err -> expectationFailure (T.unpack err)
                Right cmds ->
                    List.sort (loadSetErrors cmds)
                        `shouldBe` expectedUnimplemented

-- The living burn-down list, now empty: every option in the author's
-- real ~/.tmux.conf loads with behavior. M10's acceptance target.
expectedUnimplemented :: [Text]
expectedUnimplemented = []
