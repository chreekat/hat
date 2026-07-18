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
    , optionScopeClass, validateScope
    )
import Hat.Server
    (SetMode (..), setOption, chooseScope, SetScope (..), SetDefault (..))
import Hat.Server.Keys (KeyAction (..), PrefixState (..), routeKeys, tokenizeKeys)

-- Set an option on the defaults, failing the test if it is rejected.
setOK :: Text -> Text -> Options
setOK name value = either (error . T.unpack) id
    (setOption Assign defaultOptions name value)

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
                `shouldBe` Left "unimplemented option: no-such-option"

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

    -- Loud-failure audit: an option hat accepts must have an observable
    -- effect. Each case sets a NON-default value and confirms it reaches the
    -- field its consumer reads, so an accepted-but-ignored option (a silent
    -- no-op) fails here instead of lying to the user.
    describe "option effects (loud-failure audit)" $ do
        it "prefix rebinds which key arms the prefix table" $ do
            let km = Map.fromList
                    [ ("prefix", Map.singleton "c" [["new-window"]]) ]
                opts = setOK "prefix" "C-a"
                -- C-a (0x01) then c: the prefixed c runs its command.
                (_, acts) = routeKeys opts.prefix km Nothing NoPrefix
                    (tokenizeKeys "\x01\&c")
            opts.prefix `shouldBe` "C-a"
            acts `shouldBe` [RunCommands [["new-window"]]]

        it "prefix change means the old default key no longer arms" $ do
            let km = Map.fromList
                    [ ("prefix", Map.singleton "c" [["new-window"]]) ]
                opts = setOK "prefix" "C-a"
                -- C-b (0x02, the default) is now just a passthrough.
                (_, acts) = routeKeys opts.prefix km Nothing NoPrefix
                    (tokenizeKeys "\x02\&c")
            acts `shouldBe` [Passthrough "\x02\&c"]

        it "base-index changes the first window number" $
            (setOK "base-index" "1").baseIndex `shouldBe` 1

        it "pane-base-index changes the first pane number" $
            (setOK "pane-base-index" "1").paneBaseIndex `shouldBe` 1

        it "status-position moves the bar to the top" $
            (setOK "status-position" "top").statusPosition
                `shouldNotBe` defaultOptions.statusPosition

        it "mode-keys selects vi" $
            (setOK "mode-keys" "vi").modeKeys
                `shouldNotBe` defaultOptions.modeKeys

        it "history-limit sets the scrollback cap" $
            (setOK "history-limit" "1000").historyLimit `shouldBe` 1000

        it "default-terminal sets $TERM for new panes" $
            (setOK "default-terminal" "xterm-256color").defaultTerminal
                `shouldBe` "xterm-256color"

        it "word-separators drives copy-mode word motions" $
            (setOK "word-separators" " ").wordSeparators `shouldBe` " "

        it "status-left/right and their lengths take the set value" $ do
            (setOK "status-left" "L").statusLeft `shouldBe` "L"
            (setOK "status-left-length" "5").statusLeftLength `shouldBe` 5
            (setOK "status-right" "R").statusRight `shouldBe` "R"
            (setOK "status-right-length" "7").statusRightLength `shouldBe` 7

        it "window-status formats take the set value" $ do
            (setOK "window-status-format" "F").windowStatusFormat
                `shouldBe` "F"
            (setOK "window-status-current-format" "C").windowStatusCurrentFormat
                `shouldBe` "C"

        it "the status/border styles parse to a non-default style" $ do
            (setOK "status-style" "fg=red").statusStyle
                `shouldNotBe` defaultOptions.statusStyle
            (setOK "window-status-style" "fg=red").windowStatusStyle
                `shouldNotBe` defaultOptions.windowStatusStyle
            let wscs = setOK "window-status-current-style" "fg=red"
            wscs.windowStatusCurrentStyle
                `shouldNotBe` defaultOptions.windowStatusCurrentStyle
            (setOK "window-status-bell-style" "fg=red").windowStatusBellStyle
                `shouldNotBe` defaultOptions.windowStatusBellStyle
            (setOK "pane-border-style" "fg=red").paneBorderStyle
                `shouldNotBe` defaultOptions.paneBorderStyle
            (setOK "pane-active-border-style" "fg=blue").paneActiveBorderStyle
                `shouldNotBe` defaultOptions.paneActiveBorderStyle
            (setOK "mode-style" "fg=red").modeStyle
                `shouldNotBe` defaultOptions.modeStyle

        it "pane-border-lines and -indicators take the set value" $ do
            (setOK "pane-border-lines" "double").paneBorderLines
                `shouldNotBe` defaultOptions.paneBorderLines
            (setOK "pane-border-indicators" "both").paneBorderIndicators
                `shouldNotBe` defaultOptions.paneBorderIndicators

        it "set-titles toggles OSC-title emission" $
            (setOK "set-titles" "on").setTitles `shouldBe` True

        it "display-time changes the toast duration" $
            (setOK "display-time" "1500").displayTime `shouldBe` 1500

        it "focus-events toggles focus reporting" $
            (setOK "focus-events" "on").focusEvents `shouldBe` True

        it "aggressive-resize toggles the resize policy" $
            (setOK "aggressive-resize" "on").aggressiveResize `shouldBe` True

        it "monitor-activity toggles the activity flag" $
            (setOK "monitor-activity" "on").monitorActivity `shouldBe` True

        it "automatic-rename and its format take the set value" $ do
            (setOK "automatic-rename" "off").automaticRename `shouldBe` False
            (setOK "automatic-rename-format" "#{host}").automaticRenameFormat
                `shouldBe` "#{host}"

        it "update-environment sets the refreshed-on-attach var list" $
            (setOK "update-environment" "FOO BAR").updateEnvironment
                `shouldBe` ["FOO", "BAR"]

        it "main-pane-width/height size the main-* layout pane" $ do
            (setOK "main-pane-width" "120").mainPaneWidth `shouldBe` 120
            (setOK "main-pane-height" "40").mainPaneHeight `shouldBe` 40

        -- escape-time: only the value 0 has behavior (ESC disambiguation in
        -- Hat.Server.Keys is hardcoded to escape-time-0 semantics). A non-zero
        -- value is not implemented, so it must fail loud rather than be stored
        -- and silently ignored.
        it "escape-time 0 is accepted (the implemented value)" $
            setOption Assign defaultOptions "escape-time" "0"
                `shouldSatisfy` (== Right (defaultOptions { escapeTime = 0 }))

        it "a non-zero escape-time fails loud (unimplemented, not a no-op)" $
            setOption Assign defaultOptions "escape-time" "500"
                `shouldSatisfy` isLeft

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
