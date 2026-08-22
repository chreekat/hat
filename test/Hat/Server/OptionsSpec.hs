-- | The config-load burn-down: loading the author's real @~/.tmux.conf@
-- must reject exactly the options whose behavior hat has not yet
-- implemented — never silently accept them. As each milestone lands its
-- behavior, its option leaves 'expectedUnimplemented' and this list
-- shrinks toward empty.
module Hat.Server.OptionsSpec (spec) where

import Data.Either (isLeft, lefts)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Test.Hspec

import Hat.Command.Parser (parseConfig)
import Hat.Model.Options
    ( Options (..), defaultOptions
    , OptionName (..), OptionValue (..), ScopeClass (..)
    , emptyDelta, singletonDelta, deleteDelta, mergeDeltas, resolveOptions
    , optionScopeClass, validateScope, resolveOptionName
    , lookupCommandAlias
    )
import Hat.Server
    ( SetMode (..), setOption, chooseScope, SetScope (..), SetDefault (..)
    , Insert (..), WindowPlacement (..), Replace (..)
    , applyShifts, listingLines, placeWindow, selectNamed
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

        it "parses status-interval as a number of seconds" $
            fmap (.statusInterval)
                (setOption Assign defaultOptions "status-interval" "5")
                `shouldBe` Right 5

        it "parses status off/on into a line count" $ do
            fmap (.statusLines)
                (setOption Assign defaultOptions "status" "off")
                `shouldBe` Right 0
            fmap (.statusLines)
                (setOption Assign defaultOptions "status" "on")
                `shouldBe` Right 1

        it "rejects a multi-line status bar loudly (unsupported)" $
            setOption Assign defaultOptions "status" "3"
                `shouldBe`
                    Left "status: multi-line status (2-5) not yet supported"

        it "rejects a junk status value" $
            setOption Assign defaultOptions "status" "yes"
                `shouldBe` Left "status: off, on, or 2-5"

        it "stores a valid cursor-colour and rejects junk" $ do
            fmap (.cursorColour)
                (setOption Assign defaultOptions "cursor-colour" "red")
                `shouldBe` Right "red"
            setOption Assign defaultOptions "cursor-colour" "zzz"
                `shouldSatisfy` isLeft

        it "appends to a string option with -a" $ do
            let set mode opts name v =
                    either (const opts) id (setOption mode opts name v)
                opts0 = set Assign defaultOptions "status-right" "a"
                opts1 = set Append opts0 "status-right" "b" :: Options
            (opts1.statusRight :: Text) `shouldBe` "ab"

        it "sets one command-alias index, keeping the other entries" $ do
            let r = setOption Assign defaultOptions
                        "command-alias[100]" "zoom=resize-pane -Z"
            fmap (Map.lookup 100 . (.commandAlias)) r
                `shouldBe` Right (Just "zoom=resize-pane -Z")
            fmap (Map.lookup 1 . (.commandAlias)) r
                `shouldBe` Right (Just "splitp=split-window")

        it "appends onto one array index with -a" $ do
            let set mode opts name v =
                    either (const opts) id (setOption mode opts name v)
                opts0 = set Assign defaultOptions
                    "command-alias[7]" "zoom=resize-pane"
                opts1 = set Append opts0 "command-alias[7]" " -Z" :: Options
            Map.lookup 7 opts1.commandAlias `shouldBe` Just "zoom=resize-pane -Z"

        it "whole-array assign replaces the array, split on commas" $
            fmap (.commandAlias)
                (setOption Assign defaultOptions "command-alias" "a=b,c=d")
                `shouldBe` Right (Map.fromList [(0, "a=b"), (1, "c=d")])

        it "whole-array append continues after the highest index" $
            -- the defaults occupy 0 and 1
            fmap (Map.lookup 2 . (.commandAlias))
                (setOption Append defaultOptions "command-alias" "x=y")
                `shouldBe` Right (Just "x=y")

        it "rejects an index on a non-array option" $
            setOption Assign defaultOptions "status-left[0]" "x"
                `shouldBe` Left "status-left is not an array option"

        it "rejects a malformed array index" $
            setOption Assign defaultOptions "command-alias[x]" "v"
                `shouldBe` Left "bad array index: command-alias[x]"

    describe "lookupCommandAlias" $ do
        it "resolves the first name= match in index order" $ do
            let opts = defaultOptions
                    { commandAlias =
                        Map.fromList [(2, "z=first"), (5, "z=second")] }
            lookupCommandAlias opts "z" `shouldBe` Just "first"

        it "resolves the default splitp alias" $
            lookupCommandAlias defaultOptions "splitp"
                `shouldBe` Just "split-window"

        it "answers Nothing for a name no entry aliases" $
            lookupCommandAlias defaultOptions "zoom" `shouldBe` Nothing

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

        it "deleting a scope's entry reveals the inherited value" $ do
            let g = singletonDelta OptStatusLeft (OVText "GLOBAL")
                s = singletonDelta OptStatusLeft (OVText "SESSION")
            (resolveOptions [deleteDelta OptStatusLeft s, g]).statusLeft
                `shouldBe` "GLOBAL"
            -- and at the top, the compiled default
            (resolveOptions [deleteDelta OptStatusLeft g]).statusLeft
                `shouldBe` defaultOptions.statusLeft

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

        it "set -p routes a user option to the pane table" $
            chooseScope DefaultSession ["-p"] (OptUser "@u")
                `shouldBe` Right SetLocalPane

        it "set -p routes cursor-colour (window-and-pane) to the pane" $
            chooseScope DefaultSession ["-p"] OptCursorColour
                `shouldBe` Right SetLocalPane

        it "bare set on cursor-colour infers the window scope" $
            chooseScope DefaultSession [] OptCursorColour
                `shouldBe` Right SetLocalWindow

        it "set -p on a non-pane option fails loud" $
            chooseScope DefaultSession ["-p"] OptStatusLeft
                `shouldSatisfy` isLeft

    describe "show-options listings" $ do
        it "lists own entries plain and inherited entries starred" $
            listingLines False
                (singletonDelta (OptUser "@own") (OVText "mine"))
                [singletonDelta OptStatusLeft (OVText "GLOBAL")]
                `shouldBe` ["@own mine", "status-left* GLOBAL"]

        it "an own entry shadows the parent's copy" $
            listingLines False
                (singletonDelta OptStatusLeft (OVText "SESSION"))
                [singletonDelta OptStatusLeft (OVText "GLOBAL")]
                `shouldBe` ["status-left SESSION"]

        it "quotes values that would not re-parse bare" $
            listingLines False
                (singletonDelta OptStatusLeft (OVText "[#{session_name}] "))
                []
                `shouldBe` ["status-left \"[#{session_name}] \""]

        it "prints one indexed line per array entry" $
            listingLines False
                (singletonDelta OptCommandAlias
                    (OVIndexed (Map.fromList [(0, "a=b"), (100, "z=y x")])))
                []
                `shouldBe` ["command-alias[0] a=b", "command-alias[100] \"z=y x\""]

    describe "new-window index placement" $ do
        let ws = Map.fromList [(100 :: Int, ())]
            run = Map.fromList [(i, ()) | i <- [0, 1, 2, 3, 9 :: Int]]
            plain n = WindowPlacement { shifts = [], replaced = Nothing, index = n }
        it "numbers from the session-resolved base-index" $
            -- the base-index bug: with origin window 100 and base-index 200,
            -- the next window is 200, not 101 (upstream new-session-base-index.sh)
            placeWindow Nothing InsertAt NoReplace 100 200 ws
                `shouldBe` Right (plain 200)
        it "falls to the next free slot when base-index itself is taken" $
            placeWindow Nothing InsertAt NoReplace 100 100 ws
                `shouldBe` Right (plain 101)
        it "honors a free explicit -t index verbatim" $
            placeWindow (Just 5) InsertAt NoReplace 100 100 ws
                `shouldBe` Right (plain 5)
        it "rejects an occupied -t index without -k (bug 06)" $
            placeWindow (Just 100) InsertAt NoReplace 100 200 ws
                `shouldBe` Left "create window failed: index 100 in use"
        it "-k replaces the occupant of the -t index (bug 06)" $
            placeWindow (Just 100) InsertAt Replace 100 200 ws
                `shouldBe` Right WindowPlacement
                    { shifts = [], replaced = Just 100, index = 100 }
        it "-a lands after the target, shuffling the following run up (bug 06)" $
            placeWindow (Just 1) InsertAfter NoReplace 0 0 run
                `shouldBe` Right WindowPlacement
                    { shifts = [(3, 4), (2, 3)], replaced = Nothing, index = 2 }
        it "-a with no target inserts after the current window" $
            placeWindow Nothing InsertAfter NoReplace 1 0 run
                `shouldBe` Right WindowPlacement
                    { shifts = [(3, 4), (2, 3)], replaced = Nothing, index = 2 }
        it "-a on an absent -t index takes that index directly" $
            placeWindow (Just 5) InsertAfter NoReplace 0 0 run
                `shouldBe` Right (plain 5)
        it "-b lands at the target, shuffling it and its followers up (bug 06)" $
            placeWindow (Just 0) InsertBefore NoReplace 0 0 run
                `shouldBe` Right WindowPlacement
                    { shifts = [(3, 4), (2, 3), (1, 2), (0, 1)]
                    , replaced = Nothing, index = 0 }
        it "applyShifts renumbers highest-first without clobbering" $
            applyShifts [(3, 4), (2, 3)]
                (Map.fromList [(0, 'a'), (2, 'c'), (3, 'd')])
                `shouldBe` Map.fromList [(0, 'a'), (3, 'c'), (4, 'd')]

    describe "new-window -S window reuse" $ do
        it "selects the lone window carrying the requested name (bug 06)" $
            selectNamed "editor" [(0, "shell"), (3, "editor")]
                `shouldBe` Right (Just 3)
        it "creates normally when no window carries the name" $
            selectNamed "editor" [(0, "shell")] `shouldBe` Right Nothing
        it "rejects an ambiguous name (bug 06)" $
            selectNamed "editor" [(0, "editor"), (3, "editor")]
                `shouldBe` Left "multiple windows named editor"

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
