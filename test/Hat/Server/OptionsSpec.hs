-- | The option registry and @set@\/@show@ machinery over it, plus the
-- config-load burn-down: loading the author's real @~/.tmux.conf@ must
-- reject exactly the options whose behavior hat has not yet implemented —
-- never silently accept them.
module Hat.Server.OptionsSpec (spec) where

import Data.Either (lefts)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Test.Hspec

import Hat.Command.Parser (parseConfig)
import Hat.Model.Options
import Hat.Server
    ( SetMode (..), setOption
    , Insert (..), WindowPlacement (..), Replace (..)
    , Renumbered (..), renumberSession
    , applyShifts, placeWindow, selectNamed
    )
import Hat.Server.Command.Option
    ( Display (..), Inheritance (..), ShowLine (..), ValuePresence (..)
    , renderShowLine )
import Hat.Server.OptionParse (parseScalar)

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

set :: SetMode -> Options -> Text -> Text -> Options
set mode opts name v = either (const opts) id (setOption mode opts name v)

idx :: Integer -> ArrayKey
idx = KeyIndex

spec :: Spec
spec = do
    describe "the registry" $ do
        it "decodes every registered default into the record" $
            -- forcing every field: a field no row binds would throw
            length (show defaultOptions) `shouldSatisfy` (> 0)

        it "registers every live option's default as a value it would accept" $
            [ d.name
            | d <- optionTable, isLive d, d.role == PlainOption
            , OVScalar t <- [d.defaultValue]
            , parseScalar d Assign Nothing t (Just t) /= Right t ]
                `shouldBe` []

        it "lists options in tmux's table order" $
            take 3 [ d.name | d <- optionTable ]
                `shouldBe` ["backspace", "buffer-limit", "command-alias"]

        it "places each option at tmux's scope" $ do
            (.scope) <$> lookupOptionDef "prefix" `shouldBe` Just SessionScope
            (.scope) <$> lookupOptionDef "mode-keys" `shouldBe` Just WindowScope
            (.scope) <$> lookupOptionDef "focus-events" `shouldBe` Just ServerScope
            (.scope) <$> lookupOptionDef "cursor-colour"
                `shouldBe` Just WindowPaneScope

    describe "setOption" $ do
        it "rejects an unknown option instead of silently storing it" $
            setOption Assign defaultOptions "no-such-option" "x"
                `shouldBe` Left "invalid option: no-such-option"

        it "refuses a registered option hat does not implement" $
            setOption Assign defaultOptions "synchronize-panes" "on"
                `shouldBe` Left "synchronize-panes: option not yet supported by hat"

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

        it "takes each of window-size's and remain-on-exit's choices" $ do
            let chosen n v = optionText n <$> setOption Assign defaultOptions n v
            mapM (chosen "window-size") ["largest", "smallest", "manual", "latest"]
                `shouldBe` Right ["largest", "smallest", "manual", "latest"]
            mapM (chosen "remain-on-exit") ["off", "on", "failed", "key"]
                `shouldBe` Right ["off", "on", "failed", "key"]

        it "sets a multi-line status bar" $
            fmap (.statusLines) (setOption Assign defaultOptions "status" "3")
                `shouldBe` Right 3

        it "rejects a junk status value" $
            setOption Assign defaultOptions "status" "yes"
                `shouldBe` Left "unknown value: yes"

        it "stores a valid cursor-colour and rejects junk" $ do
            fmap (.cursorColour)
                (setOption Assign defaultOptions "cursor-colour" "red")
                `shouldBe` Right "red"
            setOption Assign defaultOptions "cursor-colour" "zzz"
                `shouldBe` Left "invalid colour: zzz"

        it "accepts a historical -color spelling" $
            fmap (.cursorColour)
                (setOption Assign defaultOptions "cursor-color" "red")
                `shouldBe` Right "red"

        it "appends to a string option with -a" $ do
            let opts0 = set Assign defaultOptions "status-right" "a"
                opts1 = set Append opts0 "status-right" "b"
            optionText "status-right" opts1 `shouldBe` "ab"

        it "sets one command-alias key, keeping the other entries" $ do
            let r = setOption Assign defaultOptions
                        "command-alias[100]" "zoom=resize-pane -Z"
            fmap (Map.lookup (idx 100) . (.commandAlias)) r
                `shouldBe` Right (Just "zoom=resize-pane -Z")
            fmap (Map.lookup (idx 1) . (.commandAlias)) r
                `shouldBe` Right (Just "splitp=split-window")

        it "keeps a string key alongside numeric ones" $
            fmap (Map.keys . (.commandAlias))
                (setOption Assign defaultOptions "command-alias[zoom]" "z=x")
                `shouldBe` Right [idx 0, idx 1, KeyName "zoom"]

        it "appends onto one array key with -a" $ do
            let opts0 = set Assign defaultOptions
                    "command-alias[7]" "zoom=resize-pane"
                opts1 = set Append opts0 "command-alias[7]" " -Z"
            Map.lookup (idx 7) opts1.commandAlias
                `shouldBe` Just "zoom=resize-pane -Z"

        it "whole-array assign replaces the array, split on commas" $
            fmap (.commandAlias)
                (setOption Assign defaultOptions "command-alias" "a=b,c=d")
                `shouldBe` Right (Map.fromList [(idx 0, "a=b"), (idx 1, "c=d")])

        it "whole-array append fills the next free index" $
            fmap (Map.lookup (idx 2) . (.commandAlias))
                (setOption Append defaultOptions "command-alias" "x=y")
                `shouldBe` Right (Just "x=y")

        it "rejects a key on a non-array option" $
            setOption Assign defaultOptions "status-left[0]" "x"
                `shouldBe` Left "not an array: status-left[0]"

        it "rejects an empty array key" $
            setOption Assign defaultOptions "command-alias[]" "v"
                `shouldBe` Left "invalid option: command-alias[]"

    describe "lookupCommandAlias" $ do
        it "resolves the first name= match in key order" $ do
            let opts = defaultOptions
                    { commandAlias =
                        Map.fromList [(idx 2, "z=first"), (idx 5, "z=second")] }
            lookupCommandAlias opts "z" `shouldBe` Just "first"

        it "resolves the default splitp alias" $
            lookupCommandAlias defaultOptions "splitp"
                `shouldBe` Just "split-window"

        it "answers Nothing for a name no entry aliases" $
            lookupCommandAlias defaultOptions "zoom" `shouldBe` Nothing

    describe "matchOptionName" $ do
        it "resolves an exact name" $
            matchOptionName "status-left" `shouldBe` Right ("status-left", Nothing)

        it "resolves a unique prefix" $
            matchOptionName "history-l" `shouldBe` Right ("history-limit", Nothing)

        it "prefers an exact name over a longer collision" $
            matchOptionName "status-left-l"
                `shouldBe` Right ("status-left-length", Nothing)

        it "rejects an ambiguous prefix in tmux's words" $
            matchOptionName "status-l"
                `shouldBe` Left (MatchError "ambiguous option: status-l")

        it "rejects an unknown name in tmux's words" $
            matchOptionName "no-such-option"
                `shouldBe` Left (MatchError "invalid option: no-such-option")

        it "passes @-options through untouched" $
            matchOptionName "@foo" `shouldBe` Right ("@foo", Nothing)

        it "normalises a numeric key" $
            matchOptionName "status-format[01]"
                `shouldBe` Right ("status-format", Just (idx 1))

    describe "option-scope overlays" $ do
        it "resolves an empty chain to the defaults" $
            resolveOptions [] `shouldBe` defaultOptions

        it "an empty delta changes nothing" $
            resolveOptions [emptyDelta] `shouldBe` defaultOptions

        it "a single delta overrides only its own field" $ do
            let opts = resolveOptions [singletonDelta "prefix" (OVScalar "C-a")]
            opts.prefix `shouldBe` "C-a"
            opts.baseIndex `shouldBe` defaultOptions.baseIndex

        it "a more-specific delta wins field-by-field" $ do
            let global = mergeDeltas
                    [ singletonDelta "prefix" (OVScalar "C-a")
                    , singletonDelta "base-index" (OVScalar "0") ]
                session = singletonDelta "base-index" (OVScalar "1")
                opts = resolveOptions [session, global]
            opts.prefix `shouldBe` "C-a"
            opts.baseIndex `shouldBe` 1

        it "deleting a scope's entry reveals the inherited value" $ do
            let g = singletonDelta "status-left" (OVScalar "GLOBAL")
                s = singletonDelta "status-left" (OVScalar "SESSION")
            optionText "status-left" (resolveOptions [deleteDelta "status-left" s, g])
                `shouldBe` "GLOBAL"
            optionText "status-left" (resolveOptions [deleteDelta "status-left" g])
                `shouldBe` optionText "status-left" defaultOptions

        it "exposes any registered option through the typed accessors" $ do
            let opts = resolveOptions [singletonDelta "xterm-keys" (OVScalar "off")]
            optionFlag "xterm-keys" opts `shouldBe` False
            optionFlag "xterm-keys" defaultOptions `shouldBe` True
            optionNumber "display-time" defaultOptions `shouldBe` 3000
            optionArray "update-environment" defaultOptions
                `shouldBe` defaultOptions.updateEnvironment

    describe "options as format variables" $ do
        it "exposes live options, flags as 1/0 and arrays space-joined" $ do
            let fmts = optionFormats
                    (resolveOptions [singletonDelta "@x" (OVScalar "y")])
            Map.lookup "focus-events" fmts `shouldBe` Just "0"
            Map.lookup "status-left" fmts `shouldBe` Just "[#{session_name}] "
            Map.lookup "command-alias" fmts
                `shouldBe` Just "split-pane=split-window splitp=split-window"
            Map.lookup "@x" fmts `shouldBe` Just "y"
        it "leaves out options hat does not implement" $
            Map.member "synchronize-panes" (optionFormats defaultOptions)
                `shouldBe` False
        it "renders one scope's own entries the same way" $
            deltaFormats (mergeDeltas
                [ singletonDelta "monitor-bell" (OVScalar "off")
                , singletonDelta "@u" (OVScalar "v") ])
                `shouldBe` Map.fromList [("@u", "v"), ("monitor-bell", "0")]

    describe "show-options lines" $ do
        let line n v = ShowLine
                { option = n, definition = lookupOptionDef n, key = Nothing
                , value = v, inheritance = OwnValue, presence = HasValue
                , extraFormats = Map.empty }
        it "quotes string values as command arguments" $ do
            renderShowLine NameAndValue (line "@words" "two words")
                `shouldBe` "@words \"two words\""
            renderShowLine NameAndValue (line "status-left" "")
                `shouldBe` "status-left ''"
        it "prints non-string values bare" $
            renderShowLine NameAndValue (line "display-time" "1234")
                `shouldBe` "display-time 1234"
        it "marks an inherited value and names an array key" $
            renderShowLine NameAndValue
                (line "update-environment" "AAA")
                    { key = Just (idx 0), inheritance = InheritedValue }
                `shouldBe` "update-environment[0]* AAA"
        it "prints an empty array's bare name" $
            renderShowLine NameAndValue
                (line "update-environment" "") { presence = NoValue }
                `shouldBe` "update-environment"
        it "prints only the value under -v" $
            renderShowLine ValueOnly (line "@words" "two words")
                `shouldBe` "two words"

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

    describe "move-window -r renumbering" $ do
        let gappy = Map.fromList [(0, 'a'), (3, 'c'), (7, 'd')]
        it "packs the windows into consecutive indices, keeping their order" $
            (renumberSession 0 [] 0 gappy).windows
                `shouldBe` Map.fromList [(0, 'a'), (1, 'c'), (2, 'd')]
        it "numbers from base-index" $
            (renumberSession 0 [] 1 gappy).windows
                `shouldBe` Map.fromList [(1, 'a'), (2, 'c'), (3, 'd')]
        it "the current window follows to its new index" $
            (renumberSession 3 [] 0 gappy).current `shouldBe` 1
        it "the last-window stack follows its windows" $
            (renumberSession 3 [7, 0] 0 gappy).history `shouldBe` [2, 0]
        it "drops a history entry naming no window" $
            (renumberSession 0 [5, 3] 0 gappy).history `shouldBe` [1]

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
