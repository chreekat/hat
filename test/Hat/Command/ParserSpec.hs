module Hat.Command.ParserSpec (spec) where

import Test.Hspec

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Command.Parser
import Hat.Command.Script
import Hat.Server.Environ (EnvVisibility (..))

parses :: Text -> [[Text]] -> Expectation
parses input expected = commands input `shouldBe` expected

-- | The commands of a script that parses whole, conditionals aside.
commands :: Text -> [[Text]]
commands input = case parseScript input of
    Script items Nothing -> [ cmd | (_, ScriptCommand cmd) <- items ]
    Script _ (Just err) -> error (show err)

spec :: Spec
spec = do
    it "splits words" $
        parses "new-window -n foo" [["new-window", "-n", "foo"]]

    it "handles single quotes literally" $
        parses "display-message 'hello  world'" [["display-message", "hello  world"]]

    it "handles double quotes" $
        parses "bind r source-file \"my file.conf\""
            [["bind", "r", "source-file", "my file.conf"]]

    it "unescapes inside double quotes" $
        parses "echo \"a \\\" b\"" [["echo", "a \" b"]]

    it "joins adjacent quoted and bare pieces into one word" $
        parses "set @r 'can'\\''t ; x' a\"b c\"'d'"
            [["set", "@r", "can't ; x", "ab cd"]]

    it "splits commands on semicolons" $
        parses "select-pane -L ; kill-pane"
            [["select-pane", "-L"], ["kill-pane"]]

    it "treats escaped semicolons as arguments" $
        parses "bind r source-file x \\; display ok"
            [["bind", "r", "source-file", "x", ";", "display", "ok"]]

    it "keeps brace blocks as single arguments" $
        parses "if-shell true { display yes } { display no }"
            [["if-shell", "true", " display yes ", " display no "]]

    it "handles nested braces" $
        parses "bind X { if-shell true { display a } }"
            [["bind", "X", " if-shell true { display a } "]]

    it "ignores comments" $
        commands "# a comment\nset -g base-index 1 # trailing\n"
            `shouldBe` [["set", "-g", "base-index", "1"]]

    it "starts a comment with a # that starts a word, and only then" $ do
        commands "set a x;#c\nset b x ;#c\n" `shouldBe` [["set", "a", "x"], ["set", "b", "x"]]
        commands "set a x#c \"y\"#c 'z'#c \\#c\n"
            `shouldBe` [["set", "a", "x#c", "y#c", "z#c", "#c"]]
        commands "bind x {#c\nset a\n}\n" `shouldBe` [["bind", "x", "\nset a\n"]]
        commands "# don't\nset a b\n" `shouldBe` [["set", "a", "b"]]
        commands "set a \\'#c\n" `shouldBe` [["set", "a", "'#c"]]

    it "drops a quoted line's leading blanks, and the comment lines a quote spans" $
        commands "set a \"x\n  # c\n\t#{b}\" 'y\n   #, z'\n"
            `shouldBe` [["set", "a", "x\n\n#{b}", "y\n#, z"]]

    it "parses a multi-line config with continuations" $
        commands "set -g status-left \\\n  'abc'\nbind v split-window -h\n"
            `shouldBe`
                [ ["set", "-g", "status-left", "abc"]
                , ["bind", "v", "split-window", "-h"]
                ]

    it "numbers each config command by the line it starts on, and an error by its" $ do
        parseScript "a\n\nb \\\n  c ; d\ne {\n f\n}\n"
            `shouldBe` Script
                [ (1, ScriptCommand ["a"]), (3, ScriptCommand ["b", "c"])
                , (3, ScriptCommand ["d"]), (5, ScriptCommand ["e", "\n f\n"]) ]
                Nothing
        fmap fst (parseScript "a\nb }\n").failure `shouldBe` Just 2

    it "skips blank lines" $
        commands "\n\nset -g x y\n\n" `shouldBe` [["set", "-g", "x", "y"]]

    describe "config conditionals and assignments" $ do
        it "keeps each directive with its line and its format whole" $
            parseScript (T.unlines
                [ "%if #{m:*a b*,#{host}}", "set a", "%elif x", "set b"
                , "%else", "set c", "%endif" ])
                `shouldBe` Script
                    [ (1, ScriptIf "#{m:*a b*,#{host}}"), (2, ScriptCommand ["set", "a"])
                    , (3, ScriptElif "x"), (4, ScriptCommand ["set", "b"])
                    , (5, ScriptElse), (6, ScriptCommand ["set", "c"]), (7, ScriptEndif) ]
                    Nothing
        it "takes a condition on one line among commands" $
            (parseScript "%if #{l:1} set a ; set b %else set c %endif ; set d\n").items
                `shouldBe` map (1,)
                    [ ScriptIf "#{l:1}", ScriptCommand ["set", "a"], ScriptCommand ["set", "b"]
                    , ScriptElse, ScriptCommand ["set", "c"], ScriptEndif, ScriptCommand ["set", "d"] ]
        it "reads a quoted condition as a word, and a comment after a format as a comment" $ do
            (parseScript "%if \"#{==:a,a}\"\n\n%endif\n").items
                `shouldBe` [(1, ScriptIf "#{==:a,a}"), (3, ScriptEndif)]
            (parseScript "%if #{l:1} # c\nx\n%endif #{y}\n").items
                `shouldBe` [(1, ScriptIf "#{l:1}"), (2, ScriptCommand ["x"]), (3, ScriptEndif)]
        it "takes a branch holding only a blank or comment line" $ do
            (parseScript "%if 1\n\n%endif\n").failure `shouldBe` Nothing
            (parseScript "%if 1\n# c\n%endif\n").failure `shouldBe` Nothing
        it "reads a word of only percent signs and digits as a word" $
            commands "%10 %%\n" `shouldBe` [["%10", "%%"]]
        it "reads assignments, alone, before a command, and hidden" $
            (parseScript "X=1\n'Y'=2 set a\n%hidden H=3\nset b X=1\n").items
                `shouldBe`
                    [ (1, ScriptAssign EnvVisible "X" "1"), (2, ScriptAssign EnvVisible "Y" "2")
                    , (2, ScriptCommand ["set", "a"]), (3, ScriptAssign EnvHidden "H" "3")
                    , (4, ScriptCommand ["set", "b", "X=1"]) ]
        it "fails as tmux's grammar does, at the line it does" $
            mapM_ (\(input, n) -> (input, (parseScript input).failure)
                    `shouldBe` (input, Just (n, "syntax error")))
                [ ("%if 1\n%endif\n", 2), ("%if 1\nset a\n", 3), ("%if 1\nset a", 2)
                , ("%endif\n", 1), ("%else\n", 1), ("%elif 1\n", 1), ("%foo\n", 1)
                , ("set a\ndisplay %foo\n", 2), ("%if 1 ; set a\n%endif\n", 1)
                , ("%if X=1\nset a\n%endif\n", 1), ("%if 1\nset a\n%else foo\n%endif\n", 3)
                , ("%if 1\nset a\n%endif ; set b\n", 3), ("%if #{l:1\nset a\n%endif\n", 1)
                , ("%if\nset a\n%endif\n", 1), ("%if 1 %endif\n", 1)
                , ("%if 1 { set a } %endif\n", 1), ("%hidden X=1 ; set a\n", 1)
                , ("set a ; %if 1\nset b\n%endif\n", 1)
                , ("%if 1\nset a\n%elif 0\n%else\nb\n%endif\n", 4)
                , ("X=1 Y=2 set a\n", 1) ]
        it "keeps what it read before an error" $ do
            parseScript "X=1 Y=2 set a\n"
                `shouldBe` Script [(1, ScriptAssign EnvVisible "X" "1")] (Just (1, "syntax error"))
            (parseScript "X=1\n%foo\n").items `shouldBe` [(1, ScriptAssign EnvVisible "X" "1")]

    it "reports parse errors" $
        (parseScript "echo 'unterminated").failure `shouldSatisfy` isJust

    -- argv splitter for the CLI wire path: each argv is one word except
    -- when it ends in an unescaped ';' (command boundary) or in "\;"
    -- (literal ';' at end). Semicolons in the middle stay literal.

    it "keeps a single argv token as one command" $
        parseArgv ["new", "-d", "-x40", "cat file.txt; printf '\\e[9H'; cat"]
            `shouldBe` [["new", "-d", "-x40", "cat file.txt; printf '\\e[9H'; cat"]]

    it "splits on argv ending with unescaped semicolon" $
        parseArgv ["start;", "has", "-tfoo"]
            `shouldBe` [["start"], ["has", "-tfoo"]]

    it "splits on a lone semicolon argv" $
        parseArgv ["new", "-d", ";", "has", "-tfoo"]
            `shouldBe` [["new", "-d"], ["has", "-tfoo"]]

    it "treats backslash-semicolon at end as literal ;" $
        parseArgv ["bind", "r", "display\\;", "ok"]
            `shouldBe` [["bind", "r", "display;", "ok"]]

    it "handles bundled flag ending in ; (e.g. -dsfoo\\;)" $
        parseArgv ["new", "-dsfoo;", "has", "-tfoo"]
            `shouldBe` [["new", "-dsfoo"], ["has", "-tfoo"]]

    it "joins brace blocks that span multiple config lines" $
        commands "bind -T tbl w {\n  send -X foo\n  send -X bar }\n"
            `shouldBe`
                [["bind", "-T", "tbl", "w",
                  "\n  send -X foo\n  send -X bar "]]
