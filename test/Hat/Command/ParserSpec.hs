module Hat.Command.ParserSpec (spec) where

import Test.Hspec

import Data.Text (Text)

import Hat.Command.Parser

parses :: Text -> [[Text]] -> Expectation
parses input expected = parseCommandLine input `shouldBe` Right expected

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
        parseConfig "# a comment\nset -g base-index 1 # trailing\n"
            `shouldBe` Right [["set", "-g", "base-index", "1"]]

    it "parses a multi-line config with continuations" $
        parseConfig "set -g status-left \\\n  'abc'\nbind v split-window -h\n"
            `shouldBe` Right
                [ ["set", "-g", "status-left", "abc"]
                , ["bind", "v", "split-window", "-h"]
                ]

    it "skips blank lines" $
        parseConfig "\n\nset -g x y\n\n" `shouldBe` Right [["set", "-g", "x", "y"]]

    it "reports parse errors" $
        parseCommandLine "echo 'unterminated" `shouldSatisfy` \case
            Left _ -> True
            Right _ -> False

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
        parseConfig "bind -T tbl w {\n  send -X foo\n  send -X bar }\n"
            `shouldBe` Right
                [["bind", "-T", "tbl", "w",
                  "\n  send -X foo\n  send -X bar "]]
