module Hat.Server.FormatSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (utc, utcToZonedTime)
import Test.Hspec

import Hat.Server.Format

env :: Map.Map Text Text
env = Map.fromList
    [ ("session_name", "work")
    , ("session_id", "$7")
    , ("window_index", "3")
    , ("window_name", "vim")
    , ("window_flags", "*")
    , ("window_active_clients", "2")
    , ("empty", "")
    , ("zero", "0")
    -- the format-modifiers.sh user options
    , ("@s", "abcdefghij")
    , ("@path", "/usr/local/bin/foo")
    , ("@name", "window-name")
    , ("@greek", "αβγ")
    , ("@cjk", "中文")
    , ("@emoji", "😀😀")
    , ("@host", "myhost")
    , ("@ts", "1000000000")
    , ("@sp", "a b$c")
    , ("@hash", "a#b")
    , ("@sq", "a'b")
    , ("@sub", "abABab")
    , ("@slash", "foo/bar foo/")
    , ("@nl", "a\nb")
    , ("@rec", "#{E:@rec}")
    , ("@age30", tshow (testNow - 30))
    , ("@age4000", tshow (testNow - 4000))
    , ("@age3000000", tshow (testNow - 3000000))
    , ("@future", tshow (testNow + 100000))
    ]
  where
    tshow = T.pack . show

-- A fixed "now" (2025-12-17 UTC) so time modifiers are deterministic.
testNow :: Integer
testNow = 1766000000

ctx :: FormatCtx
ctx = (formatCtx env (\cmd -> "OUT:" <> cmd) zt)
    { sessions =
        [ item [("session_name", "zeta"), ("session_id", "$0")]
        , item [("session_name", "alpha"), ("session_id", "$1")]
        , item [("session_name", "mike"), ("session_id", "$2")]
        ]
    , windows =
        [ item [("window_index", "0"), ("window_name", "charlie"), ("window_active", "1")]
        , item [("window_index", "1"), ("window_name", "alpha"), ("window_active", "0")]
        , item [("window_index", "2"), ("window_name", "bravo"), ("window_active", "0")]
        ]
    , panes =
        [ item [("pane_index", "0"), ("pane_id", "%0"), ("pane_active", "1")]
        , item [("pane_index", "1"), ("pane_id", "%1"), ("pane_active", "0")]
        , item [("pane_index", "2"), ("pane_id", "%2"), ("pane_active", "0")]
        ]
    , clients = [item [("client_name", "c0")], item [("client_name", "c1")]]
    , paneLines = ["$ echo Zebra_Marker_42   ", "Zebra_Marker_42", "", ""]
    }
  where
    zt = utcToZonedTime utc (posixSecondsToUTCTime (fromIntegral testNow))
    item = Map.fromList

eval :: Text -> Text
eval = evaluateCtx ctx

-- Render through the real server seam, with an echo-faking resolver.
render :: Text -> Text
render = renderFormatCtx ctx { shellRes = \cmd -> maybe "" id (T.stripPrefix "echo " cmd) }

-- One table row: format in, expected out.
cases :: String -> [(Text, Text)] -> Spec
cases name rows = it name $
    mapM_ (\(fmt, want) -> (fmt, eval fmt) `shouldBe` (fmt, want)) rows

spec :: Spec
spec = do
    it "passes plain text through" $
        eval "hello world" `shouldBe` "hello world"

    it "expands shorthand variables" $
        eval "#S #I:#W#F" `shouldBe` "work 3:vim*"

    it "expands braced variables" $
        eval "#{session_name}" `shouldBe` "work"

    it "renders unknown variables empty" $
        eval "x#{nope}y" `shouldBe` "xy"

    it "escapes ##" $
        eval "100##" `shouldBe` "100#"

    it "evaluates conditionals on non-empty" $ do
        eval "#{?session_name,yes,no}" `shouldBe` "yes"
        eval "#{?empty,yes,no}" `shouldBe` "no"
        eval "#{?zero,yes,no}" `shouldBe` "no"

    it "expands inside conditional branches" $
        eval "#{?window_flags,[#S],-}" `shouldBe` "[work]"

    it "supports nested conditions" $
        eval "#{?#{e|>:#{window_active_clients},1},shared,solo}"
            `shouldBe` "shared"

    it "compares with e| operators" $ do
        eval "#{e|>:2,1}" `shouldBe` "1"
        eval "#{e|>:1,2}" `shouldBe` "0"
        eval "#{e|==:5,5}" `shouldBe` "1"
        eval "#{e|+:2,3}" `shouldBe` "5"

    it "truncates with =N:" $
        eval "#{=2:session_name}" `shouldBe` "wo"

    it "delegates #() to the shell resolver" $
        eval "a #(uptime -p) b" `shouldBe` "a OUT:uptime -p b"

    it "keeps strftime sequences untouched for the caller" $
        eval "%H:%M" `shouldBe` "%H:%M"

    it "strftimes literal runs but not expansion output" $
        -- %t is a literal tab; the % from echo must survive verbatim.
        render "#(echo -77%)·%t" `shouldBe` "-77%·\t"

    cases "matches with m (glob, regex, flags)"
        [ ("#{m:*foo*,barfoobar}", "1")
        , ("#{m:*foo*,barbar}", "0")
        , ("#{m:abc,abc}", "1")
        , ("#{m/i:*FOO*,barfoobar}", "1")
        , ("#{m/i:*FOO*,barbar}", "0")
        , ("#{m/r:^[0-9]+$,12345}", "1")
        , ("#{m/r:^[0-9]+$,12a45}", "0")
        , ("#{m/ri:^ab+$,ABBB}", "1")
        , ("#{m/ri:^ab+$,ACCC}", "0")
        ]

    cases "matches fuzzily with m/z and m/p"
        [ ("#{m/z:foo,foobar}", "1")
        , ("#{m/z:xyz,foobar}", "0")
        , ("#{m/p:ac,abc}", "0,2")
        , ("#{m/p:xyz,abc}", "")
        , ("#{m/p:x,}", "")
        , ("#{m/z:x,}", "0")
        , ("#{m/z:,abc}", "1")
        , ("#{m/p:,abc}", "")
        , ("#{m/z:abc,a_b_c}", "1")
        , ("#{m/p:abc,a_b_c}", "0,2,4")
        , ("#{m/z:abc,acb}", "0")
        , ("#{m/p:abc,acb}", "")
        , ("#{m/z:dev bash,dev:1 bash}", "1")
        , ("#{m/p:dev bash,dev:1 bash}", "0,1,2,6,7,8,9")
        , ("#{m/z:dev bash,dev:1 sh}", "0")
        , ("#{m/p:dev bash,dev:1 sh}", "")
        , ("#{m/z:abc,ABC}", "1")
        , ("#{m/p:abc,ABC}", "0,1,2")
        , ("#{m/z:ABC,abc}", "0")
        , ("#{m/p:ABC,abc}", "")
        , ("#{m/z:'bash,dev bash}", "1")
        , ("#{m/p:'bash,dev bash}", "4,5,6,7")
        , ("#{m/z:'bash,b-a-s-h}", "0")
        , ("#{m/p:'bash,b-a-s-h}", "")
        , ("#{m/z:^dev,dev bash}", "1")
        , ("#{m/p:^dev,dev bash}", "0,1,2")
        , ("#{m/z:^dev,prod dev}", "0")
        , ("#{m/p:^dev,prod dev}", "")
        , ("#{m/z:bash$,dev bash}", "1")
        , ("#{m/p:bash$,dev bash}", "4,5,6,7")
        , ("#{m/z:bash$,bash dev}", "0")
        , ("#{m/p:bash$,bash dev}", "")
        , ("#{m/z:!ssh,dev bash}", "1")
        , ("#{m/z:!ssh,dev ssh}", "0")
        , ("#{m/z:!ssh,s_s_h}", "1")
        , ("#{m/z:dev !ssh,dev bash}", "1")
        , ("#{m/p:dev !ssh,dev bash}", "0,1,2")
        , ("#{m/z:dev !ssh,dev ssh}", "0")
        , ("#{m/p:dev !ssh,dev ssh}", "")
        , ("#{m/z:prod | dev,dev bash}", "1")
        , ("#{m/p:prod | dev,dev bash}", "0,1,2")
        , ("#{m/z:prod | dev,prod bash}", "1")
        , ("#{m/p:prod | dev,prod bash}", "0,1,2,3")
        , ("#{m/z:prod | dev,test bash}", "0")
        , ("#{m/p:prod | dev,test bash}", "")
        , ("#{m/z:dev,#[bold]dev#[default]}", "1")
        , ("#{m/p:dev,#[bold]dev#[default]}", "0,1,2")
        , ("#{m/p:dev,#[fg=red#,bg=blue]dev#[default]}", "0,1,2")
        , ("#{m/p:dev,#[bold]d#[default]e#[underscore]v#[default]}", "0,1,2")
        , ("#{m/p:bash,#[bold]dev#[default] bash}", "4,5,6,7")
        , ("#{m/z:é,café}", "1")
        , ("#{m/p:é,café}", "3")
        , ("#{m/z:é,É}", "0")
        , ("#{m/p:é,É}", "")
        , ("#{m/z:éx,éx}", "1")
        , ("#{m/p:éx,éx}", "0,1")
        , ("#{m/z:界,a界b}", "1")
        , ("#{m/p:界,a界b}", "1,2")
        ]

    cases "compares strings"
        [ ("#{==:#{@host},myhost}", "1")
        , ("#{==:#{@host},other}", "0")
        , ("#{!=:abc,xyz}", "1")
        , ("#{!=:abc,abc}", "0")
        , ("#{<:3,5}", "1")
        , ("#{<:5,3}", "0")
        , ("#{>:5,3}", "1")
        , ("#{>:3,5}", "0")
        , ("#{<=:5,5}", "1")
        , ("#{<=:6,5}", "0")
        , ("#{>=:5,5}", "1")
        , ("#{>=:4,5}", "0")
        ]

    cases "negates and canonicalises booleans"
        [ ("#{!:0}", "1")
        , ("#{!:1}", "0")
        , ("#{!!:}", "0")
        , ("#{!!:0}", "0")
        , ("#{!!:non-empty}", "1")
        ]

    cases "quotes with q"
        [ ("#{q:@sp}", "a\\ b\\$c")
        , ("#{q/s:@sp}", "'a b$c'")
        , ("#{q/s:@sq}", "'a'\\''b'")
        , ("#{q/s:@nl}", "'a\nb'")
        , ("#{q/e:@hash}", "a##b")
        , ("#{q/h:@hash}", "a##b")
        , ("#{q/a:@sp}", "\"a b\\$c\"")
        ]

    cases "tests name existence with N"
        [ ("#{N/s:zeta}", "1")
        , ("#{N/s:nosuchsession}", "0")
        , ("#{N/w:charlie}", "1")
        , ("#{N/w:nosuchwindow}", "0")
        , ("#{N:nosuchwindow}", "0")
        , ("#{N:bravo}", "1")
        ]

    cases "computes with e (integers)"
        [ ("#{e|+|:2,3}", "5")
        , ("#{e|-|:10,4}", "6")
        , ("#{e|-|:2,5}", "-3")
        , ("#{e|*|:6,7}", "42")
        , ("#{e|/|:20,4}", "5")
        , ("#{e|m|:7,3}", "1")
        , ("#{e|%%|:7,3}", "1")
        , ("#{e|==|:5,5}", "1")
        , ("#{e|!=|:5,5}", "0")
        , ("#{e|<|:2,5}", "1")
        , ("#{e|>|:9,2}", "1")
        , ("#{e|<=|:5,5}", "1")
        , ("#{e|>=|:5,5}", "1")
        ]

    cases "computes with e (floating point)"
        [ ("#{e|*|f|4:5.5,3}", "16.5000")
        , ("#{e|/|f|3:1,3}", "0.333")
        , ("#{e|/|f|2:10,3}", "3.33")
        , ("#{e|*|f:2.5,2}", "5.00")
        ]

    cases "survives malformed e expressions"
        [ ("#{e|/|:5,0}", "")
        , ("#{e|/|f:5,0}", "")
        , ("#{e|+|:notanumber,2}", "")
        , ("#{e|+|:2,notanumber}", "")
        , ("#{e|badop|:1,2}", "")
        , ("#{e|+|f|x:1,2}", "")
        , ("#{e|+|:1}", "")
        , ("#{e|+|f|2|extra:1,2}", "")
        ]

    cases "converts with a and repeats with R"
        [ ("#{a:98}", "b")
        , ("#{a:65}", "A")
        , ("#{a:200}", "")
        , ("#{a:notanumber}", "")
        , ("#{R:a,3}", "aaa")
        , ("#{R:ab,2}", "abab")
        , ("#{n:#{R:x,300}}", "300")
        , ("#{R:a,notanumber}", "")
        , ("#{R:a,0}", "")
        , ("#{R:中,3}", "中中中")
        ]

    cases "truncates and pads width-aware"
        [ ("#{=5:@s}", "abcde")
        , ("#{=-5:@s}", "fghij")
        , ("#{=:@s}", "abcdefghij")
        , ("#{=/x:@s}", "abcdefghij")
        , ("#{=/5/...:@s}", "abcde...")
        , ("#{=/5/...:@name}", "windo...")
        , ("#{=/20/...:@s}", "abcdefghij")
        , ("#{=3:@greek}", "αβγ")
        , ("#{=2:@greek}", "αβ")
        , ("#{=2:@cjk}", "中")
        , ("#{=1:@cjk}", "")
        , ("#{=/2/x:@cjk}", "中x")
        , ("#{=/1/x:@cjk}", "x")
        , ("#{=/1/中:@s}", "a中")
        , ("#{=2:@emoji}", "😀")
        , ("#{p12:@name}", "window-name ")
        , ("#{p-12:@name}", " window-name")
        , ("#{p3:@name}", "window-name")
        , ("#{p:@name}", "window-name")
        , ("#{p6:@cjk}", "中文  ")
        , ("#{p-6:@cjk}", "  中文")
        , ("#{p/x:@s}", "abcdefghij")
        ]

    cases "reports byte length and display width"
        [ ("#{n:@s}", "10")
        , ("#{w:@s}", "10")
        , ("#{n:@greek}", "6")
        , ("#{w:@greek}", "3")
        , ("#{n:@cjk}", "6")
        , ("#{w:@cjk}", "4")
        , ("#{n:@emoji}", "8")
        , ("#{w:@emoji}", "4")
        ]

    cases "takes basename and dirname"
        [ ("#{b:@path}", "foo")
        , ("#{d:@path}", "/usr/local/bin")
        ]

    cases "converts times with t"
        [ ("#{t:@ts}", "Sun Sep  9 01:46:40 2001")
        , ("#{t/p:@ts}", "Sep01")
        , ("#{t/d:@ts}", "766000000")
        , ("#{t/d:@future}", "-100000")
        , ("#{t/r:@future}", "")
        , ("#{t/r:@age30}", "30s")
        , ("#{t/r:@age4000}", "1h6m")
        , ("#{t/r:@age3000000}", "34d17h")
        , ("#{T:@ts}", "1000000000")
        , ("#{t/f/%Y:@ts}", "2001")
        , ("#{t/f/%Y-%m-%d:@ts}", "2001-09-09")
        , ("#{t/f/%H#:%M#:%S:@ts}", "01:46:40")
        , ("#{t/f/%Y#,end:@ts}", "2001,end")
        ]

    it "gives pretty and relative forms for every age band" $
        mapM_ (\age -> do
            let e' = Map.insert "@age" (T.pack (show (testNow - age))) env
                c' = ctx { vars = e' }
            evaluateCtx c' "#{t/r:@age}" `shouldNotBe` ""
            evaluateCtx c' "#{t/p:@age}" `shouldNotBe` "")
            [30, 300, 4000, 90000, 200000, 3000000, 40000000 :: Integer]

    cases "searches pane content with C"
        [ ("#{C:Zebra_Marker_42}", "1")
        , ("#{C:Absent_String_999}", "0")
        , ("#{C/r:Zebra_.*_42}", "1")
        , ("#{C/i:zebra_marker_42}", "1")
        ]

    cases "converts colours with c"
        [ ("#{c:red}", "800000")
        , ("#{c:colour4}", "000080")
        , ("#{c:#7f7f7f}", "7f7f7f")
        , ("#{c/f:red}", "\ESC[31m")
        , ("#{c/b:red}", "\ESC[41m")
        , ("#{c/b:colour4}", "\ESC[48;5;4m")
        , ("#{c/f:none}", "\ESC[0m")
        , ("#{c:notacolour}", "")
        , ("#{c/f:notacolour}", "")
        ]

    cases "chains and nests modifiers"
        [ ("#{=5:#{b:@path}}", "foo")
        , ("#{=2:#{b:@path}}", "fo")
        , ("#{p6:#{b:@path}}", "foo   ")
        , ("#{n:#{b:@path}}", "3")
        , ("#{=5:#{l:abcdefghij}}", "abcde")
        , ("#{=5:#{p10:#{b:@path}}}", "foo  ")
        , ("#{s/o/O/:#{b:@path}}", "fOO")
        ]

    it "caps unbounded recursion at the loop limit" $
        eval "#{E:@rec}" `shouldBe` ""

    cases "handles missing and malformed inputs"
        [ ("#{@undefined}", "")
        , ("#{=5:@undefined}", "")
        , ("#{b:@undefined}", "")
        , ("#{n:@undefined}", "0")
        , ("#{==:a}", "")
        , ("#{<:a}", "")
        , ("#{s/onlyone:@s}", "abcdefghij")
        , ("#{=/x:@s}", "abcdefghij")
        , ("#{I/c:RGB}", "")
        , ("#{I/f:overline}", "")
        , ("#{I:x}", "")
        ]

    cases "escapes inside modifier arguments"
        [ ("#{s/#,/-/:#{l:a,b,c}}", "a-b-c")
        , ("#{=/3/#,:@s}", "abc,")
        , ("#{=/3/#{l:>}:@s}", "abc>")
        ]

    cases "substitutes with s"
        [ ("#{s/z/X/:@s}", "abcdefghij")
        , ("#{s/[bd]/X/:@s}", "aXcXefghij")
        , ("#{s/A/X/i:@s}", "Xbcdefghij")
        , ("#{s/a(.)/\\1x/i:@sub}", "bxBxbx")
        , ("#{s/(.)(.)/\\2\\1/:@s}", "badcfehgji")
        , ("#{s|foo/|bar/|:@slash}", "bar/bar bar/")
        , ("#{s/^abc/ABC/:@s}", "ABCdefghij")
        , ("#{s/^(.)(.)/\\2\\1/:@s}", "bacdefghij")
        , ("#{s/^x*//:@s}", "abcdefghij")
        , ("#{s/^/X/:@s}", "Xabcdefghij")
        , ("#{s/^x*/X/:@s}", "Xabcdefghij")
        , ("#{s/$/X/:@s}", "abcdefghijX")
        , ("#{s/x*//:@s}", "abcdefghij")
        , ("#{s/x*/X/:@s}", "aXbXcXdXeXfXgXhXiXjX")
        , ("#{s/[/X/:@s}", "abcdefghij")
        , ("#{s/文/X/:@cjk}", "中X")
        ]

    cases "matches unicode in modifier arguments"
        [ ("#{m:*中*,#{@cjk}}", "1")
        ]

    cases "loops over sessions with S"
        [ ("#{S:#{session_name} }", "zeta alpha mike ")
        , ("#{S/i:#{session_name} }", "zeta alpha mike ")
        , ("#{S/n:#{session_name} }", "alpha mike zeta ")
        , ("#{S/nr:#{session_name} }", "zeta mike alpha ")
        , ("#{S/ir:#{session_name} }", "mike alpha zeta ")
        , ("#{S/t:x}", "xxx")
        , ("#{S/r:#{session_name} }", "mike alpha zeta ")
        ]

    cases "loops over windows with W"
        [ ("#{W:#{window_name} }", "charlie alpha bravo ")
        , ("#{W/n:#{window_name} }", "alpha bravo charlie ")
        , ("#{W/nr:#{window_name} }", "charlie bravo alpha ")
        , ("#{W/ir:#{window_index}}", "210")
        , ("#{W/i:#{window_name} }", "charlie alpha bravo ")
        , ("#{W/t:x}", "xxx")
        , ("#{W/r:#{window_name} }", "bravo alpha charlie ")
        ]

    cases "loops over panes with P"
        [ ("#{P:#{pane_index}}", "012")
        , ("#{P/r:#{pane_index}}", "210")
        , ("#{P/i:x}", "xxx")
        , ("#{P/i:#{pane_index}}", "012")
        , ("#{P/z:x}", "xxx")
        , ("#{P/n:x}", "xxx")
        , ("#{P/t:x}", "xxx")
        ]

    cases "loops over clients with L"
        [ ("#{L:x}", "xx")
        , ("#{L/i:x}", "xx")
        , ("#{L/n:x}", "xx")
        , ("#{L/t:x}", "xx")
        , ("#{L/nr:x}", "xx")
        , ("#{L/r:x}", "xx")
        ]

    it "uses the alternative body for the active loop item" $
        eval "#{W:#{window_name} ,[#{window_name}] }"
            `shouldBe` "[charlie] alpha bravo "

    it "fails loudly on unimplemented modifiers" $
        eval "#{A/2:a,b}" `shouldSatisfy` T.isInfixOf "unimplemented"
