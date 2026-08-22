module Hat.Server.FormatSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (utc, utcToZonedTime)
import Test.Hspec

import Hat.Server.Format

env :: Map.Map Text Text
env = Map.fromList
    [ ("session_name", "work")
    , ("window_index", "3")
    , ("window_name", "vim")
    , ("window_flags", "*")
    , ("window_active_clients", "2")
    , ("empty", "")
    , ("zero", "0")
    ]

eval :: Text -> Text
eval = evaluate env (\cmd -> "OUT:" <> cmd)

-- Render through the real server seam. %t (tab) is time-independent, so the
-- epoch stands in for "now" without affecting the result.
render :: Text -> Text
render = renderFormat env resolver (utcToZonedTime utc (posixSecondsToUTCTime 0))
  where
    resolver cmd = maybe "" id (T.stripPrefix "echo " cmd)

renderZoned :: Map.Map Text Text -> Text -> Text
renderZoned e =
    renderFormat e (const "") (utcToZonedTime utc (posixSecondsToUTCTime 45000))

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

    it "compares strings with ==: and !=:" $ do
        eval "#{==:#{session_name},work}" `shouldBe` "1"
        eval "#{==:#{session_name},play}" `shouldBe` "0"
        eval "#{!=:a,b}" `shouldBe` "1"
        eval "#{!=:a,a}" `shouldBe` "0"

    it "compares percent-bearing values literally" $
        evaluate (Map.singleton "pane" "%5") (const "")
            "#{==:#{pane},%5}" `shouldBe` "1"

    it "matches globs with m:" $ do
        eval "#{m:client-*,client-5}" `shouldBe` "1"
        eval "#{m:client-*,work}" `shouldBe` "0"
        eval "#{m:w?rk,work}" `shouldBe` "1"

    it "renders a recent epoch with t/p as HH:MM" $
        renderZoned (Map.singleton "fire" "44100") "#{t/p:fire}"
            `shouldBe` "12:15"

    it "renders t/p empty for an absent key" $
        renderZoned Map.empty "#{t/p:fire}" `shouldBe` ""

    it "delegates #() to the shell resolver" $
        eval "a #(uptime -p) b" `shouldBe` "a OUT:uptime -p b"

    it "keeps strftime sequences untouched for the caller" $
        eval "%H:%M" `shouldBe` "%H:%M"

    it "strftimes literal runs but not expansion output" $
        -- %t is a literal tab; the % from echo must survive verbatim.
        render "#(echo -77%)·%t" `shouldBe` "-77%·\t"
