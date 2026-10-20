module Hat.Command.ScriptSpec (spec) where

import Test.Hspec

import Data.Text (Text)
import Data.Text qualified as T

import Hat.Command.Parser (parseScript)
import Hat.Command.Script
import Hat.Server.Environ (EnvVisibility (..))

-- | What a run asked of its effects, in order.
data Effect = Asked Text | Assigned EnvVisibility Text Text
    deriving (Eq, Show)

-- | Run a script's items with each condition true when it reads 1, logging
-- every effect.
run :: [Text] -> ([Effect], [(Int, [Text])])
run ls = evalScript truth assign (parseScript (T.unlines ls)).items
  where
    truth fmt = ([Asked fmt], fmt == "1")
    assign vis n v = ([Assigned vis n v], ())

spec :: Spec
spec = do
    it "takes the first branch whose condition holds" $
        snd (run ["%if 0", "a", "%elif 0", "b", "%elif 1", "c", "%else", "d", "%endif", "e"])
            `shouldBe` [(6, ["c"]), (10, ["e"])]
    it "takes the else branch when no condition holds, also on one line" $
        snd (run ["%if 0 a %elif 0 b %else c ; d %endif ; e"])
            `shouldBe` [(1, ["c"]), (1, ["d"]), (1, ["e"])]
    it "takes nothing inside a branch not taken" $
        snd (run ["%if 0", "%if 1", "a", "%else", "b", "%endif", "%endif", "c"])
            `shouldBe` [(8, ["c"])]
    it "asks every condition, in order, interleaved with the assignments" $
        fst (run ["X=1", "%if 1", "Y=2", "%elif #{X}", "a", "%endif"])
            `shouldBe`
                [ Assigned EnvVisible "X" "1", Asked "1", Assigned EnvVisible "Y" "2"
                , Asked "#{X}" ]
    it "assigns by tmux's scope flags: an %elif's own condition, an %else's predecessor's negation" $ do
        fst (run ["%if 1", "a", "%elif 1", "X=1", "%endif"])
            `shouldBe` [Asked "1", Asked "1", Assigned EnvVisible "X" "1"]
        fst (run ["%if 1", "a", "%elif 0", "b", "%else", "%hidden Y=1", "%endif"])
            `shouldBe` [Asked "1", Asked "0", Assigned EnvHidden "Y" "1"]
        fst (run ["%if 0", "%if 1", "X=1", "%endif", "%endif"])
            `shouldBe` [Asked "0", Asked "1"]
