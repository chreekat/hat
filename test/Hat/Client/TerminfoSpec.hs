module Hat.Client.TerminfoSpec (spec) where

import Control.Concurrent.Async (forConcurrently)
import Test.Hspec

import Hat.Client.Terminfo

spec :: Spec
spec = do
    it "reads a terminal's capabilities as name=value" $ do
        r <- readTermCaps "xterm-256color"
        r `shouldSatisfy` either (const False)
            (\cs -> all (`elem` cs) ["am=1", "colors=256", "smcup=\ESC[?1049h\ESC[22;0;0t"])
    it "refuses a terminal terminfo does not know" $
        readTermCaps "hat-no-such-terminal"
            `shouldReturn` Left "missing or unsuitable terminal: hat-no-such-terminal"
    -- Regression guard: concurrent reads never see each other's terminal.
    it "reads each terminal's own capabilities while others are read at once" $ do
        alone <- mapM readTermCaps terms
        concat <$> forConcurrently [1 .. 64 :: Int] (const (mapM readTermCaps terms))
            `shouldReturn` concat (replicate 64 alone)
  where
    terms = ["xterm-256color", "hat-no-such-terminal", "screen"]
