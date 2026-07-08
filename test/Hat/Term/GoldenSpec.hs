module Hat.Term.GoldenSpec (spec) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Hspec

import Hat.Geometry
import qualified Hat.Term.Emulator as Emu

-- Fixtures are captured from real programs by @cabal run gen-fixtures@.
golden :: String -> Spec
golden name = it (name <> " renders as blessed") $ do
    raw <- B.readFile ("test/fixtures/" <> name <> ".raw")
    expected <- TIO.readFile ("test/fixtures/" <> name <> ".expected")
    emu <- Emu.newEmulator Size { rows = 24, cols = 80 } 1000
    _ <- Emu.feed emu raw
    scr <- Emu.snapshot emu
    let rendered = T.unlines
            [T.stripEnd (Emu.screenRowText scr r) | r <- [0 .. 23]]
    rendered `shouldBe` expected

spec :: Spec
spec = do
    golden "vim-readme"
