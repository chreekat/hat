module Hat.Term.GoldenSpec (spec) where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Test.Hspec

import Hat.Geometry
import Hat.Term.Emulator qualified as Emu

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
