module Hat.Term.EmulatorSpec (spec) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (resize)

import Hat.Geometry
import Hat.Term.Cell
import Hat.Term.Emulator

new80x24 :: IO Emulator
new80x24 = newEmulator Size { rows = 24, cols = 80 } 1000

-- Visible text of a row, trailing blanks stripped.
rowText :: Screen -> Int -> T.Text
rowText scr r = T.stripEnd (screenRowText scr r)

feedStr :: Emulator -> B8.ByteString -> IO [Event]
feedStr = feed

newtype PlainLine = PlainLine String
    deriving (Show)

instance Arbitrary PlainLine where
    arbitrary = do
        n <- chooseInt (0, 80)
        PlainLine <$> vectorOf n (chooseEnum (' ', '~'))
    shrink (PlainLine s) = PlainLine <$> shrinkList (const []) s

spec :: Spec
spec = do
    it "puts plain text on the first row" $ do
        e <- new80x24
        _ <- feedStr e "hello"
        scr <- snapshot e
        rowText scr 0 `shouldBe` "hello"

    it "advances the cursor on CR LF" $ do
        e <- new80x24
        _ <- feedStr e "hi\r\n"
        scr <- snapshot e
        scr.cursor `shouldBe` Pos { row = 1, col = 0 }

    it "honors absolute cursor positioning" $ do
        e <- new80x24
        _ <- feedStr e "abc\ESC[1;1Hx"
        scr <- snapshot e
        rowText scr 0 `shouldBe` "xbc"

    it "records SGR styles on cells" $ do
        e <- new80x24
        _ <- feedStr e "\ESC[1;31mr"
        scr <- snapshot e
        let cell = screenCell scr Pos { row = 0, col = 0 }
        cell.style.fg `shouldBe` Indexed 1
        cell.style.bold `shouldBe` True

    it "tracks the alternate screen" $ do
        e <- new80x24
        m0 <- modes e
        m0.altScreen `shouldBe` False
        _ <- feedStr e "\ESC[?1049h"
        m1 <- modes e
        m1.altScreen `shouldBe` True
        _ <- feedStr e "\ESC[?1049l"
        m2 <- modes e
        m2.altScreen `shouldBe` False

    it "encodes cursor keys per application-cursor-keys mode" $ do
        e <- new80x24
        normal <- encodeKey e CursorUp
        normal `shouldBe` "\ESC[A"
        _ <- feedStr e "\ESC[?1h"          -- DECCKM on (application)
        app <- encodeKey e CursorUp
        app `shouldBe` "\ESCOA"
        _ <- feedStr e "\ESC[?1l"          -- DECCKM off (normal)
        back <- encodeKey e CursorUp
        back `shouldBe` "\ESC[A"

    it "reports title changes" $ do
        e <- new80x24
        evs <- feedStr e "\ESC]2;my title\BEL"
        evs `shouldSatisfy` elem (TitleChanged "my title")

    it "reports bells" $ do
        e <- new80x24
        evs <- feedStr e "\BEL"
        evs `shouldSatisfy` elem Bell

    it "answers cursor position reports" $ do
        e <- new80x24
        evs <- feedStr e "\ESC[6n"
        let outs = [bs | Output bs <- evs]
        B.concat outs `shouldSatisfy` B8.isInfixOf "R"

    it "gives wide characters width 2" $ do
        e <- new80x24
        _ <- feedStr e (B8.pack "\xe6\x97\xa5")  -- 日 in utf-8
        scr <- snapshot e
        let cell = screenCell scr Pos { row = 0, col = 0 }
        cell.width `shouldBe` 2
        cell.text `shouldBe` "日"

    it "pushes scrolled-off lines into scrollback" $ do
        e <- newEmulator Size { rows = 5, cols = 20 } 1000
        _ <- feedStr e (B8.intercalate "\r\n" ["line" <> B8.pack (show i) | i <- [1 :: Int .. 10]])
        n <- scrollbackLength e
        n `shouldBe` 5
        Just line <- scrollbackLine e 0  -- oldest
        cellsText line `shouldBe` "line1"

    it "caps scrollback at the limit" $ do
        e <- newEmulator Size { rows = 5, cols = 20 } 3
        _ <- feedStr e (B8.intercalate "\r\n" ["line" <> B8.pack (show i) | i <- [1 :: Int .. 20]])
        n <- scrollbackLength e
        n `shouldBe` 3

    it "survives resize both ways" $ do
        e <- new80x24
        _ <- feedStr e "stay"
        resize e Size { rows = 10, cols = 40 }
        resize e Size { rows = 50, cols = 200 }
        scr <- snapshot e
        scr.size `shouldBe` Size { rows = 50, cols = 200 }

    prop "plain ascii lands verbatim on row 0" $ \(PlainLine s) -> ioProperty $ do
        e <- new80x24
        _ <- feedStr e (B8.pack s)
        scr <- snapshot e
        pure $ rowText scr 0 === T.stripEnd (T.pack s)

    prop "arbitrary bytes never crash the emulator" $ \bytes -> ioProperty $ do
        e <- newEmulator Size { rows = 10, cols = 40 } 50
        _ <- feed e (B.pack bytes)
        _ <- snapshot e
        pure True
  where
    cellsText cells = T.stripEnd (T.concat (map (.text) cells))
