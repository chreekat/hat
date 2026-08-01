module Hat.Term.EmulatorSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Vector as V
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

    -- SGR 2 (faint/dim) is what Claude Code paints its ghost suggestions with;
    -- the vendored libvterm is patched to carry it (bug 33/48).
    it "records the faint (SGR 2) attribute, and SGR 22 clears it" $ do
        e <- new80x24
        _ <- feedStr e "\ESC[2md\ESC[22mn"
        scr <- snapshot e
        (screenCell scr Pos { row = 0, col = 0 }).style.faint `shouldBe` True
        (screenCell scr Pos { row = 0, col = 1 }).style.faint `shouldBe` False

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

    it "survives resizing a restored emulator across a size mismatch" $ do
        -- Mirror adoptPane: the pane ran at the client's real size, but the
        -- fresh emulator is built at 24x80 and only resized on client attach.
        src <- newEmulator Size { rows = 50, cols = 200 } 1000
        forM_ [1 .. 80 :: Int] $ \i ->
            feedStr src (B8.pack ("line " ++ show i ++ "\r\n"))
        _ <- feedStr src "\ESC[?1049h\ESC[42malt content"
        scr <- snapshot src
        m <- modes src
        sblen <- scrollbackLength src
        sblines <- catMaybes <$> mapM (scrollbackLine src) [0 .. sblen - 1]
        dst <- newEmulator Size { rows = 24, cols = 80 } 1000
        _ <- feed dst (restoreBytes m defaultStyle scr)
        seedScrollback dst sblines
        resize dst Size { rows = 50, cols = 200 }
        scr2 <- snapshot dst
        scr2.size `shouldBe` Size { rows = 50, cols = 200 }

    it "tracks focus reporting (?1004)" $ do
        e <- new80x24
        m0 <- modes e
        m0.focusReport `shouldBe` False
        _ <- feedStr e "\ESC[?1004h"
        m1 <- modes e
        m1.focusReport `shouldBe` True
        _ <- feedStr e "\ESC[?1004l"
        m2 <- modes e
        m2.focusReport `shouldBe` False

    it "tracks color-scheme reporting (?2031)" $ do
        e <- new80x24
        m0 <- modes e
        m0.colorReport `shouldBe` False
        _ <- feedStr e "\ESC[?2031h"
        m1 <- modes e
        m1.colorReport `shouldBe` True
        _ <- feedStr e "\ESC[?2031l"
        m2 <- modes e
        m2.colorReport `shouldBe` False

    -- A reload adopts a running program into a fresh emulator, which starts
    -- with every mode off; replaying the app-set modes back into it is what
    -- keeps a subscription (e.g. ?2031 colour reports) alive across a reload.
    it "replays app-set modes into a fresh emulator" $ do
        src <- new80x24
        _ <- feedStr src "\ESC[?2031h\ESC[?1004h\ESC[?1002h"
        m <- modes src
        dst <- new80x24
        _ <- feed dst (modeReplayBytes m)
        m' <- modes dst
        (m'.colorReport, m'.focusReport, m'.mouse)
            `shouldBe` (True, True, MouseDrag)

    it "replays nothing for an emulator with no app-set modes" $ do
        e <- new80x24
        m <- modes e
        modeReplayBytes m `shouldBe` ""

    -- Reload restores a pane's live screen by replaying 'restoreBytes' into
    -- the adopted pane's fresh emulator; the visible grid must come back
    -- byte-identical, so a full-screen app survives restart-server intact.
    it "round-trips a styled screen through restoreBytes" $ do
        src <- new80x24
        _ <- feedStr src
            "line one\r\n\ESC[1;31mred bold\ESC[0m\r\n\ESC[44mon blue\ESC[0m\r\n\
            \wide \228\184\150\231\149\140 chars"  -- UTF-8 世界 (width 2 each)
        scr <- snapshot src
        m <- modes src
        dst <- new80x24
        _ <- feed dst (restoreBytes m defaultStyle scr)
        scr' <- snapshot dst
        scr'.cells `shouldBe` scr.cells

    -- The restart-server-mid-vim case: an alt-screen grid restores intact AND
    -- leaves ?1049 armed, so a later exit reverts to a clean primary buffer
    -- instead of stranding the app's frame.
    it "round-trips an alt-screen grid and keeps ?1049 armed for exit" $ do
        src <- new80x24
        _ <- feedStr src "\ESC[?1049h\ESC[42mvim buffer\r\nsecond line"
        scr <- snapshot src
        m <- modes src
        dst <- new80x24
        _ <- feed dst (restoreBytes m defaultStyle scr)
        restored <- snapshot dst
        restored.cells `shouldBe` scr.cells
        m' <- modes dst
        m'.altScreen `shouldBe` True
        _ <- feedStr dst "\ESC[?1049l"
        cleared <- snapshot dst
        rowText cleared 0 `shouldBe` ""

    -- Scrollback lives in the emulator (not libvterm), so reload restores it
    -- by capturing the lines and seeding them into the fresh emulator.
    it "round-trips scrollback through capture and seed" $ do
        src <- new80x24
        forM_ [1 .. 30 :: Int] $ \i ->
            feedStr src (B8.pack ("line " ++ show i ++ "\r\n"))
        len <- scrollbackLength src
        captured <- catMaybes <$> traverse (scrollbackLine src) [0 .. len - 1]
        dst <- new80x24
        seedScrollback dst captured
        len' <- scrollbackLength dst
        len' `shouldBe` len
        restored <- catMaybes <$> traverse (scrollbackLine dst) [0 .. len - 1]
        restored `shouldBe` captured

    it "surfaces a color-scheme query (CSI ? 996 n) as an event" $ do
        e <- new80x24
        evs <- feedStr e "\ESC[?996n"
        evs `shouldContain` [ColorSchemeQuery]

    it "does not answer the color-scheme query itself" $ do
        e <- new80x24
        evs <- feedStr e "\ESC[?996n"
        B.concat [bs | Output bs <- evs] `shouldBe` ""

    it "treats subscribing to ?2031 as asking for the current scheme" $ do
        e <- new80x24
        evs <- feedStr e "\ESC[?2031h"
        evs `shouldContain` [ColorSchemeQuery]

    it "answers queries in stream order (reply fences like a real terminal)" $ do
        -- Apps fence color probes with DA/CPR queries and match replies to
        -- probes by arrival order; a reply hoisted ahead of the preceding
        -- fence's answer gets attributed to the wrong probe.
        e <- new80x24
        evs <- feedStr e "\ESC[6n\ESC]11;?\a\ESC[6n"
        let isCpr ev = case ev of Output bs -> "R" `B8.isSuffixOf` bs; _ -> False
            slots = [ev | ev <- evs, isCpr ev || ev == OscColorQuery Background TermBel]
        map isCpr slots `shouldBe` [True, False, True]

    it "surfaces OSC 10/11 color queries as events, stripped from display" $ do
        e <- new80x24
        evs10 <- feedStr e "\ESC]10;?\a"
        evs10 `shouldContain` [OscColorQuery Foreground TermBel]
        evs11 <- feedStr e "\ESC]11;?\ESC\\"
        evs11 `shouldContain` [OscColorQuery Background TermSt]
        scr <- snapshot e
        rowText scr 0 `shouldBe` ""

    -- Bug 75f20c8a: an inner app's desktop notification (OSC 9 or OSC 777
    -- notify) must be surfaced verbatim so the server can forward it to the
    -- outer terminal, and never rendered onto the pane as text.
    it "surfaces OSC 9 and OSC 777 desktop notifications as events" $ do
        e <- new80x24
        evs9 <- feedStr e "A<\ESC]9;build finished\a>B"
        evs9 `shouldContain` [DesktopNotification "\ESC]9;build finished\a"]
        evs777 <- feedStr e "C<\ESC]777;notify;Title;Body\ESC\\>D"
        evs777 `shouldContain` [DesktopNotification "\ESC]777;notify;Title;Body\ESC\\"]
        scr <- snapshot e
        rowText scr 0 `shouldBe` "A<>BC<>D"

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

    it "surfaces a vterm prop it does not handle (DECSCUSR cursor shape)" $ do
        e <- new80x24
        -- CSI 2 SP q sets the cursor shape (VTERM_PROP_CURSORSHAPE), which
        -- hat does not act on; it must be surfaced, not silently dropped.
        evs <- feedStr e "\ESC[2 q"
        evs `shouldSatisfy` any (\case UnknownProp {} -> True; _ -> False)

    it "answers cursor position reports" $ do
        e <- new80x24
        evs <- feedStr e "\ESC[6n"
        let outs = [bs | Output bs <- evs]
        B.concat outs `shouldSatisfy` B8.isInfixOf "R"

    -- Bug b9: a tmux-aware app (claude) sees $TMUX set and wraps its OSC
    -- sequences in DCS tmux passthrough (ESC Ptmux; <ESC-doubled seq> ST).
    -- libvterm mis-parses the wrapper and spills the payload ("11;?",
    -- "9;4;0;") onto the screen as text. Match tmux's default
    -- (allow-passthrough off): consume the whole DCS silently.
    it "consumes DCS tmux passthrough instead of rendering its payload" $ do
        e <- new80x24
        _ <- feedStr e "A<\ESCPtmux;\ESC\ESC]11;?\a\ESC\\>B"
        _ <- feedStr e "C<\ESCPtmux;\ESC\ESC]9;4;0;\a\ESC\\>D"
        scr <- snapshot e
        rowText scr 0 `shouldBe` "A<>BC<>D"

    it "consumes a DCS passthrough split across feeds" $ do
        e <- new80x24
        _ <- feedStr e "A<\ESCPtmux;\ESC\ESC]9;4;"
        _ <- feedStr e "0;\a\ESC\\>B"
        scr <- snapshot e
        rowText scr 0 `shouldBe` "A<>B"

    -- Bug 75f20c8a: a tmux-aware app (claude, with $TMUX set) wraps its
    -- desktop notification in DCS passthrough. hat un-wraps it and forwards
    -- the notification, while still discarding other passthrough payloads
    -- (e.g. ConEmu 9;4 progress) and never rendering any of it.
    it "forwards a desktop notification wrapped in tmux passthrough" $ do
        e <- new80x24
        evs <- feedStr e "A<\ESCPtmux;\ESC\ESC]9;hello\a\ESC\\>B"
        evs `shouldContain` [DesktopNotification "\ESC]9;hello\a"]
        scr <- snapshot e
        rowText scr 0 `shouldBe` "A<>B"

    it "does not forward wrapped OSC 9;4 progress as a notification" $ do
        e <- new80x24
        evs <- feedStr e "\ESCPtmux;\ESC\ESC]9;4;0;\a\ESC\\"
        [() | DesktopNotification _ <- evs] `shouldBe` []

    -- Bug 0b: a passthrough payload hat does not answer (OSC 52 clipboard,
    -- OSC 12 cursor color, OSC 4 palette) must not vanish silently — it is
    -- surfaced as an UnhandledPassthrough event the reader logs. Otherwise
    -- allow-passthrough-off would discard it with no trace.
    it "surfaces an unhandled passthrough payload (OSC 52 clipboard)" $ do
        e <- new80x24
        evs <- feedStr e "\ESCPtmux;\ESC\ESC]52;c;aGVsbG8=\a\ESC\\"
        [bs | UnhandledPassthrough bs <- evs]
            `shouldBe` ["\ESC]52;c;aGVsbG8=\a"]

    -- A payload hat answers (OSC 11) is handled, not reported as unhandled,
    -- so the log stays quiet for the sequences hat honours.
    it "does not surface an answered passthrough payload as unhandled" $ do
        e <- new80x24
        evs <- feedStr e "\ESCPtmux;\ESC\ESC]11;?\a\ESC\\"
        [() | UnhandledPassthrough _ <- evs] `shouldBe` []

    -- Bug 16: a tmux-aware app (claude) wraps its OSC 10/11 color query in DCS
    -- passthrough to reach past the multiplexer. hat is the terminal that
    -- knows the OS scheme, so it must answer the wrapped query exactly as it
    -- answers a direct one — otherwise the app blocks until its slow OSC-11
    -- poll fallback, the ~2-3s theme lag. The wrapped query yields the same
    -- OscColorQuery event a direct query does.
    it "answers an OSC 11 color query wrapped in tmux passthrough" $ do
        e <- new80x24
        evs <- feedStr e "\ESCPtmux;\ESC\ESC]11;?\a\ESC\\"
        evs `shouldContain` [OscColorQuery Background TermBel]

    it "answers an OSC 10 color query wrapped in tmux passthrough" $ do
        e <- new80x24
        evs <- feedStr e "\ESCPtmux;\ESC\ESC]10;?\a\ESC\\"
        evs `shouldContain` [OscColorQuery Foreground TermBel]

    -- The DEC 2031 scheme query (CSI ? 996 n) is likewise answerable from the
    -- scheme hat tracks, so honour it inside passthrough too.
    it "answers a DEC 2031 scheme query wrapped in tmux passthrough" $ do
        e <- new80x24
        evs <- feedStr e "\ESCPtmux;\ESC\ESC[?996n\ESC\\"
        evs `shouldContain` [ColorSchemeQuery]

    -- Bug f: ghostty (like most terminals) ignores DECXCPR (CSI ? 6 n), but
    -- libvterm answers it with a DEC-private cursor report (CSI ? row;col R).
    -- Under a multiplexer that unexpected reply reaches the shell on the app's
    -- exit/resume, where the line editor spills its bare parameters as "9;4;0"
    -- garbage. Match the real terminal: keep plain CPR, stay silent on DECXCPR.
    it "answers plain CPR but not DECXCPR (CSI ? 6 n)" $ do
        e <- new80x24
        cpr  <- feedStr e "\ESC[6n"
        xcpr <- feedStr e "\ESC[?6n"
        let out evs = B.concat [bs | Output bs <- evs]
        out cpr `shouldSatisfy` B8.isInfixOf "R"   -- CPR still answered
        out xcpr `shouldBe` ""                      -- DECXCPR suppressed

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

    it "trims existing scrollback when the limit is lowered live" $ do
        e <- newEmulator Size { rows = 5, cols = 20 } 1000
        _ <- feedStr e (B8.intercalate "\r\n" ["line" <> B8.pack (show i) | i <- [1 :: Int .. 20]])
        scrollbackLength e `shouldReturn` 15
        setScrollbackLimit e 3
        scrollbackLength e `shouldReturn` 3
        -- the newest scrolled-off lines are kept
        Just line <- scrollbackLine e 0
        cellsText line `shouldBe` "line13"

    it "honors a raised limit for future scrollback growth" $ do
        e <- newEmulator Size { rows = 5, cols = 20 } 3
        _ <- feedStr e (B8.intercalate "\r\n" ["a" <> B8.pack (show i) | i <- [1 :: Int .. 10]])
        scrollbackLength e `shouldReturn` 3
        setScrollbackLimit e 100
        _ <- feedStr e (B8.intercalate "\r\n" ["b" <> B8.pack (show i) | i <- [1 :: Int .. 10]])
        scrollbackLength e `shouldReturn` 12

    it "survives resize both ways" $ do
        e <- new80x24
        _ <- feedStr e "stay"
        resize e Size { rows = 10, cols = 40 }
        resize e Size { rows = 50, cols = 200 }
        scr <- snapshot e
        scr.size `shouldBe` Size { rows = 50, cols = 200 }

    it "reflows history back onto the screen as it grows taller" $ do
        e <- newEmulator Size { rows = 3, cols = 20 } 1000
        _ <- feedStr e (B8.intercalate "\r\n" ["line" <> B8.pack (show i) | i <- [1 :: Int .. 6]])
        -- lines 1..3 have scrolled into scrollback; 4..6 are on screen
        resize e Size { rows = 6, cols = 20 }
        scr <- snapshot e
        rowText scr 0 `shouldBe` "line1"
        scrollbackLength e `shouldReturn` 0

    it "rewraps a long line instead of erasing it as it narrows" $ do
        e <- newEmulator Size { rows = 3, cols = 40 } 1000
        _ <- feedStr e (B8.pack (replicate 30 'x'))
        resize e Size { rows = 3, cols = 20 }
        scr <- snapshot e
        rowText scr 0 `shouldBe` T.replicate 20 "x"
        rowText scr 1 `shouldBe` T.replicate 10 "x"

    -- An extreme shrink of a pane holding a wrapped long line once abort()ed
    -- the whole server inside libvterm's screen_resize (bug 7); the emulator
    -- must clamp to a safe minimum so the C call never sees it.
    it "survives an extreme shrink of a wrapped long line" $ do
        e <- new80x24
        _ <- feedStr e (B8.intercalate "\r\n" [B8.pack ("line" <> show i) | i <- [1 :: Int .. 20]])
        _ <- feedStr e ("\r\n" <> B8.pack (replicate 300 'x'))
        resize e Size { rows = 2, cols = 2 }
        scr <- snapshot e
        scr.size.rows `shouldSatisfy` (>= 2)
        scr.size.cols `shouldSatisfy` (>= 2)

    describe "OSC title sequences (zsh preexec announcing the command)" $ do
        -- zsh/oh-my-zsh retitle the terminal on every command: the bare
        -- command word via OSC 1 (icon/tab title) and the whole line via
        -- OSC 2 (window title). None of that payload may reach the grid.
        let allText scr = T.concat [ screenRowText scr r | r <- [0 .. 23] ]
            titles evs = [ t | TitleChanged t <- evs ]
            noEcho scr = allText scr `shouldSatisfy` (not . T.isInfixOf "echo")

        it "swallows an OSC 2 window title (BEL-terminated)" $ do
            e <- new80x24
            evs <- feedStr e "\ESC]2;echo foo\afoo"
            scr <- snapshot e
            rowText scr 0 `shouldBe` "foo"
            noEcho scr
            titles evs `shouldBe` ["echo foo"]

        it "swallows an OSC 2 window title (ST-terminated)" $ do
            e <- new80x24
            evs <- feedStr e "\ESC]2;echo foo\ESC\\foo"
            scr <- snapshot e
            rowText scr 0 `shouldBe` "foo"
            noEcho scr
            titles evs `shouldBe` ["echo foo"]

        it "swallows an OSC 1 icon/tab title, the bare command word (BEL)" $ do
            e <- new80x24
            _ <- feedStr e "\ESC]1;echo\afoo"
            scr <- snapshot e
            rowText scr 0 `shouldBe` "foo"
            noEcho scr

        -- Bug c28: the OSC 1 icon name (VTERM_PROP_ICONNAME, prop 5/str) is a
        -- recognized emulator property, not an UnknownProp — it must not spam
        -- the logs, and the emulator records it.
        it "records an OSC 1 icon name without an UnknownProp event" $ do
            e <- new80x24
            evs <- feedStr e "\ESC]1;my-icon\a"
            evs `shouldSatisfy` all (\case UnknownProp {} -> False; _ -> True)
            icon <- iconName e
            icon `shouldBe` "my-icon"

        it "swallows an OSC 1 icon/tab title (ST)" $ do
            e <- new80x24
            _ <- feedStr e "\ESC]1;echo\ESC\\foo"
            scr <- snapshot e
            rowText scr 0 `shouldBe` "foo"
            noEcho scr

        it "swallows an OSC 0 icon+title (BEL)" $ do
            e <- new80x24
            evs <- feedStr e "\ESC]0;echo foo\afoo"
            scr <- snapshot e
            rowText scr 0 `shouldBe` "foo"
            noEcho scr
            titles evs `shouldBe` ["echo foo"]

        it "swallows the oh-my-zsh preexec pair before command output" $ do
            e <- new80x24
            _ <- feedStr e "\ESC]1;echo\a\ESC]2;echo foo\afoo"
            scr <- snapshot e
            rowText scr 0 `shouldBe` "foo"
            noEcho scr

        -- The pty tears output at arbitrary byte boundaries, so the title
        -- may split right after the ESC, mid-payload, or before the
        -- terminator; the scrubber's cross-chunk state must still swallow it.
        it "swallows an OSC 1 title torn at every split point" $ do
            let full = "\ESC]1;echo\afoo" :: B8.ByteString
            forM_ [1 .. B.length full - 1] $ \k -> do
                e <- new80x24
                _ <- feedStr e (B.take k full)
                _ <- feedStr e (B.drop k full)
                scr <- snapshot e
                allText scr `shouldSatisfy` (not . T.isInfixOf "echo")

    describe "screen/tmux ESC k title (hat advertises TERM=tmux-256color)" $ do
        -- Under a tmux/screen TERM, oh-my-zsh sets the title with the screen
        -- escape @ESC k <name> ST@ (name = the running command word) instead
        -- of an OSC. libvterm doesn't know it, so hat swallows it here and
        -- treats it as the pane title, exactly as it does OSC 0/2.
        let allText scr = T.concat [ screenRowText scr r | r <- [0 .. 23] ]

        it "swallows ESC k and records it as the pane title" $ do
            e <- new80x24
            evs <- feedStr e "\ESCkecho\ESC\\lol"
            scr <- snapshot e
            rowText scr 0 `shouldBe` "lol"
            allText scr `shouldSatisfy` (not . T.isInfixOf "echo")
            t <- title e
            t `shouldBe` "echo"
            [ x | TitleChanged x <- evs ] `shouldBe` ["echo"]

        it "swallows a BEL-terminated ESC k title" $ do
            e <- new80x24
            _ <- feedStr e "\ESCkecho\alol"
            scr <- snapshot e
            rowText scr 0 `shouldBe` "lol"
            allText scr `shouldSatisfy` (not . T.isInfixOf "echo")

        it "swallows an ESC k title torn at every split point" $ do
            let full = "\ESCkecho\ESC\\lol" :: B8.ByteString
            forM_ [1 .. B.length full - 1] $ \k -> do
                e <- new80x24
                _ <- feedStr e (B.take k full)
                _ <- feedStr e (B.drop k full)
                scr <- snapshot e
                allText scr `shouldSatisfy` (not . T.isInfixOf "echo")
                t <- title e
                t `shouldBe` "echo"

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
    cellsText cells = T.stripEnd (T.concat (map (.text) (V.toList cells)))
