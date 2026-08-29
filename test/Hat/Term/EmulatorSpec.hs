module Hat.Term.EmulatorSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.Mem.StableName (makeStableName)
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
    it "interns equal cells to one shared heap object (peekShimCell)" $ do
        e <- new80x24
        _ <- feedStr e "hi"
        scr <- snapshot e
        let blanks =
                [ c | row <- V.toList scr.cells, c <- V.toList row, c == blankCell ]
        case blanks of
            (a : b : _) -> do
                sa <- makeStableName a
                sb <- makeStableName b
                (sa == sb) `shouldBe` True
            _ -> expectationFailure "expected at least two blank cells"

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
    -- libghostty carries it natively (it was bug 33/48 on the old libvterm).
    it "records the faint (SGR 2) attribute, and SGR 22 clears it" $ do
        e <- new80x24
        _ <- feedStr e "\ESC[2md\ESC[22mn"
        scr <- snapshot e
        (screenCell scr Pos { row = 0, col = 0 }).style.faint `shouldBe` True
        (screenCell scr Pos { row = 0, col = 1 }).style.faint `shouldBe` False

    -- capture-pane -J joins on these flags (bug d2).
    it "reports soft-wrap on screen and scrollback rows" $ do
        e <- new80x24
        _ <- feedStr e (B8.replicate 100 'x')
        screenRowWrapped e 0 `shouldReturn` True
        screenRowWrapped e 1 `shouldReturn` False
        _ <- feedStr e (B8.concat (replicate 30 "\r\n"))
        scrollbackLineWrapped e 0 `shouldReturn` True
        scrollbackLineWrapped e 1 `shouldReturn` False

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
        seedScrollback dst sblines
        _ <- feed dst (restoreBytes m defaultStyle scr)
        resize dst Size { rows = 50, cols = 200 }
        scr2 <- snapshot dst
        scr2.size `shouldBe` Size { rows = 50, cols = 200 }

    -- bug 6: less draws its help on the alternate screen; on exit (?1049l) the
    -- primary buffer must come back, and a later zoom (resize) must not bleed
    -- the alt-screen content or leave a stale/garbage row.
    it "reverts to the primary buffer on alt-screen exit, and a resize stays clean" $ do
        e <- newEmulator Size { rows = 6, cols = 20 } 1000
        _ <- feedStr e "primary prompt$ "
        _ <- feedStr e "\ESC[?1049h"          -- less enters the alt screen
        _ <- feedStr e "\ESC[31mHELP SCREEN\ESC[0m\r\nmore help"
        _ <- feedStr e "\ESC[?1049l"          -- less exits: primary returns
        resize e Size { rows = 12, cols = 40 }  -- zoom the pane
        scr <- snapshot e
        let allText = T.concat [ screenRowText scr r | r <- [0 .. 11] ]
        -- no alt-screen content bled through
        allText `shouldNotSatisfy` T.isInfixOf "HELP"
        allText `shouldNotSatisfy` T.isInfixOf "more help"
        -- the primary line survived
        rowText scr 0 `shouldBe` "primary prompt$"

    -- bug: a pane with shell scrollback runs vim on the alt screen, then Ctrl-Z
    -- suspends it (?1049l restores the primary). A later zoom grows the pane and
    -- pulls scrollback back up -- that revealed area must be the shell's history,
    -- never the file vim was showing on the alt screen.
    it "keeps alt-screen content out of scrollback revealed by a zoom" $ do
        e <- newEmulator Size { rows = 6, cols = 20 } 1000
        forM_ [1 .. 20 :: Int] $ \i ->
            feedStr e (B8.pack ("shell line " ++ show i ++ "\r\n"))
        _ <- feedStr e "prompt$ vim file\r\n"
        _ <- feedStr e "\ESC[?1049h"          -- vim opens on the alt screen
        forM_ [1 .. 6 :: Int] $ \i ->
            feedStr e (B8.pack ("FILEDATA line " ++ show i ++ "\r\n"))
        _ <- feedStr e "\ESC[?1049l"          -- Ctrl-Z: primary returns
        resize e Size { rows = 24, cols = 40 }  -- zoom: grows, reveals scrollback
        scr <- snapshot e
        let allText = T.concat [ screenRowText scr r | r <- [0 .. 23] ]
        allText `shouldNotSatisfy` T.isInfixOf "FILEDATA"

    -- bug: vim on the alt screen, zoom (grow) then unzoom (shrink) while it is
    -- still up, then Ctrl-Z (?1049l) suspends it, then zoom again. A shrink of
    -- the alt screen must not push its rows into scrollback -- if it does, the
    -- later grow of the primary pops them back and the file reappears.
    it "keeps alt-screen rows out of scrollback across a zoom/unzoom cycle" $ do
        e <- newEmulator Size { rows = 6, cols = 20 } 1000
        forM_ [1 .. 20 :: Int] $ \i ->
            feedStr e (B8.pack ("shell line " ++ show i ++ "\r\n"))
        _ <- feedStr e "prompt$ vim file\r\n"
        _ <- feedStr e "\ESC[?1049h"          -- vim opens on the alt screen
        forM_ [1 .. 6 :: Int] $ \i ->
            feedStr e (B8.pack ("FILEDATA line " ++ show i ++ "\r\n"))
        resize e Size { rows = 24, cols = 40 }  -- zoom while vim runs
        resize e Size { rows = 6, cols = 20 }   -- unzoom while vim runs
        _ <- feedStr e "\ESC[?1049l"          -- Ctrl-Z: primary returns
        resize e Size { rows = 24, cols = 40 }  -- zoom: reveals scrollback
        scr <- snapshot e
        sblen <- scrollbackLength e
        sblines <- catMaybes <$> mapM (scrollbackLine e) [0 .. sblen - 1]
        let onScreen = T.concat [ screenRowText scr r | r <- [0 .. 23] ]
            inSb = T.concat (map cellsText sblines)
        inSb `shouldNotSatisfy` T.isInfixOf "FILEDATA"
        onScreen `shouldNotSatisfy` T.isInfixOf "FILEDATA"

    -- bug: a full-screen app (vim in light mode) sets a background pen on the
    -- alt screen. Zooming then unzooming it resizes the primary buffer behind
    -- it; libvterm erases the reflowed cells with the live pen, so after ?1049l
    -- the shell's restored screen wears the app's background on its blank cells.
    it "keeps the app's background pen off the primary screen across a resize" $ do
        e <- newEmulator Size { rows = 4, cols = 20 } 1000
        _ <- feedStr e "diff line one\r\ndiff line two"
        _ <- feedStr e "\ESC[?1049h"           -- vim opens on the alt screen
        _ <- feedStr e "\ESC[44m\ESC[2J"        -- light-mode bg, clear (BCE)
        resize e Size { rows = 12, cols = 60 }   -- zoom
        resize e Size { rows = 4, cols = 20 }    -- unzoom
        _ <- feedStr e "\ESC[?1049l"           -- Ctrl-Z: primary returns
        scr <- snapshot e
        rowText scr 0 `shouldBe` "diff line one"
        forM_ [ Pos { row = r, col = c } | r <- [0 .. 3], c <- [0 .. 19] ] $ \p ->
            (screenCell scr p).style.bg `shouldBe` DefaultColor

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

    -- libghostty owns scrollback, so reload restores it by capturing the lines
    -- and re-seeding them (byte-replay) into the fresh emulator.
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

    -- bug: adoptPane restores a primary-screen pane by both painting the live
    -- grid and seeding scrollback into one fresh emulator. The live grid must
    -- survive the seed -- a shell that never repaints on attach would otherwise
    -- come back to an empty viewport.
    it "keeps the live grid after seeding scrollback (adoptPane order)" $ do
        src <- new80x24
        forM_ [1 .. 40 :: Int] $ \i ->
            feedStr src (B8.pack ("line " ++ show i ++ "\r\n"))
        _ <- feedStr src "prompt$ "
        scr <- snapshot src
        m <- modes src
        len <- scrollbackLength src
        sblines <- catMaybes <$> mapM (scrollbackLine src) [0 .. len - 1]
        dst <- new80x24
        -- adoptPane order: seed scrollback under the live grid, then paint it.
        seedScrollback dst sblines
        _ <- feed dst (restoreBytes m defaultStyle scr)
        restored <- snapshot dst
        restored.cells `shouldBe` scr.cells

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

    -- bug 64: what a pane receives for a modified key depends on the key
    -- protocol its app turned on, and an app that turned none on must never
    -- see an extended-key sequence.
    describe "key protocol encoding" $ do
        let cSs   = KeyPress { code = 115, mods = 5 }
            cEnter = KeyPress { code = 13, mods = 5 }
            sEnter = KeyPress { code = 13, mods = 2 }
            mX    = KeyPress { code = 120, mods = 3 }
            cSpace = KeyPress { code = 32, mods = 5 }
            enc e kp = encodeKeyPress e kp

        it "sends legacy bytes to a pane that turned no protocol on" $ do
            e <- new80x24
            enc e cSs `shouldReturn` Just "\x13"
            enc e mX `shouldReturn` Just "\ESCx"
            enc e cSpace `shouldReturn` Just "\NUL"
            -- No legacy encoding at all: the modifiers drop rather than
            -- escaping into an extended sequence.
            enc e cEnter `shouldReturn` Just "\r"
            enc e sEnter `shouldReturn` Just "\r"

        it "spells every modified key as modifyOtherKeys once the app asked" $ do
            e <- new80x24
            _ <- feedStr e "\ESC[>4;2m"
            enc e cSs `shouldReturn` Just "\ESC[27;5;115~"
            enc e cEnter `shouldReturn` Just "\ESC[27;5;13~"
            enc e mX `shouldReturn` Just "\ESC[27;3;120~"

        it "spells modified keys as CSI-u once the app pushed kitty flags" $ do
            e <- new80x24
            _ <- feedStr e "\ESC[>1u"
            enc e cSs `shouldReturn` Just "\ESC[115;5u"
            enc e cEnter `shouldReturn` Just "\ESC[13;5u"

        it "returns to legacy bytes when the app turns the protocol off" $ do
            e <- new80x24
            _ <- feedStr e "\ESC[>4;2m"
            _ <- feedStr e "\ESC[>4;0m"
            enc e cSs `shouldReturn` Just "\x13"
            enc e cEnter `shouldReturn` Just "\r"

        it "reads back the protocols an app turned on" $ do
            e <- new80x24
            keyModes e `shouldReturn` KeyModes False 0
            _ <- feedStr e "\ESC[>4;2m"
            keyModes e `shouldReturn` KeyModes True 0
            _ <- feedStr e "\ESC[>5u"
            keyModes e `shouldReturn` KeyModes False 5

        -- The reload contract: replaying the captured protocols into a fresh
        -- emulator encodes keys as the surviving program still expects.
        it "re-arms a captured protocol in a fresh emulator" $
            forM_ [("\ESC[>4;2m", Just "\ESC[27;5;115~"), ("\ESC[>1u", Just "\ESC[115;5u")]
                $ \(enable, expected) -> do
                    live <- new80x24
                    _ <- feedStr live enable
                    km <- keyModes live
                    fresh <- new80x24
                    _ <- feed fresh (keyModeReplayBytes km)
                    keyModes fresh `shouldReturn` km
                    enc fresh cSs `shouldReturn` expected

    it "reports title changes" $ do
        e <- new80x24
        evs <- feedStr e "\ESC]2;my title\BEL"
        evs `shouldSatisfy` elem (TitleChanged "my title")

    it "reports bells" $ do
        e <- new80x24
        evs <- feedStr e "\BEL"
        evs `shouldSatisfy` elem Bell

    it "consumes DECSCUSR (cursor shape) without rendering it" $ do
        e <- new80x24
        -- CSI 2 SP q sets the cursor shape. libghostty handles it internally,
        -- so unlike libvterm it raises no UnknownProp; the sequence is consumed,
        -- not rendered.
        evs <- feedStr e "\ESC[2 qX"
        scr <- snapshot e
        rowText scr 0 `shouldBe` "X"
        evs `shouldSatisfy` all (\case UnknownProp {} -> False; _ -> True)

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

    -- Bug f: plain CPR is answered, DECXCPR (CSI ? 6 n) is not — a
    -- DEC-private cursor report reaches the shell as visible garbage. Nothing
    -- filters the reply any more, so this pins libghostty's own silence.
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
        cell.content `shouldBe` Glyph '日' [] Wide

    it "keeps combining marks with their base character's cell" $ do
        e <- new80x24
        _ <- feedStr e (B8.pack "e\xcc\x81x")  -- e, U+0301, x in utf-8
        scr <- snapshot e
        let cell = screenCell scr Pos { row = 0, col = 0 }
        cell.content `shouldBe` Glyph 'e' ['\x0301'] Narrow
        baseChar (screenCell scr Pos { row = 0, col = 1 }) `shouldBe` 'x'

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

    -- The row limit is a non-destructive read-side cap over libghostty's
    -- byte-bounded history: a lowered limit only hides the oldest rows, so
    -- raising it again reveals every row still retained (14 here, where a
    -- destructive per-row cap would have discarded a4/a5 and reported 12).
    it "honors a raised limit for future scrollback growth" $ do
        e <- newEmulator Size { rows = 5, cols = 20 } 3
        _ <- feedStr e (B8.intercalate "\r\n" ["a" <> B8.pack (show i) | i <- [1 :: Int .. 10]])
        scrollbackLength e `shouldReturn` 3
        setScrollbackLimit e 100
        _ <- feedStr e (B8.intercalate "\r\n" ["b" <> B8.pack (show i) | i <- [1 :: Int .. 10]])
        scrollbackLength e `shouldReturn` 14

    -- bug 5b: DECSTBM/DECOM/IND/RI scroll cases must match tmux row-for-row.
    describe "scroll regions (DECSTBM)" $ do
        let capture e n = do
                scr <- snapshot e
                pure [rowText scr r | r <- [0 .. n - 1]]

        it "writes at the region origin under DECOM" $ do
            e <- newEmulator Size { rows = 4, cols = 6 } 1000
            _ <- feedStr e "111111\r\n222222\r\n333333\r\n444444"
            _ <- feedStr e "\ESC[2;3r\ESC[?6h\ESC[1;1HAA\ESC[?6l\ESC[r"
            capture e 4 `shouldReturn` ["111111", "AA2222", "333333", "444444"]

        it "scrolls the region up on LF at the bottom margin" $ do
            e <- newEmulator Size { rows = 4, cols = 5 } 1000
            _ <- feedStr e "11111\r\n22222\r\n33333\r\n44444"
            _ <- feedStr e "\ESC[2;3r\ESC[3;1HAAAAA\r\nBBBBB\ESC[r"
            capture e 4 `shouldReturn` ["11111", "AAAAA", "BBBBB", "44444"]

        it "scrolls the region down on SD (CSI T)" $ do
            e <- newEmulator Size { rows = 4, cols = 5 } 1000
            _ <- feedStr e "11111\r\n22222\r\n33333\r\n44444"
            _ <- feedStr e "\ESC[2;3r\ESC[2;1H\ESC[TZZZZZ\ESC[r"
            capture e 4 `shouldReturn` ["11111", "ZZZZZ", "22222", "44444"]

        it "scrolls the region down on RI at the top margin" $ do
            e <- newEmulator Size { rows = 4, cols = 5 } 1000
            _ <- feedStr e "11111\r\n22222\r\n33333\r\n44444"
            _ <- feedStr e "\ESC[2;3r\ESC[2;1H\ESCMZZZZZ\ESC[r"
            capture e 4 `shouldReturn` ["11111", "ZZZZZ", "22222", "44444"]

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

        -- libghostty consumes OSC 1 (icon/tab title) but does not surface the
        -- icon name to hat; it must still be swallowed, not rendered, and raise
        -- no UnknownProp so the logs stay quiet.
        it "swallows an OSC 1 icon name without an UnknownProp event" $ do
            e <- new80x24
            evs <- feedStr e "\ESC]1;my-icon\aX"
            scr <- snapshot e
            rowText scr 0 `shouldBe` "X"
            evs `shouldSatisfy` all (\case UnknownProp {} -> False; _ -> True)

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
        -- of an OSC. The emulator doesn't know it, so hat swallows it here and
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
    cellsText cells = T.stripEnd (T.pack (concatMap cluster (V.toList cells)))
