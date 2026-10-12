module Hat.Server.MouseSpec (spec) where

import Data.ByteString.Char8 qualified as B8
import Data.List (mapAccumL)
import Data.Text (Text)
import Test.Hspec

import Hat.Geometry (Pos (..))
import Hat.Model.Ids (PaneId (..), SessionId (..), WindowId (..))
import Hat.Server.Mouse

-- Two panes side by side (40 and 39 columns, a border between) in an 80x23
-- area, with a one-line status line below.
view :: MouseView
view = MouseView
    { session = SessionId 0, window = WindowId 1
    , statusAt = Just 23, statusLines = 1
    , statusRange = \x _ -> if x < 10 then Just (RangeWindow (WindowId 1))
                            else if x < 20 then Just RangeLeft else Nothing
    , width = 80, height = 23
    , boxes =
        [ PaneBox { pane = PaneId 0, xoff = 0, yoff = 0, sx = 40, sy = 23, kind = TiledBox }
        , PaneBox { pane = PaneId 1, xoff = 41, yoff = 0, sx = 39, sy = 23, kind = TiledBox } ]
    , modal = Nothing, origin = Pos 0 0 }

-- 'view' with a floating pane over the left pane: content 10x4 at 10,5,
-- its border around it.
floatView :: BoxKind -> MouseView
floatView k = view
    { boxes = PaneBox { pane = PaneId 2, xoff = 10, yoff = 5, sx = 10, sy = 4, kind = k }
        : view.boxes }

-- The keys a run of reports makes against a view, all at time 0, with the
-- pane each is for; a report swallowed outside a modal pane shows as the
-- modal pane it closes.
placed :: MouseView -> [MouseInput] -> [Either (Maybe PaneId) (Text, Maybe PaneId)]
placed v = concatMap outcome . snd . mapAccumL (checkMouse 0 v) initialMouseState
  where
    outcome = \case
        Keyed k -> [Right (k.name, k.event.onPane)]
        Unplaced -> []
        Swallowed m -> [Left m]

sgr :: Int -> Int -> Int -> Char -> MouseInput
sgr b x y t = case parseMouseInput bytes of
    Just m -> m
    Nothing -> error "bad report"
  where
    bytes = B8.pack ("\ESC[<" <> show b <> ";" <> show (x + 1) <> ";" <> show (y + 1) <> [t])

-- The key names a run of reports makes, all at time 0 unless timed.
names :: [(Int, MouseInput)] -> [Text]
names = concatMap (\case Keyed k -> [k.name]; _ -> []) . snd
    . mapAccumL (\st (t, m) -> checkMouse t view st m) initialMouseState

at0 :: [MouseInput] -> [Text]
at0 = names . map (0,)

spec :: Spec
spec = do
    describe "parseMouseInput" $ do
        it "reads an SGR press and release" $ do
            parseMouseInput "\ESC[<0;3;1M" `shouldBe`
                Just MouseInput { b = 0, x = 2, y = 0, sgrType = 'M', sgrB = 0 }
            parseMouseInput "\ESC[<0;3;1m" `shouldBe`
                Just MouseInput { b = 3, x = 2, y = 0, sgrType = 'm', sgrB = 0 }
        it "reads a legacy report" $
            parseMouseInput "\ESC[M !!" `shouldBe`
                Just MouseInput { b = 0, x = 0, y = 0, sgrType = ' ', sgrB = 0 }
        it "drops a wheel release (PuTTY sends them)" $
            parseMouseInput "\ESC[<64;3;1m" `shouldBe` Nothing

    describe "a view panned over a larger window" $ do
        let panned = view { origin = Pos { row = 0, col = 41 } }
        it "places a report at the window cell under it" $
            placed panned [sgr 0 3 0 'M'] `shouldBe` [Right ("MouseDown1Pane", Just (PaneId 1))]
        it "gives a pane the position inside it" $ do
            let (_, p) = checkMouse 0 panned initialMouseState (sgr 0 3 2 'M')
            case p of
                Keyed k -> mouseAt False k.event (view.boxes !! 1) `shouldBe` Just (3, 2)
                _ -> expectationFailure "no key"

    describe "scrollbarHover" $ do
        let hover = scrollbarHover view initialMouseState
        it "a move over the window is at its window point" $
            hover (sgr 35 50 3 'M') `shouldBe` Just (Just (50, 3))
        it "a move onto the status line is off the window" $
            hover (sgr 35 5 23 'M') `shouldBe` Just Nothing
        it "a click is no move" $
            hover (sgr 0 50 3 'M') `shouldBe` Nothing
    describe "checkMouse names the key" $ do
        it "a click and release on a pane" $
            at0 [sgr 0 3 0 'M', sgr 0 3 0 'm'] `shouldBe` ["MouseDown1Pane", "MouseUp1Pane"]
        it "button 3 on the other pane, with ctrl" $
            at0 [sgr 18 50 5 'M'] `shouldBe` ["C-MouseDown3Pane"]
        it "the border between panes" $
            at0 [sgr 0 40 5 'M'] `shouldBe` ["MouseDown1Border"]
        it "status line ranges" $ do
            at0 [sgr 0 3 23 'M'] `shouldBe` ["MouseDown1Status"]
            at0 [sgr 0 13 23 'M'] `shouldBe` ["MouseDown1StatusLeft"]
            at0 [sgr 0 50 23 'M'] `shouldBe` ["MouseDown1StatusDefault"]
        it "the wheel" $ do
            at0 [sgr 64 3 0 'M'] `shouldBe` ["WheelUpPane"]
            at0 [sgr 65 3 23 'M'] `shouldBe` ["WheelDownStatus"]
        it "a drag: start, updates, end" $
            at0 [sgr 0 3 0 'M', sgr 32 4 0 'M', sgr 32 5 0 'M', sgr 0 5 0 'm']
                `shouldBe` ["MouseDown1Pane", "MouseDrag1Pane", "MouseDrag1Pane", "MouseDragEnd1Pane"]
        it "a second and third click within the timeout" $
            names [ (0, sgr 0 3 0 'M'), (10, sgr 0 3 0 'm')
                  , (100, sgr 0 3 0 'M'), (110, sgr 0 3 0 'm')
                  , (200, sgr 0 3 0 'M') ]
                `shouldBe` [ "MouseDown1Pane", "MouseUp1Pane", "SecondClick1Pane"
                           , "MouseUp1Pane", "TripleClick1Pane" ]
        it "a click on another pane restarts the run" $
            names [(0, sgr 0 3 0 'M'), (100, sgr 0 50 0 'M')]
                `shouldBe` ["MouseDown1Pane", "MouseDown1Pane"]
        it "a second click left alone becomes a double click" $ do
            let (st1, _) = checkMouse 0 view initialMouseState (sgr 0 3 0 'M')
                (st2, _) = checkMouse 100 view st1 (sgr 0 3 0 'M')
            fmap (.name) (snd (clickExpired 300 st2)) `shouldBe` Nothing
            fmap (.name) (snd (clickExpired 400 st2)) `shouldBe` Just "DoubleClick1Pane"

    describe "checkMouse over floating panes" $ do
        it "a click inside one goes to it, not the pane beneath" $
            placed (floatView FramedFloat) [sgr 0 12 6 'M']
                `shouldBe` [Right ("MouseDown1Pane", Just (PaneId 2))]
        it "its border is its own" $
            placed (floatView FramedFloat) [sgr 0 9 6 'M', sgr 0 9 6 'm', sgr 0 12 4 'M']
                `shouldBe` map (\n -> Right (n, Just (PaneId 2)))
                    ["MouseDown1Border", "MouseUp1Border", "SecondClick1Border"]
        it "one with no border leaves its edge to the pane beneath" $
            placed (floatView BareFloat) [sgr 0 9 6 'M']
                `shouldBe` [Right ("MouseDown1Pane", Just (PaneId 0))]
        it "a drag remembers where it started" $ do
            let (st, _) = mapAccumL (checkMouse 0 (floatView FramedFloat)) initialMouseState
                    [sgr 0 12 6 'M', sgr 32 20 8 'M']
            st.dragStart `shouldBe` (12, 6)
    describe "checkMouse with a modal pane" $ do
        let modalView how = (floatView FramedFloat) { modal = Just ModalView
                { pane = PaneId 2, outside = how } }
        it "only the modal pane takes clicks" $
            placed (modalView KeepModal) [sgr 0 3 0 'M', sgr 0 12 6 'M']
                `shouldBe` [Left Nothing, Right ("MouseDown1Pane", Just (PaneId 2))]
        it "a click outside one made to close on it closes it" $
            placed (modalView CloseModal) [sgr 0 3 0 'M']
                `shouldBe` [Left (Just (PaneId 2))]
        it "a drag begun on it stays with it outside" $
            placed (modalView KeepModal) [sgr 0 12 6 'M', sgr 32 30 6 'M', sgr 0 30 6 'm']
                `shouldBe` map (\n -> Right (n, Just (PaneId 2)))
                    ["MouseDown1Pane", "MouseDrag1Pane", "MouseDragEnd1Pane"]

    describe "mouseBytes (what a pane's app is sent)" $ do
        let ev m = case checkMouse 0 view initialMouseState m of
                (_, Keyed k) -> k.event
                _ -> error "no key"
            click = ev (sgr 0 50 5 'M')
            box1 = PaneBox { pane = PaneId 1, xoff = 41, yoff = 0, sx = 39, sy = 23, kind = TiledBox }
        it "pane-relative, in SGR when the app asked for it" $ do
            mouseAt False click box1 `shouldBe` Just (9, 5)
            mouseBytes PaneMouseMode { tracking = 1, sgr = True, utf8 = False } click (9, 5)
                `shouldBe` Just "\ESC[<0;10;6M"
        it "legacy otherwise" $
            mouseBytes PaneMouseMode { tracking = 1, sgr = False, utf8 = False } click (9, 5)
                `shouldBe` Just "\ESC[M *&"
        it "nothing for an app that asked for no mouse" $
            mouseBytes PaneMouseMode { tracking = 0, sgr = True, utf8 = False } click (9, 5)
                `shouldBe` Nothing
        it "a drag only to an app tracking drags" $ do
            let drag = ev (sgr 32 50 5 'M')
            mouseBytes PaneMouseMode { tracking = 1, sgr = True, utf8 = False } drag (9, 5)
                `shouldBe` Nothing
            mouseBytes PaneMouseMode { tracking = 2, sgr = True, utf8 = False } drag (9, 5)
                `shouldBe` Just "\ESC[<32;10;6M"
    describe "outerMouseMode (the tracking a client asks of its terminal)" $ do
        it "with mouse off, mirrors the active pane's" $ do
            outerMouseMode False False 0 [0, 3] `shouldBe` 0
            outerMouseMode False True 1 [1] `shouldBe` 1
        it "with mouse on, always tracks drags" $
            outerMouseMode True False 0 [0, 1] `shouldBe` 2
        it "tracks all motion for focus-follows-mouse or a pane that wants it" $ do
            outerMouseMode True True 0 [0] `shouldBe` 3
            outerMouseMode True False 0 [0, 3] `shouldBe` 3
        it "over a menu or popup, takes the overlay's own, raised to drags by mouse" $ do
            overlayMouseMode False False 3 `shouldBe` 3
            overlayMouseMode False False 0 `shouldBe` 0
            overlayMouseMode True False 0 `shouldBe` 2
            overlayMouseMode True False 3 `shouldBe` 3
            overlayMouseMode True True 1 `shouldBe` 3
        it "clears every mode before asking for the new one" $ do
            mouseModeBytes 0 `shouldBe` "\ESC[?1006l\ESC[?1000l\ESC[?1002l\ESC[?1003l"
            mouseModeBytes 2 `shouldBe`
                "\ESC[?1006l\ESC[?1000l\ESC[?1002l\ESC[?1003l\ESC[?1006h\ESC[?1000h\ESC[?1002h"
