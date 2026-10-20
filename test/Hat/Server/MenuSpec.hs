module Hat.Server.MenuSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Hspec

import Hat.Command.Parser (parseScript)
import Hat.Command.Script (Script (..))
import Hat.Model.Ids (PaneId (..), SessionId (..), WindowId (..))
import Hat.Server.Box (BoxLines (..), Grid)
import Hat.Server.DisplayPos
import Hat.Server.Keymap (defaultKeymap, menuBindings)
import Hat.Server.Keys (Binding (..))
import Hat.Server.Menu
import Hat.Server.Theme (terminalTheme)
import Hat.Term.Cell qualified as Cell

item :: Text -> Text -> ItemSpec
item n k = ItemSpec { name = n, key = if T.null k then Nothing else Just k, command = "cmd " <> n }

-- display-menu -T Menu "Alpha item" a "" "Beta item" b "" "" "Gamma item" g ""
basic :: Menu
basic = foldl (flip (menuAddItem 40)) (menuCreate "Menu")
    [Just (item "Alpha item" "a"), Just (item "Beta item" "b"), Nothing, Just (item "Gamma item" "g")]

shown :: Menu -> WindowMenu
shown = shownWith BoxSingle

shownWith :: BoxLines -> Menu -> WindowMenu
shownWith l m = WindowMenu
    { menu = m, choice = -1, px = 6, py = 2, stayOpen = CloseOnChoice, mouse = MenuMouseOn
    , lines = l, style = Nothing, selectedStyle = Nothing, borderStyle = Nothing
    , target = (SessionId 0, WindowId 0, PaneId 0), event = Nothing }

rowText :: Grid -> Int -> Text
rowText g y = T.pack [ Cell.baseChar c | c <- maybe [] V.toList (g V.!? y) ]

look :: MenuLook
look = MenuLook
    { style = Cell.defaultStyle { Cell.fg = Cell.Indexed 250 }
    , selected = Cell.defaultStyle { Cell.fg = Cell.Indexed 16 }
    , border = Cell.defaultStyle { Cell.fg = Cell.Indexed 45 } }

press, release :: Int -> Int -> MenuInput
press x y = MenuMouseAt 0 (Just (x, y))
release x y = MenuMouseAt 3 (Just (x, y))

spec :: Spec
spec = do
    describe "menuAddItem" $ do
        it "right-aligns an item's key after its name" $ do
            map (fmap (.name)) basic.items `shouldBe`
                [ Just "Alpha item#[default] #[align=right](a)"
                , Just "Beta item#[default] #[align=right](b)"
                , Nothing
                , Just "Gamma item#[default] #[align=right](g)" ]
            basic.width `shouldBe` 14
        it "never leads with or doubles a separator" $
            length (foldl (flip (menuAddItem 40)) (menuCreate "")
                [Nothing, Just (item "a" ""), Nothing, Nothing, Just (item "b" "")]).items
                `shouldBe` 3
        it "drops an item whose name expands to nothing" $
            menuAddItem 40 (Just (item "" "x")) (menuCreate "") `shouldBe` menuCreate ""
        it "keeps a long name's right end and drops a key that cannot fit" $
            map (fmap (.name)) (menuAddItem 12
                (Just (item "This item is much wider than the window" "t")) (menuCreate "")).items
                `shouldBe` [Just " window>"]

    describe "menuGrid" $ do
        let grid = menuGrid terminalTheme look (shown basic) { choice = 1 }
        it "draws the box, title, items and separator" $
            map (rowText grid) [0 .. 5] `shouldBe`
                [ "\x250c\x2500Menu\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2510"
                , "\x2502 Alpha item (a) \x2502"
                , "\x2502 Beta item  (b) \x2502"
                , "\x251c" <> T.replicate 16 "\x2500" <> "\x2524"
                , "\x2502 Gamma item (g) \x2502"
                , "\x2514" <> T.replicate 16 "\x2500" <> "\x2518" ]
        it "draws the chosen item in the selected style, the rest in the menu style" $ do
            fmap (.style) (grid V.! 2 V.!? 1) `shouldBe` Just look.selected
            fmap (.style) (grid V.! 1 V.!? 3) `shouldBe` Just look.style
            fmap (.style) (grid V.! 0 V.!? 0) `shouldBe` Just look.border
        it "draws a box with no lines as blanks, its title still there" $
            rowText (menuGrid terminalTheme look (shownWith BoxNone basic)) 0
                `shouldBe` "  Menu            "
        it "dims a disabled item, its leading dash dropped" $ do
            let m = menuAddItem 40 (Just (item "-Off" "")) (menuCreate "")
                g = menuGrid terminalTheme look (shown m)
            rowText g 1 `shouldBe` "\x2502 Off \x2502"
            fmap (.style.faint) (g V.! 1 V.!? 2) `shouldBe` Just True

    describe "menuKey" $ do
        let wm = shown basic
        it "moves over separators and wraps" $ do
            choiceAfter [MenuKeyPress "Down", MenuKeyPress "Down", MenuKeyPress "Down"] wm
                `shouldBe` Just 3
            choiceAfter [MenuKeyPress "Up"] wm `shouldBe` Just 3
            choiceAfter [MenuKeyPress "Down", MenuKeyPress "Up"] wm `shouldBe` Just 3
        it "chooses an item by its key or by Enter" $ do
            fmap (.name) (chose (menuKey (MenuKeyPress "b") wm))
                `shouldBe` Just "Beta item#[default] #[align=right](b)"
            menuKey (MenuKeyPress "Enter") wm `shouldBe` MenuCloses
            fmap (.command) (chose (menuKey (MenuKeyPress "Enter") wm { choice = 3 }))
                `shouldBe` Just "cmd Gamma item"
        it "closes on Escape and q, ignores other keys" $ do
            menuKey (MenuKeyPress "Escape") wm `shouldBe` MenuCloses
            menuKey (MenuKeyPress "q") wm `shouldBe` MenuCloses
            menuKey (MenuKeyPress "x") wm `shouldBe` MenuStays wm
        it "highlights the item a press is over and chooses the highlight on release" $ do
            menuKey (press 8 3) wm `shouldBe` MenuStays wm { choice = 0 }
            fmap (.command) (chose (menuKey (release 8 3) wm { choice = 1 }))
                `shouldBe` Just "cmd Beta item"
        it "closes on a release outside it, or anywhere with -O on a press outside" $ do
            menuKey (release 1 1) wm `shouldBe` MenuCloses
            menuKey (press 1 1) wm { choice = 1 } `shouldBe` MenuStays wm
            menuKey (press 1 1) wm { stayOpen = StayOpen } `shouldBe` MenuCloses
        it "takes only button 1 when it has no mouse" $ do
            menuKey (press 8 3) wm { mouse = MenuNoMouse } `shouldBe` MenuStays wm { mouse = MenuNoMouse }
            menuKey (release 8 3) wm { mouse = MenuNoMouse } `shouldBe` MenuCloses

    describe "startingChoice" $ do
        it "starts a keyboard menu on the item asked for, skipping separators" $ do
            startingChoice MenuNoMouse 2 basic `shouldBe` 3
            startingChoice MenuNoMouse 9 basic `shouldBe` 3
            startingChoice MenuNoMouse (-1) basic `shouldBe` (-1)
        it "starts a mouse menu on nothing" $
            startingChoice MenuMouseOn 1 basic `shouldBe` (-1)

    describe "placing" $ do
        it "pulls a menu back inside its window" $ do
            placeMenu 40 14 basic (30, 12) `shouldBe` (22, 8)
            placeMenu 12 6 basic (5, 1) `shouldBe` (0, 0)
            (refitMenu 20 6 (shown basic)) { choice = 0 } `shouldBe` (shown basic) { px = 2, py = 0, choice = 0 }
        it "reads -y as the bottom row" $ do
            menuPlaceY 24 4 "7" `shouldBe` 3
            menuPlaceX 80 18 "5" `shouldBe` 5
            menuPlaceX 40 18 "-3" `shouldBe` 0
        it "names the window's centre and the mouse" $ do
            let vars = menuPosVars 18 6 40 14 14 (Just (MouseAt 10 5 Nothing 0)) (0, 0) Nothing
                    (PaneAt 0 0 40 14)
            Map.lookup "popup_centre_x" vars `shouldBe` Just "10"
            Map.lookup "popup_centre_y" vars `shouldBe` Just "9"
            Map.lookup "popup_mouse_top" vars `shouldBe` Just "11"
            Map.lookup "popup_status_line_y" vars `shouldBe` Nothing
        it "places a click on a top status line at the window's top" $
            Map.lookup "popup_mouse_y"
                (menuPosVars 4 4 80 23 23 (Just (MouseAt 3 0 (Just 0) 1)) (0, 0) Nothing (PaneAt 0 0 80 23))
                `shouldBe` Just "0"
        it "expands a letter to its variable" $ do
            positionFormat AxisX (Just "M") `shouldBe` "#{popup_mouse_centre_x}"
            positionFormat AxisY Nothing `shouldBe` "#{popup_centre_y}"
            positionFormat AxisY (Just "12") `shouldBe` "12"
        it "reads a number as strtol does" $ do
            strtol " 12abc" `shouldBe` 12
            strtol "-4" `shouldBe` (-4)
            strtol "x" `shouldBe` 0

    describe "the default menu bindings" $ do
        it "all parse" $
            [ k | (_, k, cmd) <- menuBindings, isJust (parseScript cmd).failure ]
                `shouldBe` []
        it "open a menu on a right click in the empty area" $
            fmap (map (take 1) . (.commands)) (Map.lookup "root" defaultKeymap >>= Map.lookup "MouseDown3Empty")
                `shouldBe` Just [["display-menu"]]
  where
    chose = \case
        MenuChose chosenItem -> Just chosenItem
        _ -> Nothing
    choiceAfter inputs wm0 = go inputs wm0
      where
        go [] w = Just w.choice
        go (i : is) w = case menuKey i w of
            MenuStays w' -> go is w'
            _ -> Nothing
