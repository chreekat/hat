module Hat.Server.KeymapSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import Test.Hspec

import Hat.Server.Keymap (DefaultBinding (..), defaultBindings, defaultKey, defaultKeymap)
import Hat.Server.Keys (Binding (..), Key (..), parseKeyName)

-- | The keys tmux's @key_bindings_init@ binds, by table, in its order.
tmuxDefaultKeys :: [(Text, [Text])]
tmuxDefaultKeys =
    [ ("prefix",
        [ "C-b", "C-o", "C-z", "Space", "!", "\"", "#", "$", "%", "&", "'", "*", "@", "(", ")"
        , ",", "-", ".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "="
        , "?", "D", "E", "L", "M", "[", "]", "c", "d", "f", "i", "l", "m", "n", "o", "C", "p"
        , "q", "r", "s", "t", "Tab", "BTab", "w", "x", "z", "{", "}", "~", "PPage", "Up"
        , "Down", "Left", "Right", "M-1", "M-2", "M-3", "M-4", "M-5", "M-6", "M-7", "M-n"
        , "M-o", "M-p", "S-Up", "S-Down", "S-Left", "S-Right", "DC", "M-Up", "M-Down"
        , "M-Left", "M-Right", "C-Up", "C-Down", "C-Left", "C-Right", "g", "<", ">" ])
    , ("move",
        [ "1", "2", "3", "4", "M-1", "M-2", "M-3", "M-4", "Up", "Down", "Left", "Right"
        , "M-Up", "M-Down", "M-Left", "M-Right", "0", ",", "." ])
    , ("root",
        [ "MouseDown1Pane", "MouseDrag1Pane", "C-MouseDrag1Pane", "C-MouseDrag1Empty"
        , "M-MouseDrag1Pane", "WheelUpPane", "MouseDown2Pane", "DoubleClick1Pane"
        , "TripleClick1Pane", "MouseDown1Border", "MouseDrag1Border", "M-MouseDrag1Border"
        , "MouseDown1Status", "C-MouseDown1Status", "MouseDown1Control9", "MouseDown1Control8"
        , "MouseDown1Control7", "WheelDownStatus", "WheelUpStatus", "MouseDown3StatusLeft"
        , "M-MouseDown3StatusLeft", "MouseDown3Status", "M-MouseDown3Status", "MouseDown3Pane"
        , "M-MouseDown3Pane", "MouseDown3Empty", "M-MouseDown3Empty", "MouseDown1ScrollbarUp"
        , "MouseDown1ScrollbarDown", "MouseDrag1ScrollbarSlider" ])
    , ("copy-mode",
        [ "C-Space", "C-a", "C-c", "C-e", "C-f", "C-b", "C-g", "C-k", "C-l", "M-l", "C-n", "C-p"
        , "C-r", "C-s", "C-v", "C-w", "Escape", "C-[", "Space", ",", ";", "F", "L", "N", "P"
        , "R", "T", "X", "f", "g", "n", "q", "r", "t", "Home", "End", "MouseDown1Pane"
        , "MouseDrag1Pane", "MouseDragEnd1Pane", "WheelUpPane", "WheelDownPane"
        , "DoubleClick1Pane", "TripleClick1Pane", "NPage", "PPage", "Up", "Down", "Left"
        , "Right", "M-1", "M-2", "M-3", "M-4", "M-5", "M-6", "M-7", "M-8", "M-9", "M-<", "M->"
        , "M-R", "M-b", "C-M-b", "M-f", "C-M-f", "M-m", "M-r", "M-v", "M-w", "M-x", "M-{", "M-}"
        , "M-Up", "M-Down", "C-Up", "C-Down", "M-C-Up", "M-C-Down" ])
    , ("copy-mode-vi",
        [ "#", "*", "C-c", "C-d", "C-e", "C-b", "C-f", "C-h", "C-j", "Enter", "C-u", "C-v", "C-y"
        , "Escape", "C-[", "Space", "$", ",", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8"
        , "9", ":", ";", "?", "A", "B", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "P"
        , "T", "V", "W", "X", "^", "b", "e", "f", "g", "h", "j", "k", "z", "l", "n", "o", "q"
        , "r", "t", "v", "w", "{", "}", "%", "Home", "End", "MouseDown1Pane", "MouseDrag1Pane"
        , "MouseDragEnd1Pane", "WheelUpPane", "WheelDownPane", "DoubleClick1Pane"
        , "TripleClick1Pane", "BSpace", "NPage", "PPage", "Up", "Down", "Left", "Right", "M-x"
        , "C-Up", "C-Down" ])
    ]

spec :: Spec
spec = describe "the default key tables" $ do
    it "bind every tmux default key, or name what hat lacks for it (bug c4)" $ do
        let unbound = [ (t, canonical k) | Unbound line _ <- defaultBindings
                                         , let (t, k) = defaultKey line ]
            covered t k = maybe False (Map.member k) (Map.lookup t defaultKeymap)
                || (t, k) `elem` unbound
            canonical k = maybe k (.name) (parseKeyName k)
        [ (t, k) | (t, ks) <- tmuxDefaultKeys, k <- ks, not (covered t (canonical k)) ]
            `shouldBe` []
    it "note every prefix key, as tmux does" $
        [ k | Just prefix <- [Map.lookup "prefix" defaultKeymap]
            , (k, b) <- Map.toList prefix, isNothing b.note ]
            `shouldBe` []
