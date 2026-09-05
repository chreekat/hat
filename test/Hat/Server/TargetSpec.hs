module Hat.Server.TargetSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Hat.Geometry (Rect (..), Size (..))
import Hat.Model.Ids (PaneId (..), SessionId (..), WindowId (..))
import Hat.Server.Target

spec :: Spec
spec = do
    describe "parsePaneTarget" $ do
        it "treats a missing -t as the current pane" $
            parsePaneTarget Nothing `shouldBe` PaneCurrent
        it "parses ! as the last (alternate) pane" $
            parsePaneTarget (Just "!") `shouldBe` PaneLast
        it "parses ~ and {marked} as the marked pane" $ do
            parsePaneTarget (Just "~") `shouldBe` PaneMarked
            parsePaneTarget (Just "{marked}") `shouldBe` PaneMarked
        it "parses %N as a pane id" $
            parsePaneTarget (Just "%7") `shouldBe` PaneById 7
        it "falls back to the current pane for unrecognized targets" $
            parsePaneTarget (Just "nonsense") `shouldBe` PaneCurrent

    describe "resolveTarget: sessions (upstream targets.sh)" $ do
        it "resolves a $id" $
            sessOf "$0:" `shouldBe` Right (SessionId 0)
        it "resolves an exact name with =" $
            sessOf "=alpha:" `shouldBe` Right (SessionId 0)
        it "resolves a full name" $
            sessOf "alpha:" `shouldBe` Right (SessionId 0)
        it "resolves a unique prefix" $
            sessOf "al:" `shouldBe` Right (SessionId 0)
        it "resolves a unique fnmatch pattern" $
            sessOf "al*:" `shouldBe` Right (SessionId 0)
        it "rejects an ambiguous prefix" $
            sessOf "grp:" `shouldBe` Left "can't find session: grp"
        it "rejects an ambiguous pattern" $
            sessOf "grp*:" `shouldBe` Left "can't find session: grp*"
        it "= disables prefix matching" $
            sessOf "=al:" `shouldBe` Left "can't find session: al"
        it "rejects a missing session" $
            sessOf "nosuch:" `shouldBe` Left "can't find session: nosuch"
        it "a session-only target lands on its current window's active pane" $
            resolve "alpha:" `shouldBe` Right (Found (SessionId 0) 0 (PaneId 0))

    describe "resolveTarget: windows (upstream targets.sh)" $ do
        it "resolves a bare @id to window and session" $
            resolve "@1" `shouldBe` Right (Found (SessionId 0) 1 (PaneId 1))
        it "resolves an exact window name" $
            winOf "alpha:shell" `shouldBe` Right 2
        it "resolves a unique window prefix" $
            winOf "alpha:edito" `shouldBe` Right 0
        it "resolves an exact-flagged window name" $
            winOf "alpha:=editor" `shouldBe` Right 0
        it "resolves a window pattern" $
            winOf "alpha:sh*" `shouldBe` Right 2
        it "resolves a window index" $
            winOf "alpha:1" `shouldBe` Right 1
        it "resolves a session-qualified window id" $
            winOf "alpha:@2" `shouldBe` Right 2
        it "rejects a window id from another session" $
            winOf "alpha:@4" `shouldBe` Left "can't find window: @4"
        it "rejects an ambiguous window prefix" $
            winOf "alpha:edit" `shouldBe` Left "can't find window: edit"
        it "rejects an ambiguous window pattern" $
            winOf "alpha:e*" `shouldBe` Left "can't find window: e*"
        it "rejects a missing window name" $
            winOf "alpha:nope" `shouldBe` Left "can't find window: nope"
        it "rejects a missing window id" $
            resolve "@999" `shouldBe` Left "can't find window: @999"

    describe "resolveTarget: window offsets and specials" $ do
        -- Steps wrap; each {name} alias must agree with its symbol. From
        -- current 0 (last 2): ^/{start} first, $/{end} last, +/-/{next}/
        -- {previous} steps, !/{last} the previously current window.
        forM_
            [ ("^", 0), ("$", 3), ("+", 1), ("-", 3), ("+2", 2), ("-2", 2)
            , ("{start}", 0), ("{end}", 3), ("{next}", 1), ("{previous}", 3)
            , ("!", 2), ("{last}", 2)
            ] $ \(t, n) ->
            it (T.unpack t <> " resolves to window " <> show n) $
                winOf ("alpha:" <> t) `shouldBe` Right (n :: Int)

    describe "resolveTarget: combined and empty forms" $ do
        it "no target is the current state" $
            resolveTarget FindPane targetsWorld Nothing
                `shouldBe` Right (Found (SessionId 0) 0 (PaneId 0))
        it "an empty target is the current state" $
            resolve "" `shouldBe` Right (Found (SessionId 0) 0 (PaneId 0))
        it ":window uses the current session without session fallback" $
            resolve ":shell" `shouldBe` Right (Found (SessionId 0) 2 (PaneId 2))
        it "sess:win.pane resolves all three parts" $
            resolve "alpha:shell.0" `shouldBe` Right (Found (SessionId 0) 2 (PaneId 2))
        it "an empty window part uses the session's current window" $
            resolve "alpha:.0" `shouldBe` Right (Found (SessionId 0) 0 (PaneId 0))

    describe "resolveTarget: bare-name fallbacks" $ do
        it "a bare window name falls back from pane to window" $
            resolve "editor" `shouldBe` Right (Found (SessionId 0) 0 (PaneId 0))
        it "a bare session name falls back from pane through window to session" $
            resolve "beta" `shouldBe` Right (Found (SessionId 1) 1 (PaneId 5))

    describe "resolveTarget: whole-target specials" $ do
        it "{active}/@/{current} need an attached client" $ do
            resolve "{active}" `shouldBe` Left "no current client"
            resolve "@" `shouldBe` Left "no current client"
            resolve "{current}" `shouldBe` Left "no current client"
        it "{active} is the attached client's view" $
            let w = targetsWorld { clientCurrent = Just (Found (SessionId 1) 1 (PaneId 5)) }
            in resolveTarget FindPane w (Just "{active}")
                `shouldBe` Right (Found (SessionId 1) 1 (PaneId 5))
        it "{mouse}/= have no mouse event on the command path" $ do
            resolve "{mouse}" `shouldBe` Left "no mouse target"
            resolve "=" `shouldBe` Left "no mouse target"
        it "~/{marked} fail without a mark" $ do
            resolve "~" `shouldBe` Left "no marked target"
            resolve "{marked}" `shouldBe` Left "no marked target"
        it "~ resolves the marked pane from anywhere" $
            let w = panesWorld { marked = Just (Found (SessionId 0) 0 (PaneId 1)) }
            in resolveTarget FindPane w (Just "~")
                `shouldBe` Right (Found (SessionId 0) 0 (PaneId 1))

    describe "resolveWindowIndex (new-window/move-window destinations)" $ do
        it "rejects a pane part" $
            resolveWindowIndex targetsWorld (Just "alpha:1.%0")
                `shouldBe` Left "can't specify pane here"
        it "resolves an offset to a raw index" $
            resolveWindowIndex targetsWorld (Just "alpha:+6")
                `shouldBe` Right (SessionId 0, Just 6)
        it "a session-only target leaves the index free" $
            resolveWindowIndex targetsWorld (Just "alpha:")
                `shouldBe` Right (SessionId 0, Nothing)
        it "keeps a nonexistent numeric index" $
            resolveWindowIndex targetsWorld (Just "alpha:9")
                `shouldBe` Right (SessionId 0, Just 9)
        it "an existing window resolves to its index" $
            resolveWindowIndex targetsWorld (Just "alpha:shell")
                `shouldBe` Right (SessionId 0, Just 2)
        it "a bare session name leaves the index free" $
            resolveWindowIndex targetsWorld (Just "beta")
                `shouldBe` Right (SessionId 1, Nothing)
        it "no target is the current session with a free index" $
            resolveWindowIndex targetsWorld Nothing
                `shouldBe` Right (SessionId 0, Nothing)

    describe "resolveTarget: panes (upstream targets-panes.sh)" $ do
        it "resolves an exact pane id" $
            paneOf "p:0.%3" `shouldBe` Right (PaneId 3)
        it "resolves a pane by index" $
            paneOf "p:0.3" `shouldBe` Right (PaneId 3)
        it "resolves the .pane form in the current window" $
            paneOf ".%1" `shouldBe` Right (PaneId 1)
        it "resolves sess:.pane in the session's current window" $
            paneOf "p:.%1" `shouldBe` Right (PaneId 1)
        it "resolves sess:.{positional}" $
            paneOf "p:.{top-left}" `shouldBe` Right (PaneId 0)
        it "+ is the next pane from the active one" $
            paneOf "p:0.+" `shouldBe` Right (PaneId 1)
        it "- wraps to the previous pane" $
            paneOf "p:0.-" `shouldBe` Right (PaneId 3)
        it "! is the window's last active pane" $
            paneOf "p:0.!" `shouldBe` Right (PaneId 2)
        it "resolves the positional tokens" $ do
            paneOf "p:0.{top-left}" `shouldBe` Right (PaneId 0)
            paneOf "p:0.{top-right}" `shouldBe` Right (PaneId 1)
            paneOf "p:0.{bottom-left}" `shouldBe` Right (PaneId 2)
            paneOf "p:0.{bottom-right}" `shouldBe` Right (PaneId 3)
            paneOf "p:0.{top}" `shouldBe` Right (PaneId 0)
            paneOf "p:0.{bottom}" `shouldBe` Right (PaneId 2)
            paneOf "p:0.{left}" `shouldBe` Right (PaneId 0)
            paneOf "p:0.{right}" `shouldBe` Right (PaneId 1)
        it "resolves directional tokens from the active pane" $ do
            paneOf "p:0.{down-of}" `shouldBe` Right (PaneId 2)
            paneOf "p:0.{right-of}" `shouldBe` Right (PaneId 1)
        it "resolves directional tokens from the bottom-right pane" $ do
            paneOfFrom (PaneId 3) "p:0.{up-of}" `shouldBe` Right (PaneId 1)
            paneOfFrom (PaneId 3) "p:0.{left-of}" `shouldBe` Right (PaneId 2)
        it "rejects a pane id outside the target window" $
            paneOf "p:solo.%0" `shouldBe` Left "can't find pane: %0"
        it "rejects a directional token with no neighbour" $
            paneOf "p:solo.{up-of}" `shouldBe` Left "can't find pane: {up-of}"
        it "rejects an out-of-range pane index" $
            paneOf "p:0.9" `shouldBe` Left "can't find pane: 9"
        it "numbers pane indexes from the configured pane-base-index" $ do
            let based t = (.pane) <$> resolveTarget FindPane
                    panesWorld { paneBase = 1 } (Just t)
            based "p:0.1" `shouldBe` Right (PaneId 0)
            based "p:0.4" `shouldBe` Right (PaneId 3)
            based "p:0.5" `shouldBe` Left "can't find pane: 5"

    describe "snapshot helpers" $ do
        it "paneFound locates a pane in its own window" $
            paneFound targetsWorld.sessions (PaneId 5)
                `shouldBe` Just (Found (SessionId 1) 1 (PaneId 5))
        it "sessionCurrentFound lands on the pane's session current view" $
            sessionCurrentFound targetsWorld.sessions (PaneId 3)
                `shouldBe` Just (Found (SessionId 0) 0 (PaneId 0))

    describe "wildMatch" $ do
        it "matches * and ?" $ do
            wildMatch "al*" "alpha" `shouldBe` True
            wildMatch "a?pha" "alpha" `shouldBe` True
            wildMatch "al*" "beta" `shouldBe` False
        it "matches character classes" $ do
            wildMatch "grp[12]" "grp1" `shouldBe` True
            wildMatch "grp[!12]" "grp3" `shouldBe` True
            wildMatch "grp[!12]" "grp1" `shouldBe` False
        it "requires a full match" $
            wildMatch "al" "alpha" `shouldBe` False

-- Fixtures -------------------------------------------------------------------

area :: Size
area = Size { rows = 23, cols = 80 }

fullRect :: Rect
fullRect = Rect { startRow = 0, endRow = 23, startCol = 0, endCol = 80 }

-- A one-pane window.
soloWin :: Int -> Text -> Int -> WindowEntry
soloWin wid nm pid = WindowEntry
    { windowId = WindowId wid
    , name = nm
    , panes = [(PaneId pid, fullRect)]
    , activePane = PaneId pid
    , lastPane = Nothing
    , area = area
    }

-- The targets.sh fixture: session alpha with four named windows (current 0,
-- last 2), plus beta and the ambiguous grp* pair.
targetsWorld :: World
targetsWorld = World
    { sessions =
        [ SessionEntry
            { sessionId = SessionId 0
            , name = "alpha"
            , windows =
                [ (0, soloWin 0 "editor" 0)
                , (1, soloWin 1 "editing" 1)
                , (2, soloWin 2 "shell" 2)
                , (3, soloWin 3 "logs" 3)
                ]
            , currentIx = 0
            , lastIx = Just 2
            }
        , SessionEntry
            { sessionId = SessionId 1
            , name = "beta"
            , windows = [ (0, soloWin 4 "bw0" 4), (1, soloWin 5 "bw1" 5) ]
            , currentIx = 1
            , lastIx = Nothing
            }
        , SessionEntry
            { sessionId = SessionId 2, name = "grp1"
            , windows = [ (0, soloWin 6 "g1" 6) ], currentIx = 0, lastIx = Nothing }
        , SessionEntry
            { sessionId = SessionId 3, name = "grp2"
            , windows = [ (0, soloWin 7 "g2" 7) ], currentIx = 0, lastIx = Nothing }
        ]
    , paneBase = 0
    , marked = Nothing
    , clientCurrent = Nothing
    , current = Just (Found (SessionId 0) 0 (PaneId 0))
    }

-- The targets-panes.sh fixture: session p, window 0 a 2x2 split
-- (%0 top-left, %1 top-right, %2 bottom-left, %3 bottom-right; active %0,
-- last %2), window "solo" single-pane.
panesWorldFrom :: PaneId -> World
panesWorldFrom active = World
    { sessions =
        [ SessionEntry
            { sessionId = SessionId 0
            , name = "p"
            , windows =
                [ ( 0
                  , WindowEntry
                        { windowId = WindowId 0
                        , name = "grid"
                        , panes =
                            [ (PaneId 0, Rect 0 11 0 40)
                            , (PaneId 1, Rect 0 11 41 80)
                            , (PaneId 2, Rect 12 23 0 40)
                            , (PaneId 3, Rect 12 23 41 80)
                            ]
                        , activePane = active
                        , lastPane = Just (PaneId 2)
                        , area = area
                        }
                  )
                , (1, soloWin 1 "solo" 4)
                ]
            , currentIx = 0
            , lastIx = Nothing
            }
        ]
    , paneBase = 0
    , marked = Nothing
    , clientCurrent = Nothing
    , current = Just (Found (SessionId 0) 0 active)
    }

panesWorld :: World
panesWorld = panesWorldFrom (PaneId 0)

-- Helpers --------------------------------------------------------------------

resolve :: Text -> Either Text Found
resolve t = resolveTarget FindPane targetsWorld (Just t)

sessOf :: Text -> Either Text SessionId
sessOf t = (.session) <$> resolve t

winOf :: Text -> Either Text Int
winOf t = (.windowIx) <$> resolve t

paneOf :: Text -> Either Text PaneId
paneOf t = (.pane) <$> resolveTarget FindPane panesWorld (Just t)

paneOfFrom :: PaneId -> Text -> Either Text PaneId
paneOfFrom active t =
    (.pane) <$> resolveTarget FindPane (panesWorldFrom active) (Just t)
