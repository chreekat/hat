-- | Pane modes (copy mode, and tmux's mode-tree screens) driven through the
-- real command table against an in-process server with real ptys.
module Hat.Server.PaneModeSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.Timeout (timeout)
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model
import Hat.Server (Reply (..))
import Hat.Server.Dispatch (installHooks, runArgv)
import Hat.Server.Keymap (defaultKeymap)
import Hat.Server.PaneMode (dropModesForReload)
import Hat.Server.DisplayPanes (drawPanesMode)
import Hat.Server.TreeMode (drawTreeMode)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Pty qualified as Pty

-- | A fresh server for one test; on exit every pane still alive is killed
-- and its child reaped, so no test leaks a process.
withServer :: (ServerState -> IO a) -> IO a
withServer = bracket open close
  where
    open = do
        lg <- newLogger "/dev/null"
        st <- newServerState defaultKeymap lg "/tmp/hat-panemodespec.sock" Nothing
        installHooks st
        pure st
    close st = do
        panes <- allPanes st
        void (run st ["kill-server"])
        forM_ panes $ \p -> timeout 2_000_000 (Pty.waitExit p.pty)

allPanes :: ServerState -> IO [Pane]
allPanes st = atomically $ do
    sessions <- Map.elems <$> readTVar st.sessions
    wins <- concat <$> mapM (fmap Map.elems . readTVar . (.windows)) sessions
    concat <$> mapM (fmap Map.elems . readTVar . (.panes)) wins

run :: ServerState -> [Text] -> IO [Reply]
run st = runArgv st Nothing

-- | Expand a format against a target pane.
display :: ServerState -> Text -> Text -> IO [Reply]
display st tgt fmt = run st ["display-message", "-p", "-t", tgt, fmt]

spec :: Spec
spec = do
    describe "pane_in_mode" $
        it "reports the target pane's own mode, not its session's active pane" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["new-window", "-t", "S:1", "cat"] `shouldReturn` []
                run st ["copy-mode", "-t", "S:0.0"] `shouldReturn` []
                display st "S:0.0" "#{pane_in_mode} #{pane_mode}"
                    `shouldReturn` [ROutput "1 copy-mode"]
                display st "S:1.0" "#{pane_in_mode}"
                    `shouldReturn` [ROutput "0"]

    describe "-k" $ do
        it "kills the pane when its copy mode exits" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["split-window", "-t", "S:0", "cat"] `shouldReturn` []
                [a, b] <- paneIds st "S:0"
                run st ["copy-mode", "-k", "-t", b] `shouldReturn` []
                run st ["send-keys", "-t", b, "-X", "cancel"] `shouldReturn` []
                paneIds st "S:0" `shouldReturn` [a]
        it "leaves the pane alone without -k" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["split-window", "-t", "S:0", "cat"] `shouldReturn` []
                [a, b] <- paneIds st "S:0"
                run st ["copy-mode", "-t", b] `shouldReturn` []
                run st ["send-keys", "-t", b, "-X", "cancel"] `shouldReturn` []
                display st b "#{pane_in_mode}" `shouldReturn` [ROutput "0"]
                paneIds st "S:0" `shouldReturn` [a, b]

    describe "send-keys" $
        it "runs a key through the copy-mode table of a pane in copy mode" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["set-option", "-g", "mode-keys", "vi"] `shouldReturn` []
                run st ["copy-mode", "-t", "S:0.0"] `shouldReturn` []
                run st ["send-keys", "-t", "S:0.0", "q"] `shouldReturn` []
                display st "S:0.0" "#{pane_in_mode}" `shouldReturn` [ROutput "0"]

    describe "tree modes" $ do
        it "choose-tree shows tree-mode until q" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["choose-tree", "-t", "S:0.0"] `shouldReturn` []
                display st "S:0.0" "#{pane_in_mode} #{pane_mode}"
                    `shouldReturn` [ROutput "1 tree-mode"]
                run st ["send-keys", "-t", "S:0.0", "q"] `shouldReturn` []
                display st "S:0.0" "#{pane_in_mode}" `shouldReturn` [ROutput "0"]
        it "choose-tree -k kills the pane when the mode exits" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["split-window", "-t", "S:0", "cat"] `shouldReturn` []
                [a, b] <- paneIds st "S:0"
                run st ["choose-tree", "-k", "-t", b] `shouldReturn` []
                run st ["send-keys", "-t", b, "q"] `shouldReturn` []
                paneIds st "S:0" `shouldReturn` [a]
        it "stacks over copy mode and returns to it" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["copy-mode", "-t", "S:0.0"] `shouldReturn` []
                run st ["choose-tree", "-t", "S:0.0"] `shouldReturn` []
                run st ["send-keys", "-t", "S:0.0", "Escape"] `shouldReturn` []
                display st "S:0.0" "#{pane_mode}" `shouldReturn` [ROutput "copy-mode"]
        it "rejects an unknown sort order" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["choose-tree", "-O", "sideways", "-t", "S:0.0"]
                    `shouldReturn` [RErr "invalid sort order"]
        it "choose-buffer has nothing to show without a buffer" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["choose-buffer", "-t", "S:0.0"] `shouldReturn` []
                display st "S:0.0" "#{pane_in_mode}" `shouldReturn` [ROutput "0"]
        it "choose-buffer d deletes the selected buffer" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["set-buffer", "-b", "one", "1"] `shouldReturn` []
                run st ["set-buffer", "-b", "two", "2"] `shouldReturn` []
                run st ["choose-buffer", "-O", "name", "-t", "S:0.0"] `shouldReturn` []
                run st ["send-keys", "-t", "S:0.0", "d"] `shouldReturn` []
                run st ["list-buffers", "-F", "#{buffer_name}"]
                    `shouldReturn` [ROutput "two"]
                display st "S:0.0" "#{pane_mode}" `shouldReturn` [ROutput "buffer-mode"]
        it "/ narrows the tree to fuzzy matches as typed, and Enter chooses the best" $
            withServer $ \st -> do
                searchable st
                run st ["send-keys", "-t", "work:0.0", "/", "hat", "Enter"] `shouldReturn` []
                run st ["show", "-gv", "@chosen"] `shouldReturn` [ROutput "=projects:1."]
                display st "work:0.0" "#{pane_in_mode}" `shouldReturn` [ROutput "0"]
        it "Up and Down move through the matches while the search is typed" $
            withServer $ \st -> do
                searchable st
                run st ["send-keys", "-t", "work:0.0", "/", "hat", "Down", "Enter"]
                    `shouldReturn` []
                run st ["show", "-gv", "@chosen"] `shouldReturn` [ROutput "=work:"]
        it "Escape leaves the search for the whole tree, selected where it was" $
            withServer $ \st -> do
                searchable st
                run st ["send-keys", "-t", "work:0.0", "/", "hat", "Escape"] `shouldReturn` []
                display st "work:0.0" "#{pane_mode}" `shouldReturn` [ROutput "tree-mode"]
                run st ["send-keys", "-t", "work:0.0", "Enter"] `shouldReturn` []
                run st ["show", "-gv", "@chosen"] `shouldReturn` [ROutput "=work:0."]
        it "the search prompt shows a quoted control key as ^ and its letter" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["choose-tree", "-t", "S:0.0"] `shouldReturn` []
                run st ["send-keys", "-t", "S:0.0", "/", "a", "C-v", "C-g", "b"] `shouldReturn` []
                cells <- treeCells st
                any (T.isInfixOf "a^Gb" . rowText) cells `shouldBe` True
        it "choose-client has nothing to show without a client" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["choose-client", "-t", "S:0.0"] `shouldReturn` []
                display st "S:0.0" "#{pane_in_mode}" `shouldReturn` [ROutput "0"]

    describe "tree-mode options" $ do
        it "tree-mode-border-style draws the preview box" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["set", "-g", "tree-mode-border-style", "fg=red"] `shouldReturn` []
                run st ["choose-tree", "-t", "S:0.0"] `shouldReturn` []
                cells <- treeCells st
                styleOf '┌' cells `shouldBe` Just Cell.defaultStyle { Cell.fg = Cell.Indexed 1 }
        it "tree-mode-selection-style draws the selected line" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["set", "-g", "tree-mode-selection-style", "bg=blue"] `shouldReturn` []
                run st ["choose-tree", "-t", "S:0.0"] `shouldReturn` []
                cells <- treeCells st
                map (fmap (.style.bg) . listToMaybe) (take 2 cells)
                    `shouldBe` [Just Cell.DefaultColor, Just (Cell.Indexed 4)]
        it "tree-mode-preview-format (a pane's own) and -style label each previewed pane" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                [pane] <- paneIds st "S:0"
                run st ["set", "-p", "-t", pane, "tree-mode-preview-format", "L#{pane_index}"] `shouldReturn` []
                run st ["set", "-g", "tree-mode-preview-style", "fg=green"] `shouldReturn` []
                run st ["set", "-g", "tree-mode-border-style", "bg=black"] `shouldReturn` []
                run st ["choose-tree", "-t", "S:0.0"] `shouldReturn` []
                cells <- treeCells st
                any (T.isInfixOf "│ L0 │" . rowText) cells `shouldBe` True
                styleOf 'L' cells `shouldBe`
                    Just Cell.defaultStyle
                        { Cell.fg = Cell.Indexed 2, Cell.bg = Cell.Indexed 0 }

    describe "panes-mode" $ do
        let twoPanes st = do
                run st ["new-session", "-d", "-s", "S", "-x", "80", "-y", "24", "cat"]
                    `shouldReturn` []
                run st ["split-window", "-t", "S:0", "cat"] `shouldReturn` []
                paneIds st "S:0"
        it "display-panes zooms its pane into panes-mode until q" $
            withServer $ \st -> do
                [a, _] <- twoPanes st
                run st ["display-panes", "-d", "0", "-t", a] `shouldReturn` []
                display st a "#{pane_mode} #{window_zoomed_flag}"
                    `shouldReturn` [ROutput "panes-mode 1"]
                run st ["send-keys", "-t", a, "q"] `shouldReturn` []
                display st a "#{pane_in_mode} #{window_zoomed_flag}"
                    `shouldReturn` [ROutput "0 0"]
        it "-Z leaves the window unzoomed" $
            withServer $ \st -> do
                [a, _] <- twoPanes st
                run st ["display-panes", "-Z", "-d", "0", "-t", a] `shouldReturn` []
                display st a "#{pane_mode} #{window_zoomed_flag}"
                    `shouldReturn` [ROutput "panes-mode 0"]
        it "a zoom the window already had outlives the mode" $
            withServer $ \st -> do
                [a, _] <- twoPanes st
                run st ["resize-pane", "-Z", "-t", a] `shouldReturn` []
                run st ["display-panes", "-d", "0", "-t", a] `shouldReturn` []
                run st ["send-keys", "-t", a, "q"] `shouldReturn` []
                display st a "#{pane_in_mode} #{window_zoomed_flag}"
                    `shouldReturn` [ROutput "0 1"]
        it "a pane's number runs the template on it and leaves the mode" $
            withServer $ \st -> do
                [a, b] <- twoPanes st
                run st ["display-panes", "-d", "0", "-t", a, "set -g @picked %%"]
                    `shouldReturn` []
                run st ["send-keys", "-t", a, "1"] `shouldReturn` []
                display st a "#{pane_in_mode} #{window_zoomed_flag}"
                    `shouldReturn` [ROutput "0 0"]
                run st ["show", "-gv", "@picked"] `shouldReturn` [ROutput b]
        it "selects the pane by default" $
            withServer $ \st -> do
                [a, b] <- twoPanes st
                run st ["display-panes", "-d", "0", "-t", a] `shouldReturn` []
                run st ["send-keys", "-t", a, "1"] `shouldReturn` []
                display st "S:0" "#{pane_id}" `shouldReturn` [ROutput b]
        it "any other key leaves the mode, unless -N" $
            withServer $ \st -> do
                [a, _] <- twoPanes st
                run st ["display-panes", "-N", "-d", "0", "-t", a] `shouldReturn` []
                run st ["send-keys", "-t", a, "x", "7"] `shouldReturn` []
                display st a "#{pane_mode}" `shouldReturn` [ROutput "panes-mode"]
                run st ["send-keys", "-t", a, "Escape"] `shouldReturn` []
                run st ["display-panes", "-d", "0", "-t", a] `shouldReturn` []
                run st ["send-keys", "-t", a, "x"] `shouldReturn` []
                display st a "#{pane_in_mode}" `shouldReturn` [ROutput "0"]
        it "ends once its duration passes" $
            withServer $ \st -> do
                [a, _] <- twoPanes st
                run st ["display-panes", "-d", "50", "-t", a] `shouldReturn` []
                display st a "#{pane_mode}" `shouldReturn` [ROutput "panes-mode"]
                awaitOutput st a "#{pane_in_mode} #{window_zoomed_flag}" "0 0"
        it "rejects a malformed duration" $
            withServer $ \st -> do
                [a, _] <- twoPanes st
                run st ["display-panes", "-d", "soon", "-t", a]
                    `shouldReturn` [RErr "delay invalid"]
                display st a "#{pane_in_mode}" `shouldReturn` [ROutput "0"]
        it "stacks over copy mode and returns to it" $
            withServer $ \st -> do
                [a, _] <- twoPanes st
                run st ["copy-mode", "-t", a] `shouldReturn` []
                run st ["display-panes", "-d", "0", "-t", a] `shouldReturn` []
                run st ["send-keys", "-t", a, "1"] `shouldReturn` []
                display st a "#{pane_mode}" `shouldReturn` [ROutput "copy-mode"]
        it "is not kept under another mode" $
            withServer $ \st -> do
                [a, _] <- twoPanes st
                run st ["display-panes", "-d", "0", "-t", a] `shouldReturn` []
                run st ["copy-mode", "-t", a] `shouldReturn` []
                run st ["send-keys", "-t", a, "-X", "cancel"] `shouldReturn` []
                display st a "#{pane_in_mode} #{window_zoomed_flag}"
                    `shouldReturn` [ROutput "0 0"]
        it "-k kills the pane when the mode exits" $
            withServer $ \st -> do
                [a, b] <- twoPanes st
                run st ["display-panes", "-k", "-d", "0", "-t", b] `shouldReturn` []
                run st ["send-keys", "-t", b, "q"] `shouldReturn` []
                paneIds st "S:0" `shouldReturn` [a]
        it "-k kills the pane when another mode replaces it" $
            withServer $ \st -> do
                [a, b] <- twoPanes st
                run st ["display-panes", "-k", "-d", "0", "-t", b] `shouldReturn` []
                run st ["copy-mode", "-t", b] `shouldReturn` []
                paneIds st "S:0" `shouldReturn` [a]
        it "-s numbers the panes of another window" $
            withServer $ \st -> do
                [a, _] <- twoPanes st
                run st ["new-window", "-d", "-t", "S:1", "cat"] `shouldReturn` []
                run st ["split-window", "-t", "S:1", "cat"] `shouldReturn` []
                [_, d] <- paneIds st "S:1"
                run st ["display-panes", "-d", "0", "-s", "S:1", "-t", a, "set -g @picked %%"]
                    `shouldReturn` []
                run st ["send-keys", "-t", a, "1"] `shouldReturn` []
                run st ["show", "-gv", "@picked"] `shouldReturn` [ROutput d]
        it "draws the window's panes, numbered, between display-panes-border-style borders" $
            withServer $ \st -> do
                [a, _] <- twoPanes st
                run st ["set", "-g", "display-panes-format", "P#{pane_index}"] `shouldReturn` []
                run st ["set", "-g", "display-panes-border-style", "fg=green"] `shouldReturn` []
                run st ["display-panes", "-d", "0", "-t", a] `shouldReturn` []
                rows <- panesRows st a
                map (T.take 2 . rowText) [rows !! 0, rows !! 13] `shouldBe` ["P0", "P1"]
                map Cell.baseChar (take 3 (rows !! 12)) `shouldBe` "───"
                (.style.fg) <$> listToMaybe (rows !! 12)
                    `shouldBe` Just (Cell.Indexed 2)
        it "frames a floating pane" $
            withServer $ \st -> do
                [a, _] <- twoPanes st
                run st ["new-pane", "-d", "-B", "none", "-x", "30", "-y", "8", "-X", "8", "-Y", "3"
                       , "-t", "S:0", "cat"] `shouldReturn` []
                run st ["set", "-g", "display-panes-format", ""] `shouldReturn` []
                run st ["display-panes", "-d", "0", "-t", a] `shouldReturn` []
                rows <- panesRows st a
                T.unpack (T.take 3 (T.drop 7 (rowText (rows !! 2)))) `shouldBe` "┌──"
                Cell.baseChar (rows !! 11 !! 38) `shouldBe` '┘'

    describe "dropModesForReload" $
        it "clears every mode and undoes a -Z zoom, keeping -k panes" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["split-window", "-t", "S:0", "cat"] `shouldReturn` []
                [a, b] <- paneIds st "S:0"
                run st ["choose-tree", "-k", "-Z", "-t", b] `shouldReturn` []
                display st b "#{window_zoomed_flag}" `shouldReturn` [ROutput "1"]
                dropModesForReload st
                display st b "#{pane_in_mode} #{window_zoomed_flag}"
                    `shouldReturn` [ROutput "0 0"]
                paneIds st "S:0" `shouldReturn` [a, b]

    describe "target clients" $ do
        it "switch-client -c names an unknown client loudly" $
            withServer $ \st -> do
                run st ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                run st ["switch-client", "-c", "client-99", "-t", "S"]
                    `shouldReturn` [RErr "can't find client: client-99"]
        it "detach-client -t names an unknown client loudly" $
            withServer $ \st ->
                run st ["detach-client", "-t", "client-99"]
                    `shouldReturn` [RErr "can't find client: client-99"]

-- | Sessions @work@ (windows @chat@, @mail@) and @projects@ (@notes@,
-- @hat@), and a choose-tree -w on @work:0.0@ whose choice sets @\@chosen@.
searchable :: ServerState -> IO ()
searchable st = do
    run st ["new-session", "-d", "-s", "work", "-n", "chat", "cat"] `shouldReturn` []
    run st ["new-window", "-d", "-t", "work:1", "-n", "mail", "cat"] `shouldReturn` []
    run st ["new-session", "-d", "-s", "projects", "-n", "notes", "cat"] `shouldReturn` []
    run st ["new-window", "-d", "-t", "projects:1", "-n", "hat", "cat"] `shouldReturn` []
    run st ["choose-tree", "-w", "-t", "work:0.0", "set -g @chosen '%%'"] `shouldReturn` []

-- | What the server's only pane's tree mode draws, row by row.
treeCells :: ServerState -> IO [[Cell.Cell]]
treeCells st = do
    [p] <- allPanes st
    Just (TreeMode ts) <- atomically (topMode p)
    size <- readTVarIO p.size
    (cells, _) <- drawTreeMode st ts size
    pure (map V.toList (V.toList cells))

-- | What a pane's panes-mode draws, row by row.
panesRows :: ServerState -> Text -> IO [[Cell.Cell]]
panesRows st pid = do
    ps <- allPanes st
    [p] <- pure [ p | p <- ps, "%" <> T.pack (show (rawPane p.id)) == pid ]
    Just (PanesMode ps') <- atomically (topMode p)
    cells <- drawPanesMode st p ps'
    pure (map V.toList (V.toList cells))

-- | Poll a format on a target until it expands to @want@, for up to two
-- seconds.
awaitOutput :: ServerState -> Text -> Text -> Text -> IO ()
awaitOutput st tgt fmt want = do
    r <- timeout 2_000_000 loop
    case r of
        Just () -> pure ()
        Nothing -> display st tgt fmt `shouldReturn` [ROutput want]
  where
    loop = do
        got <- display st tgt fmt
        if got == [ROutput want] then pure () else threadDelay 10_000 >> loop

-- | The style of the first cell drawing a character.
styleOf :: Char -> [[Cell.Cell]] -> Maybe Cell.Style
styleOf c rows = listToMaybe [ cell.style | row <- rows, cell <- row, Cell.baseChar cell == c ]

rowText :: [Cell.Cell] -> Text
rowText = T.pack . map Cell.baseChar

-- | A window's pane ids, in pane-index order.
paneIds :: ServerState -> Text -> IO [Text]
paneIds st tgt = do
    out <- run st ["list-panes", "-t", tgt, "-F", "#{pane_id}"]
    pure [ t | ROutput t <- out ]
