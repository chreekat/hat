-- | @display-menu@ (tmux's @cmd-display-menu.c@): build a menu from its
-- arguments and show it over the target pane's window.
module Hat.Server.Command.Menu
    ( cmdDisplayMenu
    , targetStatus
    ) where

import Control.Concurrent.STM
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Vector qualified as V

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.Box (BoxLines (..), boxLinesOf)
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs, strtonum, unknownFlag)
import Hat.Server.DisplayPos
import Hat.Server.FormatDraw qualified as FD
import Hat.Server.FormatEnv (clientFormatVars, expandFormat, paneFormatEnv)
import Hat.Server.Keys (Key (..), parseKeyName)
import Hat.Server.Locate (findTarget, findTargetClient, paneIndexOf)
import Hat.Server.Menu
import Hat.Server.Mouse (MouseEvent (..), MouseInput (..))
import Hat.Server.MouseIO (clientMouseEvent, mouseFormatVars)
import Hat.Server.Resize (windowArrange)
import Hat.Server.Target qualified as Target
import Hat.Server.View (statusLayout, statusRangesFor)

-- | @display-menu [-MO] [-b border-lines] [-c target-client]
-- [-C starting-choice] [-H selected-style] [-s style] [-S border-style]
-- [-t target-pane] [-T title] [-x position] [-y position]
-- name [key] [command] ...@: items are a name, a key and a command, or an
-- empty name for a separator; the menu opens over the target pane's window
-- at a position @-x@\/@-y@ name, on the target client's terms.
cmdDisplayMenu :: CommandImpl
cmdDisplayMenu st mclient args
    | Just e <- unknownFlag "display-menu" "MO" flags = pure [RErr e]
    | null pos = pure [RErr "command display-menu: too few arguments (need at least 1)"]
    | Left e <- starting = pure [RErr e]
    | Left e <- lineChoice = pure [RErr e]
    | otherwise = do
        res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (sess, wix, win, pane) -> do
                mtc <- findTargetClient st mclient (Just sess.id) (lookup "-c" opts)
                case mtc of
                    Left e -> pure [RErr e]
                    Right tc -> display sess wix win pane tc
  where
    (optsRev, flags, pos) = parseArgs "bcCHsStTxy" args
    opts = reverse optsRev
    starting = case lookup "-C" opts of
        Nothing -> Right 0
        Just "-" -> Right (-1)
        Just v -> either (\c -> Left ("starting choice " <> c)) Right
            (strtonum 0 maxBound v)
    lineChoice = case lookup "-b" opts of
        Nothing -> Right Nothing
        Just v -> maybe (Left ("menu-border-lines unknown value: " <> v)) (Right . Just)
            (boxLinesOf v)

    display sess wix win pane tc = do
        mev <- clientMouseEvent mclient
        env <- do
            pix <- paneIndexOf st win pane
            penv <- paneFormatEnv st sess wix win pix pane
            mouseVars <- case (,) <$> mclient <*> mev of
                Just (c, ev) -> mouseFormatVars st c ev
                Nothing -> pure []
            cvars <- clientFormatVars st tc
            pure (Map.unions [Map.fromList mouseVars, Map.fromList cvars, penv])
        title <- maybe (pure "") (expandFormat st env) (lookup "-T" opts)
        tsize <- readTVarIO tc.size
        built <- buildItems (expandFormat st env) (fromIntegral tsize.cols) (menuCreate title) pos
        case built of
            Left e -> pure [RErr e]
            Right menu
                | null menu.items -> pure []
                | otherwise -> do
                    (x, y) <- menuPosition win pane tc mev env menu
                    wopts <- resolveForWindowIO st sess win
                    eff <- readTVarIO win.size
                    let wsx = fromIntegral eff.cols
                        wsy = fromIntegral eff.rows
                        (px, py) = placeMenu wsx wsy menu (x, y)
                        mouse = if isJust mev || "-M" `elem` flags then MenuMouseOn else MenuNoMouse
                        lns = case either (const Nothing) id lineChoice of
                            Just l -> l
                            Nothing -> maybe BoxSingle id
                                (boxLinesOf (optionText "menu-border-lines" wopts))
                        start = either (const 0) id starting
                        wm = WindowMenu
                            { menu, choice = startingChoice mouse start menu
                            , px, py
                            , stayOpen = if "-O" `elem` flags then StayOpen else CloseOnChoice
                            , mouse, lines = lns
                            , style = lookup "-s" opts
                            , selectedStyle = lookup "-H" opts
                            , borderStyle = lookup "-S" opts
                            , target = (sess.id, win.id, pane.id)
                            , event = mev }
                    atomically $ do
                        writeTVar win.menuLast (px, py)
                        writeTVar win.menu (Just wm)
                        bumpDirty st
                    pure []

    menuPosition win pane tc mev env menu = do
        (rects, _) <- atomically $ readTVar win.size >>= \eff -> windowArrange st eff win
        eff <- readTVarIO win.size
        tsize <- readTVarIO tc.size
        lastAt <- readTVarIO win.menuLast
        mstatus <- targetStatus st tc win
        let w = menuWidth menu
            h = menuHeight menu
            wsx = fromIntegral eff.cols
            wsy = fromIntegral eff.rows
            statusRows = maybe 0 (.lines) mstatus
            visible = min wsy (fromIntegral tsize.rows - statusRows)
            paneAt = case List.lookup pane.id rects of
                Just r -> PaneAt { xoff = r.startCol, yoff = r.startRow
                                 , sx = r.endCol - r.startCol, sy = r.endRow - r.startRow }
                Nothing -> PaneAt 0 0 wsx wsy
            mouseAt = (\ev -> MouseAt { x = ev.input.x, y = ev.input.y
                                      , statusAt = ev.statusAt, statusLines = ev.statusLines }) <$> mev
            vars = menuPosVars w h wsx wsy visible mouseAt lastAt mstatus paneAt
            env' = Map.union vars env
        xs <- expandFormat st env' (positionFormat AxisX (lookup "-x" opts))
        ys <- expandFormat st env' (positionFormat AxisY (lookup "-y" opts))
        pure (menuPlaceX wsx w xs, menuPlaceY wsy h ys)

-- | Build the items from @display-menu@'s arguments: a name, a key and a
-- command each, or an empty name alone for a separator; names and commands
-- format-expanded.
buildItems :: (Text -> IO Text) -> Int -> Menu -> [Text] -> IO (Either Text Menu)
buildItems expand cols = go
  where
    go menu = \case
        [] -> pure (Right menu)
        "" : rest -> go (menuAddItem cols Nothing menu) rest
        name : k : command : rest -> do
            n <- expand name
            c <- expand command
            let spec = ItemSpec { name = n, key = (.name) <$> parseKeyName k, command = c }
            go (menuAddItem cols (Just spec) menu) rest
        _ -> pure (Left "not enough arguments")

-- | The target client's status line as a menu or popup places itself
-- against it: where it is, how many lines, and where the window's entry on
-- it starts. 'Nothing' when the client shows none.
targetStatus :: ServerState -> Client -> Window -> IO (Maybe StatusAt)
targetStatus st tc win = do
    msess <- atomically $ do
        sid <- readTVar tc.session
        Map.lookup sid <$> readTVar st.sessions
    case msess of
        Nothing -> pure Nothing
        Just sess -> do
            sopts <- resolveForSessionIO st sess
            tsize <- readTVarIO tc.size
            ws <- readTVarIO sess.windows
            let (_, mrow) = statusLayout sopts.statusPosition sopts.statusLines
                    (fromIntegral tsize.rows)
                width = fromIntegral tsize.cols
                mix = listToMaybe [ ix | (ix, w) <- Map.toList ws, w.id == win.id ]
            case mrow of
                Nothing -> pure Nothing
                Just _ | sopts.statusLines == 0 -> pure Nothing
                Just _ -> do
                    entries <- traverse (\line -> do
                        ranges <- statusRangesFor st sess sopts width line
                        pure [ (line, col) | Just ix <- [mix]
                             , (col, FD.RangeWindow r) <- zip [0 ..] (V.toList ranges)
                             , r == ix ]) [0 .. sopts.statusLines - 1]
                    pure $ Just StatusAt
                        { atTop = sopts.statusPosition == StatusTop
                        , lines = sopts.statusLines
                        , windowEntry = listToMaybe (concat entries) }
