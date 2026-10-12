-- | @display-popup@ (tmux's @cmd-display-menu.c@): open a popup running a
-- command on the target client, change the one it shows, or close it.
module Hat.Server.Command.Popup
    ( cmdDisplayPopup
    ) where

import Control.Concurrent.STM
import Control.Exception (IOException, bracket_, try)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.ArgNumber (stringPercentage)
import Hat.Server.Box (BoxLines (..), boxLinesOf)
import Hat.Server.Command.Menu (targetStatus)
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs, unknownFlag)
import Hat.Server.DisplayPos
import Hat.Server.FormatEnv (clientFormatVars, expandFormat, paneFormatEnv)
import Hat.Server.Locate (findTarget, findTargetClient, paneIndexOf)
import Hat.Server.Mouse (MouseEvent (..), MouseInput (..))
import Hat.Server.MouseIO (clientMouseEvent, mouseFormatVars)
import Hat.Server.Pane (paneShell, programEnv, sessionSpawnEnv)
import Hat.Server.Popup
import Hat.Server.PopupIO (awaitPopupClosed, closePopup, openPopup)
import Hat.Server.Resize (windowArrange)
import Hat.Server.Target qualified as Target

-- | @display-popup [-BCEkN] [-b border-lines] [-c target-client]
-- [-d start-directory] [-e environment] [-h height] [-s style]
-- [-S border-style] [-t target-pane] [-T title] [-w width] [-x position]
-- [-y position] [shell-command [argument ...]]@: a command's popup on the
-- target client; on a client already showing one, its look and flags
-- change instead; @-C@ closes it. A command client waits for the popup to
-- close; an attached one carries on.
cmdDisplayPopup :: CommandImpl
cmdDisplayPopup st mclient args
    | Just e <- unknownFlag "display-popup" "BCEkN" flags = pure [RErr e]
    | otherwise = do
        res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (sess, wix, win, pane) -> do
                mtc <- findTargetClient st mclient (Just sess.id) (lookup "-c" opts)
                case mtc of
                    Left e -> pure [RErr e]
                    Right tc
                        | "-C" `elem` flags -> do
                            mp <- readTVarIO tc.popup
                            [] <$ mapM_ (closePopup st tc) mp
                        | otherwise -> popup sess wix win pane tc
  where
    (optsRev, flags, pos) = parseArgs "bcdehsStTwxy" args
    opts = reverse optsRev
    exits = length (filter (== "-E") flags)
    lineChoice
        | "-B" `elem` flags = Right (Just BoxNone)
        | Just v <- lookup "-b" opts =
            maybe (Left ("popup-border-lines unknown value: " <> v)) (Right . Just) (boxLinesOf v)
        | otherwise = Right Nothing

    popup sess wix win pane tc = do
        mexisting <- readTVarIO tc.popup
        env <- formatEnv sess wix win pane tc
        tsize <- readTVarIO tc.size
        let tsx = fromIntegral tsize.cols
            tsy = fromIntegral tsize.rows
            size flag name cur = case lookup flag opts of
                Nothing -> Right (cur `div` 2)
                Just v -> either (\c -> Left (name <> " " <> c)) Right
                    (stringPercentage 1 cur cur v)
        case mexisting of
            Just p -> case lineChoice of
                Left e -> pure [RErr e]
                Right mlines -> do
                    title <- maybe (pure "") (expandFormat st env) (lookup "-T" opts)
                    modifyPopup p title mlines
                    pure []
            Nothing -> case (size "-h" "height" tsy, size "-w" "width" tsx, lineChoice) of
                (Left e, _, _) -> pure [RErr e]
                (_, Left e, _) -> pure [RErr e]
                (_, _, Left e) -> pure [RErr e]
                (Right h0, Right w0, Right mlines) -> do
                    let w = min w0 tsx
                        h = min h0 tsy
                    (x, y) <- position sess win pane tc env w h
                    title <- maybe (pure "") (expandFormat st env) (lookup "-T" opts)
                    lns <- case mlines of
                        Just l -> pure l
                        Nothing -> do
                            wopts <- resolveForWindowIO st sess win
                            pure (fromMaybe BoxSingle
                                (boxLinesOf (optionText "popup-border-lines" wopts)))
                    let fits = if lns == BoxNone then w >= 1 && h >= 1 else w >= 3 && h >= 3
                        ps = (newPopupState x y w h lns title)
                            { style = lookup "-s" opts, borderStyle = lookup "-S" opts
                            , onExit = exitFlag StaysOpen, keys = keysFlag KeysToJob }
                    if not fits || tsx < w || tsy < h then pure [] else
                        spawn sess tc env ps

    exitFlag dflt
        | exits > 1 = CloseOnSuccess
        | exits == 1 = CloseOnExit
        | otherwise = dflt
    keysFlag dflt = if "-k" `elem` flags then AnyKeyCloses else dflt

    -- tmux's popup_modify: -N, or any of -E and -k, resets the flags.
    modifyPopup p title mlines = atomically $ do
        ps <- readTVar p.state
        let reset = "-N" `elem` flags || exits > 0 || "-k" `elem` flags
            ps1 = ps { title
                     , style = maybe ps.style Just (lookup "-s" opts)
                     , borderStyle = maybe ps.borderStyle Just (lookup "-S" opts)
                     , lines = fromMaybe ps.lines mlines }
            ps2 = if reset
                then ps1 { onExit = exitFlag StaysOpen, keys = keysFlag KeysToJob }
                else ps1
        writeTVar p.state ps2
        bumpDirty st

    formatEnv sess wix win pane tc = do
        mev <- clientMouseEvent mclient
        pix <- paneIndexOf st win pane
        penv <- paneFormatEnv st sess wix win pix pane
        mouseVars <- case (,) <$> mclient <*> mev of
            Just (c, ev) -> mouseFormatVars st c ev
            Nothing -> pure []
        cvars <- clientFormatVars st tc
        pure (Map.unions [Map.fromList mouseVars, Map.fromList cvars, penv])

    position sess win pane tc env w h = do
        mev <- clientMouseEvent mclient
        eff <- readTVarIO sess.lastSize
        (rects, _) <- atomically (windowArrange st eff win)
        tsize <- readTVarIO tc.size
        lastAt <- readTVarIO win.menuLast
        mstatus <- targetStatus st tc win
        let tsx = fromIntegral tsize.cols
            tsy = fromIntegral tsize.rows
            paneAt = case List.lookup pane.id rects of
                Just r -> PaneAt { xoff = r.startCol, yoff = r.startRow
                                 , sx = r.endCol - r.startCol, sy = r.endRow - r.startRow }
                Nothing -> PaneAt 0 0 tsx tsy
            mouseAt = (\ev -> MouseAt { x = ev.input.x, y = ev.input.y
                                      , statusAt = ev.statusAt, statusLines = ev.statusLines }) <$> mev
            vars = popupPosVars w h tsx tsy mouseAt lastAt mstatus paneAt
            env' = Map.union vars env
        xs <- expandFormat st env' (positionFormat AxisX (lookup "-x" opts))
        ys <- expandFormat st env' (positionFormat AxisY (lookup "-y" opts))
        pure (popupPlaceX tsx w xs, popupPlaceY tsy h ys)

    spawn sess tc env ps = do
        sopts <- resolveForSessionIO st sess
        cwd <- case lookup "-d" opts of
            Just d -> T.unpack <$> expandFormat st env d
            Nothing -> readTVarIO sess.startCwd
        shell <- paneShell sopts
        let shellCmd = case pos of
                [] -> Just (optionText "default-command" sopts)
                [c] -> Just c
                _ -> Nothing
            (cmd, cargs) = case shellCmd of
                Just c | T.null c -> (shell, [])
                       | otherwise -> (shell, ["-c", T.unpack c])
                Nothing -> case pos of
                    p : rest -> (T.unpack p, map T.unpack rest)
                    [] -> (shell, [])
        environ <- sessionSpawnEnv st sess
        let extra = [ (k, T.drop 1 v) | ("-e", kv) <- opts, let (k, v) = T.breakOn "=" kv ]
        penv <- programEnv st sess.id Nothing shell (environ <> extra)
        opened <- try (openPopup st tc ps cmd cargs cwd penv)
        case opened of
            Left (_ :: IOException) -> pure []
            Right p
                | waits -> [] <$ outsideBatch (atomically (awaitPopupClosed p))
                | otherwise -> pure []
    -- The wait leaves the command batch, so the screen keeps drawing.
    outsideBatch = bracket_
        (atomically (modifyTVar' st.commandDepth (subtract 1)))
        (atomically (modifyTVar' st.commandDepth (+ 1)))
    -- Only a command client can wait: an attached client's input drives the
    -- popup it would wait on.
    waits = maybe False (\c -> c.role == Control) mclient
