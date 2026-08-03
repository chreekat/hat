-- | Building the @#{…}@ format environment for a window or a specific pane
-- on top of 'Hat.Server.View.sessionFormatEnv' — the shared substrate the
-- listing, choose, capture, and display commands format against.
module Hat.Server.FormatEnv
    ( windowFormatEnv
    , paneFormatEnv
    , refreshAutoNames
    , autoName
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.Format (FormatEnv)
import Hat.Server.Layout
import Hat.Server.LayoutString (emitLayout)
import Hat.Server.Pane (paneCommandName)
import Hat.Server.View (WindowFlagState (..), expandFormat, sessionFormatEnv, windowFlags)
import qualified Hat.Term.Emulator as Emu
import qualified Hat.Term.Pty

windowFormatEnv :: ServerState -> Session -> Int -> Window -> IO FormatEnv
windowFormatEnv st sess ix win = do
    base <- sessionFormatEnv st sess
    eff <- readTVarIO sess.lastSize
    (wname, lay, cur, mlast, bell, act, auto, zoom) <- atomically $ (,,,,,,,)
        <$> readTVar win.name <*> readTVar win.layout
        <*> readTVar sess.currentIx <*> readTVar sess.lastIx
        <*> readTVar win.bellFlag <*> readTVar win.activity
        <*> readTVar win.autoRename <*> readTVar win.zoomed
    ps <- readTVarIO win.panes
    let flags = windowFlags WindowFlagState
            { flagCurrent = ix == cur
            , flagLast = Just ix == mlast
            , flagBell = bell
            , flagActivity = act
            , flagZoomed = isJust zoom
            }
    pure $ Map.union (Map.fromList
        [ ("window_index", tshow ix)
        , ("window_id", "@" <> tshow (rawWindow win.id))
        , ("window_name", wname)
        , ("window_layout", emitLayout (sizeRect (eff)) lay)
        , ("window_active", if ix == cur then "1" else "0")
        , ("window_flags", flags)
        , ("window_panes", tshow (Map.size ps))
        , ("automatic_rename", if auto then "1" else "0")
        ]) base

-- | The full format environment for a specific pane, including the
-- fields tmux-resurrect's @save.sh@ dumps (pid, command, cursor,
-- history, cwd).
paneFormatEnv
    :: ServerState -> Session -> Int -> Window -> Int -> Pane -> IO FormatEnv
paneFormatEnv st sess wix win pix pane = do
    wenv <- windowFormatEnv st sess wix win
    dir <- paneCurrentPath pane
    cmd <- paneCommandName pane
    scr <- Emu.snapshot pane.emulator
    hsize <- Emu.scrollbackLength pane.emulator
    active <- readTVarIO win.activeId
    sz <- readTVarIO pane.size
    pure $ Map.union (Map.fromList
        [ ("pane_id", "%" <> tshow (rawPane pane.id))
        , ("pane_index", tshow pix)
        , ("pane_pid", tshow (Hat.Term.Pty.pid pane.pty))
        , ("pane_current_path", T.pack dir)
        , ("pane_current_command", cmd)
        , ("pane_active", if pane.id == active then "1" else "0")
        , ("cursor_x", tshow scr.cursor.col)
        , ("cursor_y", tshow scr.cursor.row)
        , ("history_size", tshow hsize)
        , ("pane_width", tshow sz.cols)
        , ("pane_height", tshow sz.rows)
        , ("session_grouped", "0")  -- hat has no session groups
        ]) wenv

refreshAutoNames :: ServerState -> IO ()
refreshAutoNames st = do
    fmt <- (.automaticRenameFormat) <$> readTVarIO st.options
    sessions <- Map.elems <$> readTVarIO st.sessions
    forM_ sessions $ \sess -> do
        ws <- Map.toAscList <$> readTVarIO sess.windows
        forM_ ws $ \(ix, win) -> do
            auto <- readTVarIO win.autoRename
            when auto $ do
                mnew <- autoName st sess ix win fmt
                forM_ mnew $ \newName -> atomically $ do
                    cur <- readTVar win.name
                    when (cur /= newName && not (T.null newName)) $ do
                        writeTVar win.name newName
                        bumpDirty st

autoName :: ServerState -> Session -> Int -> Window -> Text -> IO (Maybe Text)
autoName st sess ix win fmt = do
    mpane <- atomically (activePane win)
    case mpane of
        Nothing -> pure Nothing
        Just pane
            | fmt == "#{pane_current_command}" -> Just <$> paneCommandName pane
            | otherwise -> do
                pbase <- (.paneBaseIndex) <$> readTVarIO st.options
                env <- paneFormatEnv st sess ix win pbase pane
                Just <$> expandFormat st env fmt
