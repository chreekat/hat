-- | The interactive client commands: detaching, sending keys to a pane,
-- entering copy-mode, the command prompt, and the choose-tree\/choose-window
-- pickers.
module Hat.Server.Command.Interact
    ( cmdDetachClient
    , cmdSendPrefix
    , cmdCopyMode
    , cmdCommandPrompt
    , cmdChooseTree
    , cmdChooseWindow
    , buildTreeNodes
    , buildWindowItems
    ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.ClientIO (send)
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import qualified Hat.Server.CopyMode as CopyMode
import Hat.Server.FormatEnv (windowFormatEnv)
import Hat.Server.Format (FormatEnv)
import Hat.Server.Keys
import Hat.Server.Locate (clientActivePane, clientView, targetPane)
import qualified Hat.Server.Picker as Picker
import qualified Hat.Server.Prompt as Prompt
import Hat.Server.FormatEnv (expandFormat)
import qualified Hat.Term.Emulator as Emu
import qualified Hat.Term.Pty
import Hat.Transport.Wire

cmdDetachClient :: CommandImpl
cmdDetachClient _ mclient _ = do
    forM_ mclient $ \client -> send client DetachOk
    pure []

cmdSendPrefix :: CommandImpl
cmdSendPrefix st mclient _ = do
    forM_ mclient $ \client -> do
        opts <- readTVarIO st.options
        forM_ (parseKeyName opts.prefix) $ \key -> do
            mpane <- clientActivePane st client
            forM_ mpane $ \pane -> Hat.Term.Pty.writePty pane.pty key.raw
    pure []

cmdCopyMode :: CommandImpl
cmdCopyMode st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
        quit = "-q" `elem` flags
    mpane <- targetPane st mclient (lookup "-t" opts)
    case mpane of
        Nothing -> pure []
        Just pane
            | quit -> do
                atomically $ do
                    writeTVar pane.mode Nothing
                    bumpDirty st
                pure []
            | otherwise -> do
                scr <- Emu.snapshot pane.emulator
                frozen <- CopyMode.freezeGrid pane.emulator
                srvOpts <- readTVarIO st.options
                let table = case srvOpts.modeKeys of
                        KeysVi -> "copy-mode-vi"
                        KeysEmacs -> "copy-mode"
                    startRow = frozen.fgHsize + scr.cursor.row
                    startCol = scr.cursor.col
                    state = CopyModeState
                        { cursorRow = startRow
                        , cursorCol = startCol
                        , selection = Nothing
                        , keyTable = table
                        , viewportOffY = 0
                        , numPrefix = Nothing
                        , pendingSearch = Nothing
                        , lastSearch = Nothing
                        , lastQuery = Nothing
                        }
                atomically $ do
                    writeTVar pane.mode (Just PaneMode
                        { frozen = frozen, copyState = state })
                    bumpDirty st
                pure []

-- | Open the interactive command prompt on the invoking client.
-- | @command-prompt [-I initial] [-p prompt] [template]@. Opens the line
-- editor; @-I@ pre-fills it (format-expanded, so @#W@ is the window name),
-- and a @template@ has the submitted line spliced in for @%%@. Backs the
-- @,@ rename binding: @command-prompt -I "#W" "rename-window '%%'"@.
cmdCommandPrompt :: CommandImpl
cmdCommandPrompt st mclient args = do
    let (opts, _flags, pos) = parseArgs "Ip" args
        tmpl = T.unwords pos
        pfx = case lookup "-p" opts of
            Just p -> p
            Nothing
                | T.null tmpl -> ":"
                | otherwise -> "(" <> T.takeWhile (/= ' ') tmpl <> ") "
    forM_ mclient $ \client -> do
        initial <- case lookup "-I" opts of
            Nothing -> pure ""
            Just raw -> do
                env <- clientPromptEnv st client
                expandFormat st env raw
        atomically $ do
            writeTVar client.prompt (Just (Prompt.promptFor pfx initial tmpl))
            bumpDirty st
    pure []

-- | The format environment of a client's current window, for expanding a
-- @command-prompt -I@ initial string. Empty if the client has no window.
clientPromptEnv :: ServerState -> Client -> IO FormatEnv
clientPromptEnv st client = do
    mv <- atomically (clientView st client)
    case mv of
        Nothing -> pure Map.empty
        Just (sess, win) -> do
            ix <- readTVarIO sess.currentIx
            windowFormatEnv st sess ix win

-- | Open a chooser overlay on the invoking client.
openPicker :: ServerState -> Client -> Text -> PickerFill -> [PickerNode] -> IO ()
openPicker st client titleText fill picked = atomically $ do
    writeTVar client.picker $ Just PickerState
        { title = titleText
        , roots = picked
        , cursor = 0
        , query = ""
        , search = ""
        , mode = Browsing
        , fill = fill
        }
    bumpDirty st

-- | @choose-tree [-GswZ]@: a filterable tree of every session, its
-- windows and their panes; Enter switches to the chosen one. @-s@ opens
-- with sessions collapsed (sessions only), @-w@ with windows expanded but
-- panes collapsed, and neither fully expanded. The config opens it with
-- @… \; send-keys /@ to jump straight into search.
cmdChooseTree :: CommandImpl
cmdChooseTree st mclient args = do
    let (_, flags, _) = parseArgs "" args
        sessionsOnly = "-s" `elem` flags
        windowsOnly  = "-w" `elem` flags
        windowsExp = if sessionsOnly then Collapsed else Expanded
        panesExp   = if sessionsOnly || windowsOnly then Collapsed else Expanded
        fill = if "-Z" `elem` flags then FillWindow else PaneRegion
    forM_ mclient $ \client -> do
        picked <- buildTreeNodes st windowsExp panesExp
        openPicker st client "choose a window" fill picked
    pure []

-- Panes are shown under their window for visual context, but tmux can't name
-- them, so they carry no meaningful search text; the picker marks them
-- (via 'PreviewPane') as non-matching so search never targets them.
buildTreeNodes :: ServerState -> Expansion -> Expansion -> IO [PickerNode]
buildTreeNodes st windowsExp panesExp = do
    sessions <- Map.elems <$> readTVarIO st.sessions
    forM sessions $ \sess -> do
        sname <- readTVarIO sess.name
        ws <- Map.toAscList <$> readTVarIO sess.windows
        winNodes <- forM ws $ \(ix, win) -> do
            wname <- readTVarIO win.name
            apid <- readTVarIO win.activeId
            ordered <- Map.elems <$> readTVarIO win.panes
            let winCmd = "switch-client -t " <> sname
                    <> " ; select-window -t " <> sname <> ":" <> tshow ix
                paneNodes =
                    [ PickerNode
                        { label = "pane " <> tshow pix
                            <> (if pane.id == apid then "*" else "")
                        , command = winCmd <> " ; select-pane -t " <> tshow pix
                        , preview = Just (PreviewPane pane.id)
                        , children = []
                        , expanded = Collapsed }
                    | (pix, pane) <- zip [0 :: Int ..] ordered ]
            pure PickerNode
                { label = tshow ix <> ":" <> wname
                , command = winCmd
                , preview = Just (PreviewWindow win.id)
                , children = Picker.windowChildren paneNodes
                , expanded = panesExp }
        pure PickerNode
            { label = sname
            , command = "switch-client -t " <> sname
            , preview = Just (PreviewSession sess.id)
            , children = winNodes
            , expanded = windowsExp }

-- | @choose-window <template>@: a list of the current session's windows;
-- selecting one runs @template@ with each @%%@ replaced by that window's
-- active pane id, so @choose-window 'join-pane -hs \"%%\"'@ joins it here.
cmdChooseWindow :: CommandImpl
cmdChooseWindow st mclient args = do
    let (_, _, pos) = parseArgs "" args
    case (mclient, pos) of
        (Just client, template : _) -> do
            picked <- buildWindowItems st client template
            openPicker st client "choose a window" PaneRegion picked
            pure []
        _ -> pure [RErr "usage: choose-window template"]

buildWindowItems :: ServerState -> Client -> Text -> IO [PickerNode]
buildWindowItems st client template = do
    sid <- readTVarIO client.session
    msess <- Map.lookup sid <$> readTVarIO st.sessions
    case msess of
        Nothing -> pure []
        Just sess -> do
            ws <- Map.toAscList <$> readTVarIO sess.windows
            forM ws $ \(ix, win) -> do
                wname <- readTVarIO win.name
                apid <- readTVarIO win.activeId
                let target = "%" <> tshow (rawPane apid)
                pure $ Picker.leaf (tshow ix <> ":" <> wname)
                    (T.replace "%%" target template)
