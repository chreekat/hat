-- | A client's popup at work (tmux's @popup.c@ and the job it runs): its
-- command on a pty of its own, the reader feeding that command's output to
-- the popup's screen, keys and mouse reports for it, and closing it — the
-- command hung up and reaped, never left behind.
module Hat.Server.PopupIO
    ( openPopup
    , closePopup
    , awaitPopupClosed
    , closeAllPopups
    , popupKeyInput
    , refitClientPopup
    ) where

import Control.Concurrent (forkIOWithUnmask, killThread, myThreadId)
import Control.Concurrent.STM
import Control.Exception (SomeException, finally, mask_, try)
import Control.Monad (forM_, unless, void, when)
import Data.ByteString qualified as B
import Data.IORef (modifyIORef', readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode (..))
import System.Timeout (timeout)

import Hat.Geometry
import Hat.Model
import Hat.Model.Options (optionText)
import Hat.Server.Keys (Key (..), KeyState (..))
import Hat.Server.Mouse (MouseInput (..), MouseState (..), mouseInputBytes, parseMouseInput)
import Hat.Server.MouseIO (emulatorMouseMode)
import Hat.Server.Pane (keyBytesFor, keyEncoding)
import Hat.Server.Popup
import Hat.Term.Emulator qualified as Emu
import Hat.Term.Pty qualified as Pty

-- | Run a popup's command — the program and arguments, in @cwd@ with
-- @env@ — on a pty the size of the box's inside, and show the popup on the
-- client.
openPopup
    :: ServerState -> Client -> PopupState -> FilePath -> [String] -> FilePath
    -> [(String, String)] -> IO Popup
openPopup st client ps cmd args cwd env = do
    let (jx, jy) = fromMaybe (ps.sx, ps.sy) (innerSize ps)
        sz = Size { rows = fromIntegral jy, cols = fromIntegral jx }
    pty <- Pty.spawn Pty.Spawn { cmd, args, env, cwd = Just cwd, size = sz }
    -- A reload execs over the server; the popup's pty must not outlive it.
    Pty.closeOnExec pty
    emu <- Emu.newEmulator sz 0
    p <- Popup
        <$> newTVarIO ps <*> pure pty <*> pure emu
        <*> newTVarIO JobRunning
        <*> newTVarIO (128 + 1)
        <*> newTVarIO Nothing
        <*> newTVarIO False
    atomically $ do
        writeTVar client.popup (Just p)
        bumpDirty st
    runPopupReader st client p
    pure p

-- | The popup's reader: its command's output onto its screen until the
-- command exits, then the exit recorded (closing the popup if @-E@ asked
-- it to). However it ends, the pty is released and the command reaped.
runPopupReader :: ServerState -> Client -> Popup -> IO ()
runPopupReader st client p = void $ mask_ $ forkIOWithUnmask $ \unmask -> do
    tid <- myThreadId
    atomically $ writeTVar p.reader (Just tid)
    exitVar <- newEmptyTMVarIO
    _ <- forkIOWithUnmask $ \u -> u (Pty.waitExit p.jobPty) >>= atomically . putTMVar exitVar
    rbuf <- Pty.newReadBuf
    let loop = do
            woke <- unmask (Pty.awaitOutputOr p.jobPty (readTMVar exitVar))
            case woke of
                Nothing -> do
                    bs <- unmask (Pty.readAvailBuf p.jobPty rbuf)
                    if B.null bs
                        -- The pty is at its end: only the exit is left.
                        then unmask (atomically (readTMVar exitVar)) >>= exited
                        else feed bs >> loop
                Just status -> do
                    drain rbuf (64 :: Int)
                    exited status
        drain buf n = when (n > 0) $ do
            bs <- Pty.readAvailBuf p.jobPty buf
            unless (B.null bs) (feed bs >> drain buf (n - 1))
    (void (try loop :: IO (Either SomeException ())))
        `finally` release
  where
    feed bs = unless (B.null bs) $ do
        events <- Emu.feed p.screen bs
        forM_ events $ \case
            Emu.Output out -> Pty.writePty p.jobPty out
            _ -> pure ()
        atomically (bumpDirty st)
    -- tmux's popup_job_complete_cb.
    exited mstatus = do
        let code = case mstatus of
                Just (Pty.Exited ExitSuccess) -> 0
                Just (Pty.Exited (ExitFailure n)) -> n
                Just (Pty.Terminated sig _) -> fromIntegral sig
                _ -> 0
        ps <- atomically $ do
            writeTVar p.job JobDone
            writeTVar p.status code
            bumpDirty st
            readTVar p.state
        when (ps.onExit == CloseOnExit || (ps.onExit == CloseOnSuccess && code == 0)) $
            closePopup st client p
    release = do
        Pty.closePty p.jobPty
        reaped <- timeout 1000000 (Pty.waitExit p.jobPty)
        case reaped of
            Just _ -> pure ()
            Nothing -> Pty.signalKill p.jobPty >> void (Pty.waitExit p.jobPty)

-- | Close a popup (tmux's @popup_free_cb@ and @job_free@): off its client,
-- its command sent SIGTERM and hung up, waiters told.
closePopup :: ServerState -> Client -> Popup -> IO ()
closePopup st client p = do
    (first, job, mtid) <- atomically $ do
        was <- readTVar p.closed
        writeTVar p.closed True
        cur <- readTVar client.popup
        case cur of
            Just q | q.closed == p.closed -> writeTVar client.popup Nothing
            _ -> pure ()
        bumpDirty st
        (,,) (not was) <$> readTVar p.job <*> readTVar p.reader
    when (first && job == JobRunning) $ do
        Pty.signalTerm p.jobPty
        Pty.signalHangup p.jobPty
        tid <- myThreadId
        forM_ mtid $ \r -> unless (r == tid) (killThread r)

-- | Block until a popup closes; its status then.
awaitPopupClosed :: Popup -> STM Int
awaitPopupClosed p = do
    readTVar p.closed >>= check
    readTVar p.status

-- | Close every client's popup and wait for its command to be reaped, as
-- an in-place reload must before it execs: nothing it started survives
-- unwatched.
closeAllPopups :: ServerState -> IO ()
closeAllPopups st = do
    cs <- Map.elems <$> readTVarIO st.clients
    forM_ cs $ \c -> do
        mp <- readTVarIO c.popup
        forM_ mp $ \p -> do
            closePopup st c p
            void $ timeout 2000000 (Pty.waitExit p.jobPty)
            Pty.signalKill p.jobPty
            void $ timeout 1000000 (Pty.waitExit p.jobPty)

-- | A key or mouse report while the client shows a popup (tmux's
-- @popup_key_cb@).
popupKeyInput :: ServerState -> Client -> Popup -> Key -> IO ()
popupKeyInput st client p k = do
    csize <- readTVarIO client.size
    job <- readTVarIO p.job
    ks <- readIORef client.keyState
    let tsx = fromIntegral csize.cols
        tsy = fromIntegral csize.rows
        paste = k.name `elem` ["PasteStart", "PasteEnd"] || ks.bracketPasting
    case k.name of
        "PasteStart" -> modifyIORef' client.keyState (\s -> s { bracketPasting = True })
        "PasteEnd" -> modifyIORef' client.keyState (\s -> s { bracketPasting = False })
        _ -> pure ()
    minput <- if k.name /= "Mouse" then pure (Just (PopupKey k.name paste, Nothing)) else
        case parseMouseInput k.raw of
            Nothing -> pure Nothing
            Just m -> do
                let lastAt = (ks.mouse.prevX, ks.mouse.prevY)
                    lb = ks.mouse.prevB
                modifyIORef' client.keyState $ \s ->
                    s { mouse = s.mouse { prevX = m.x, prevY = m.y, prevB = m.b } }
                pure (Just (PopupMouse m.b (m.x, m.y) lb lastAt, Just (m, lb)))
    forM_ minput $ \(input, mreport) -> do
        (old, ps', action) <- atomically $ do
            ps <- readTVar p.state
            let (ps', action) = popupInput tsx tsy job input ps
            writeTVar p.state ps'
            when (ps' /= ps) (bumpDirty st)
            pure (ps, ps', action)
        when (innerSize ps' /= innerSize old) (resizeJob p ps')
        case action of
            PopupTook -> pure ()
            PopupCloses -> closePopup st client p
            PopupKeyToJob -> do
                o <- readTVarIO st.options
                mbytes <- if paste && k.name `notElem` ["PasteStart", "PasteEnd"]
                    then pure (Just k.raw)
                    else if k.name `elem` ["PasteStart", "PasteEnd"]
                        then do
                            on <- Emu.bracketedPaste p.screen
                            pure (if on then Just k.raw else Nothing)
                    -- A focus report reaches only a program that asked for them.
                    else if k.name `elem` ["FocusIn", "FocusOut"]
                        then do
                            ms <- Emu.modes p.screen
                            pure (if ms.focusReport then Just k.raw else Nothing)
                        else keyBytesFor (keyEncoding o) (optionText "backspace" o) p.screen k
                forM_ mbytes (Pty.writePty p.jobPty)
            PopupMouseToJob pos -> forM_ mreport $ \(m, lb) -> do
                mode <- emulatorMouseMode p.screen
                forM_ (mouseInputBytes mode m lb pos) (Pty.writePty p.jobPty)

-- | The popup kept on its client as the client is resized.
refitClientPopup :: ServerState -> Client -> IO ()
refitClientPopup st client = do
    mp <- readTVarIO client.popup
    forM_ mp $ \p -> do
        csize <- readTVarIO client.size
        (old, new) <- atomically $ do
            ps <- readTVar p.state
            let ps' = refitPopup (fromIntegral csize.cols) (fromIntegral csize.rows) ps
            writeTVar p.state ps'
            bumpDirty st
            pure (ps, ps')
        when (innerSize new /= innerSize old) (resizeJob p new)

-- | The popup's screen and its command's pty resized to the box's inside.
resizeJob :: Popup -> PopupState -> IO ()
resizeJob p ps = forM_ (innerSize ps) $ \(w, h) -> do
    let sz = Size { rows = fromIntegral h, cols = fromIntegral w }
    Emu.resize p.screen sz
    Pty.resize p.jobPty sz
