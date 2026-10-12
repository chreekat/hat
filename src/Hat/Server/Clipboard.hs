-- | The clipboard, as tmux's @set-clipboard@ and @get-clipboard@ shape it:
-- copies made in hat sent on to the outer terminals (OSC 52), and an app's
-- own OSC 52 sets and queries answered from the paste buffers.
module Hat.Server.Clipboard
    ( setSelection
    , appClipboard
    , ClipboardAction (..)
    , clipboardAction
    , selectionBytes
    , clipboardReply
    , pushBufferBytes
    , paneViewers
    , base64Encode
    , base64Decode
    ) where

import Control.Concurrent.STM
import Control.Monad (filterM, forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Map.Strict qualified as Map

import Data.Time.Clock.POSIX (getPOSIXTime)

import Hat.Base64 (base64Decode, base64Encode)
import Hat.Model
import Hat.Model.Options (Options, optionNumber, optionText)
import Hat.Model.PasteBuffers (PasteBuffer (..), pasteAdd, pasteTop)

import Hat.Server.ClientIO (sendAs)
import Hat.Server.Hooks (notifyPane, notifyPasteChanges)
import Hat.Server.InputRequests (Asked (..))
import Hat.Server.InputRequestsIO (askTerminal)
import Hat.Server.Outbound (Delivery (..), Slot (..))
import Hat.Term.HostProtocol (OscTerm (..))
import Hat.Term.Pty qualified
import Hat.Transport.Wire (ServerToClient (..))

-- | A copy made in @pane@ (tmux's @screen_write_setselection@): unless
-- @set-clipboard@ is off, it goes to every client showing the pane's window
-- as an OSC 52, and @pane-set-clipboard@ fires. See Note [Falling behind]
-- for the @delivery@ each copy takes.
setSelection :: Delivery -> ServerState -> Pane -> ByteString -> IO ()
setSelection delivery st pane body = do
    setting <- optionText "set-clipboard" <$> readTVarIO st.options
    when (setting /= "off") $ do
        viewers <- atomically (paneViewers st pane)
        forM_ viewers $ \c -> sendAs delivery c (Notify (selectionBytes body))
        notifyPane st "pane-set-clipboard" pane []

-- | The clients whose current window holds @pane@.
paneViewers :: ServerState -> Pane -> STM [Client]
paneViewers st pane = do
    mhome <- readTVar pane.home  -- See Note [Pane home]
    case mhome of
        Nothing -> pure []
        Just (_, win) -> do
            cs <- Map.elems <$> readTVar st.clients
            sessions <- readTVar st.sessions
            flip filterM cs $ \c -> do
                sid <- readTVar c.session
                case Map.lookup sid sessions of
                    Nothing -> pure False
                    Just sess -> maybe False (\w -> w.id == win.id)
                        <$> currentWindow sess

-- | tmux's default @Ms@: set the outer terminal's clipboard.
selectionBytes :: ByteString -> ByteString
selectionBytes body = "\ESC]52;;" <> base64Encode body <> "\a"

-- | What an app's OSC 52 asks for, as @set-clipboard@ and @get-clipboard@
-- allow (tmux's @input_osc_52@).
data ClipboardAction
    = ClipboardIgnored
    | ClipboardSet ByteString     -- ^ a new buffer, and the outer clipboard
    | ClipboardAnswer Char        -- ^ reply with the newest buffer, for this
                                  --   selection letter (NUL for none)
    | ClipboardAsk                -- ^ ask a client's terminal for its clipboard
    deriving (Eq, Show)

-- | Decide an OSC 52's parameter text (after @52;@): only @set-clipboard on@
-- lets an app touch the clipboard; a @?@ query is answered from the newest
-- buffer under @get-clipboard buffer@, and asked of a terminal under
-- @request@ or @both@.
clipboardAction :: Options -> ByteString -> ClipboardAction
clipboardAction o param
    | optionText "set-clipboard" o /= "on" = ClipboardIgnored
    | otherwise = case B8.break (== ';') param of
        (_, rest) | B.null rest -> ClipboardIgnored
        (sel, rest) ->
            let payload = B.drop 1 rest
                clip = case filter (`elem` allowed) (B8.unpack sel) of
                    (c : _) -> c
                    [] -> '\NUL'
            in if B.null payload then ClipboardIgnored
                else if payload == "?"
                    then case optionText "get-clipboard" o of
                        "buffer" -> ClipboardAnswer clip
                        "off" -> ClipboardIgnored
                        _ -> ClipboardAsk
                    else maybe ClipboardIgnored ClipboardSet (base64Decode payload)
  where
    allowed = "cpqs01234567" :: String

-- | tmux's @input_reply_clipboard@: the OSC 52 answer carrying @body@,
-- terminated as the query was.
clipboardReply :: Char -> OscTerm -> ByteString -> ByteString
clipboardReply clip term body =
    "\ESC]52;" <> (if clip == '\NUL' then "" else B8.singleton clip) <> ";"
        <> base64Encode body
        <> (case term of
            TermBel -> "\a"
            TermSt -> "\ESC\\")

-- | Act on an app's OSC 52 in @pane@ under its options.
appClipboard :: ServerState -> Options -> Pane -> ByteString -> OscTerm -> IO ()
appClipboard st o pane param term = case clipboardAction o param of
    ClipboardIgnored -> pure ()
    ClipboardSet body -> do
        setSelection (Superseding ClipboardSlot) st pane body
        pushBufferBytes st body
    ClipboardAsk -> askTerminal st pane AskClipboard term
    ClipboardAnswer clip -> do
        top <- pasteTop <$> readTVarIO st.buffers
        forM_ top $ \pb -> do
            off <- readTVarIO pane.inputOff
            when (not off) $
                Hat.Term.Pty.writePty pane.pty (clipboardReply clip term pb.body)

-- | Push a fresh, auto-named buffer onto the buffer store.
pushBufferBytes :: ServerState -> ByteString -> IO ()
pushBufferBytes st body = do
    now <- floor <$> getPOSIXTime
    (old, new) <- atomically $ do
        limit <- optionNumber "buffer-limit" <$> readTVar st.options
        old <- readTVar st.buffers
        let new = pasteAdd limit "buffer" now body old
        writeTVar st.buffers new
        bumpDirty st
        pure (old, new)
    notifyPasteChanges st old new
