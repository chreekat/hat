-- | The interactive client commands: detaching, sending keys to a pane,
-- entering copy-mode, and the command prompt.
module Hat.Server.Command.Interact
    ( cmdDetachClient
    , cmdSendPrefix
    , cmdCopyMode
    , cmdCommandPrompt
    , cmdDisplayPanes
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.ClientIO (send)
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs, unknownFlag)
import Hat.Server.CopyMode qualified as CopyMode
import Hat.Server.DisplayPanes (armPanes)
import Hat.Server.FormatEnv (windowFormatEnv)
import Hat.Server.Format (FormatEnv)
import Hat.Server.Keys
import Hat.Server.Locate (clientActivePane, clientView, findClientNamed, findTarget)
import Hat.Server.Pane (typeIntoPane)
import Hat.Server.PaneMode (resetPaneModeAll, setPaneMode)
import Hat.Server.Target qualified as Target
import Hat.Server.Prompt qualified as Prompt
import Hat.Server.FormatEnv (expandFormat)
import Hat.Term.Emulator qualified as Emu
import Hat.Transport.Wire

-- | @detach-client@: detach the issuing client, or with @-s@ every client
-- attached to a session, or with @-a@ every attached client but the
-- issuing one.
cmdDetachClient :: CommandImpl
cmdDetachClient st mclient args
    | Just e <- unknownFlag "detach-client" "a" flags = pure [RErr e]
    | Just _ <- lookup "-E" opts =
        pure [RErr "detach-client -E: not supported"]
    | Just t <- lookup "-s" opts = do
        res <- findTarget st mclient Target.FindSession (Just t)
        case res of
            -- tmux's source session can fail to resolve: nothing to detach.
            Left _ -> pure []
            Right (sess, _, _, _) -> do
                cs <- atomically (sessionClients st sess.id)
                [] <$ mapM_ (`send` DetachOk) cs
    | otherwise = do
        etarget <- case lookup "-t" opts of
            Just name -> fmap Just <$> findClientNamed st name
            Nothing -> pure (Right mclient)
        case etarget of
            Left e -> pure [RErr e]
            Right mtarget
                | "-a" `elem` flags -> do
                    cs <- Map.elems <$> readTVarIO st.clients
                    others <- atomically $ filterM' cs $ \c -> do
                        sid <- readTVar c.session
                        live <- Map.member sid <$> readTVar st.sessions
                        pure (live && Just c.id /= fmap (.id) mtarget)
                    [] <$ mapM_ (`send` DetachOk) others
                | otherwise -> [] <$ forM_ mtarget (`send` DetachOk)
  where
    (opts, flags, _) = parseArgs "Est" args
    filterM' xs p = map fst . filter snd . zip xs <$> mapM p xs

cmdSendPrefix :: CommandImpl
cmdSendPrefix st mclient _ = do
    forM_ mclient $ \client -> do
        opts <- readTVarIO st.options
        forM_ (parseKeyName opts.prefix) $ \key -> do
            mpane <- clientActivePane st client
            forM_ mpane $ \pane -> typeIntoPane pane key.raw
    pure []

-- | @display-panes [-bN] [-d delay-ms] [template]@: overlay each pane's
-- number on the issuing client; a digit press runs @template@ (default
-- @select-pane -t '%%'@) against that pane. @-d@ overrides
-- @display-panes-time@; @-N@ keeps the overlay up on non-selecting keys.
-- @-b@ is tmux's \"don't block the command queue\" — hat never blocks, so
-- it is accepted as already satisfied.
cmdDisplayPanes :: CommandImpl
cmdDisplayPanes st mclient args = do
    let (opts, flags, pos) = parseArgs "d" args
        hold = if "-N" `elem` flags then PanesHold else PanesClose
        template = case pos of
            (t : _) -> t
            [] -> "select-pane -t '%%'"
        mdelay = case lookup "-d" opts of
            Nothing -> Right Nothing
            Just v -> case TR.decimal v of
                Right (n, rest) | T.null rest -> Right (Just n)
                _ -> Left ("display-panes: bad delay: " <> v)
    case (mclient, mdelay) of
        (_, Left e) -> pure [RErr e]
        (Nothing, _) -> pure [RErr "display-panes: no client"]
        (Just client, Right delay) ->
            [] <$ armPanes st client delay hold template

cmdCopyMode :: CommandImpl
cmdCopyMode st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
    res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
    case res of
        Left _ -> pure []
        Right (_, _, _, pane)
            | "-q" `elem` flags -> [] <$ resetPaneModeAll st pane
            | otherwise -> do
                let exit = if "-k" `elem` flags then KillPane else KeepPane
                [] <$ setPaneMode st pane "copy-mode" (startCopyMode pane) exit
  where
    startCopyMode pane = do
        scr <- Emu.snapshot pane.emulator
        frozen <- CopyMode.freezeGrid pane.emulator
        srvOpts <- readTVarIO st.options
        let table = case srvOpts.modeKeys of
                KeysVi -> "copy-mode-vi"
                KeysEmacs -> "copy-mode"
        pure $ CopyMode CopyModeData
            { frozen = frozen
            , copyState = CopyModeState
                { cursorRow = frozen.fgHsize + scr.cursor.row
                , cursorCol = scr.cursor.col
                , selection = Nothing
                , keyTable = table
                , viewportOffY = 0
                , numPrefix = Nothing
                , pendingSearch = Nothing
                , lastSearch = Nothing
                , lastQuery = Nothing
                }
            }

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
