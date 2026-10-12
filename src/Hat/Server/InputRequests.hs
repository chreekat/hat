{-# LANGUAGE StrictData #-}

-- | Queries a pane's program makes of the terminal that hat cannot answer
-- itself, forwarded to an attached client's terminal (tmux's input
-- requests, input.c): an OSC 4 query of a palette entry the pane leaves
-- unset, and an OSC 52 clipboard query under @get-clipboard request@ or
-- @both@. The terminal's reply comes back in that client's input and goes to
-- the pane that asked; the pane's other replies made meanwhile wait behind
-- the request, so the program reads every reply in the order it asked. An
-- unanswered request lapses after 'requestTimeout'.
module Hat.Server.InputRequests
    ( Asked (..)
    , Answer (..)
    , Pending (..)
    , Ask (..)
    , Held (..)
    , Release (..)
    , InputRequests (..)
    , noRequests
    , requestTimeout
    , ask
    , reply
    , answered
    , expire
    , forgetClient
    , forgetPane
    , clientWaiting
    , waiting
    , ReplyScan (..)
    , scanReply
    , parseAnswer
    ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Char (digitToInt, isDigit, isSpace)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)

import Hat.Base64 (base64Decode)
import Hat.Colour (colourParseX11)
import Hat.Model.Ids (ClientId, PaneId)
import Hat.Term.HostProtocol (OscTerm (..))

-- | What a pane asked a terminal.
data Asked
    = AskPalette Int  -- ^ an OSC 4 palette entry
    | AskClipboard    -- ^ the OSC 52 clipboard
    deriving (Eq, Show)

-- | A terminal's reply to a forwarded query.
data Answer
    = PaletteAnswer Int (Word8, Word8, Word8)
    | ClipboardAnswer Char ByteString  -- ^ the selection it named (NUL for
                                       --   none), and the decoded contents
    deriving (Eq, Show)

-- | One entry in a pane's queue, oldest first.
data Pending
    = Asking Ask
    | Holding Held
    deriving (Eq, Show)

-- | A query forwarded to a client's terminal.
data Ask = Ask
    { serial :: Int       -- ^ creation order, across every pane
    , client :: ClientId  -- ^ whose terminal was asked
    , asked  :: Asked
    , term   :: OscTerm   -- ^ how the pane terminated its query
    , at     :: Int       -- ^ when, in monotonic milliseconds
    }
    deriving (Eq, Show)

-- | A reply the pane is owed after the asks ahead of it.
data Held = Held
    { bytes :: ByteString
    , at    :: Int
    }
    deriving (Eq, Show)

-- | What to write to a pane, in order, as its queue settles.
data Release
    = Write ByteString
    | Reply OscTerm Answer  -- ^ the answer to the pane's ask, to be put as
                            --   its query was terminated
    deriving (Eq, Show)

-- | Every pane's queue (no pane holds an empty one), and the next serial.
data InputRequests = InputRequests
    { panes  :: Map PaneId [Pending]
    , next   :: Int
    }
    deriving (Eq, Show)

noRequests :: InputRequests
noRequests = InputRequests { panes = Map.empty, next = 0 }

-- | How long a request waits for its reply (tmux's @INPUT_REQUEST_TIMEOUT@),
-- in milliseconds.
requestTimeout :: Int
requestTimeout = 500

-- | Record that @pane@ asked @client@'s terminal at @now@ (tmux's
-- @input_add_request@).
ask :: Int -> PaneId -> ClientId -> Asked -> OscTerm -> InputRequests -> InputRequests
ask now pane cid a t rs = rs
    { panes = Map.insertWith (flip (<>)) pane
        [Asking Ask { serial = rs.next, client = cid, asked = a, term = t, at = now }]
        rs.panes
    , next = rs.next + 1 }

-- | A reply @pane@ makes at @now@ (tmux's @input_reply@): written at once,
-- unless the pane's queue is waiting, when it is held at the back.
reply :: Int -> PaneId -> ByteString -> InputRequests -> (InputRequests, Maybe ByteString)
reply now pane bs rs
    | Map.member pane rs.panes =
        ( rs { panes = Map.adjust (<> [Holding Held { bytes = bs, at = now }]) pane rs.panes }
        , Nothing )
    | otherwise = (rs, Just bs)

-- | @client@'s terminal answered (tmux's @input_request_reply@). The
-- client's oldest request of the answer's kind takes it; its older requests
-- of other kinds (or other palette entries) are dropped unanswered. In the
-- pane that asked, the replies held ahead of the request are released, then
-- the answer, then the replies held behind it up to the next ask; any other
-- ask ahead of it is dropped.
answered :: ClientId -> Answer -> InputRequests -> (InputRequests, Maybe (PaneId, [Release]))
answered cid ans rs = case search mine of
    (dropped, Nothing) -> (without dropped rs, Nothing)
    (dropped, Just (pane, found)) ->
        let rs1 = without dropped rs
            queue = Map.findWithDefault [] pane rs1.panes
            (released, kept) = settle found queue
        in (rs1 { panes = setQueue pane kept rs1.panes }, Just (pane, released))
  where
    mine = sortOn (\(_, a) -> a.serial)
        [ (pane, a) | (pane, ps) <- Map.toList rs.panes, Asking a <- ps, a.client == cid ]
    search = go []
      where
        go acc [] = (acc, Nothing)
        go acc ((pane, a) : rest)
            | takes a.asked = (acc, Just (pane, a.serial))
            | otherwise = go (a.serial : acc) rest
    takes = \case
        AskPalette n | PaletteAnswer m _ <- ans -> n == m
        AskClipboard | ClipboardAnswer _ _ <- ans -> True
        _ -> False
    settle found = go False
      where
        go _ [] = ([], [])
        go complete (p : ps) = case p of
            Holding h -> first (Write h.bytes :) (go complete ps)
            Asking a
                | complete -> ([], p : ps)
                | a.serial == found -> first (Reply a.term ans :) (go True ps)
                | otherwise -> go complete ps
    first f (x, y) = (f x, y)

-- | @pane@'s requests lapsed by @now@ (tmux's
-- @input_request_timer_callback@): its held replies among them are written,
-- in order, and its lapsed asks dropped unanswered.
expire :: Int -> PaneId -> InputRequests -> (InputRequests, [ByteString])
expire now pane rs = case Map.lookup pane rs.panes of
    Nothing -> (rs, [])
    Just ps ->
        ( rs { panes = setQueue pane (filter live ps) rs.panes }
        , [ h.bytes | p@(Holding h) <- ps, not (live p) ] )
  where
    live = \case
        Asking a -> a.at >= now - requestTimeout
        Holding h -> h.at >= now - requestTimeout

-- | Drop a departing client's requests (tmux's @input_cancel_requests@).
forgetClient :: ClientId -> InputRequests -> InputRequests
forgetClient cid = dropAsks (\a -> a.client == cid)

-- | Drop a closed pane's queue.
forgetPane :: PaneId -> InputRequests -> InputRequests
forgetPane pane rs = rs { panes = Map.delete pane rs.panes }

-- | Whether a reply from @client@'s terminal is awaited.
clientWaiting :: ClientId -> InputRequests -> Bool
clientWaiting cid rs =
    or [ a.client == cid | ps <- Map.elems rs.panes, Asking a <- ps ]

-- | Whether any pane's queue is waiting.
waiting :: InputRequests -> Bool
waiting rs = not (Map.null rs.panes)

without :: [Int] -> InputRequests -> InputRequests
without [] rs = rs
without serials rs = dropAsks (\a -> a.serial `elem` serials) rs

-- | Drop the asks @gone@ picks, leaving the held replies where they are.
dropAsks :: (Ask -> Bool) -> InputRequests -> InputRequests
dropAsks gone rs = rs { panes = Map.mapMaybe (nonEmpty . filter keep) rs.panes }
  where
    keep = \case
        Asking a -> not (gone a)
        Holding _ -> True

setQueue :: PaneId -> [Pending] -> Map PaneId [Pending] -> Map PaneId [Pending]
setQueue pane = \case
    [] -> Map.delete pane
    ps -> Map.insert pane ps

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs

-- | Whether the input after an ESC begins a terminal's reply to a forwarded
-- query (tmux's @tty_keys_clipboard@ and @tty_keys_palette@).
data ReplyScan
    = ReplyComplete Int  -- ^ it does, this many bytes long
    | ReplyCut           -- ^ it may, once more input arrives
    | NotAReply
    deriving (Eq, Show)

-- | Scan the input after an ESC for a reply: an OSC 52 or OSC 4 sequence
-- up to its BEL or ST.
scanReply :: ByteString -> ReplyScan
scanReply r = case (opening "]52;", opening "]4;") of
    (Just ReplyCut, _) -> ReplyCut
    (_, Just ReplyCut) -> ReplyCut
    (Just _, _) -> clipboardEnd 4
    (_, Just _) -> paletteEnd
    _ -> NotAReply
  where
    -- Whether @r@ opens with @pre@ (complete), or ends partway through it.
    opening pre
        | pre `B.isPrefixOf` r = Just (ReplyComplete 0)
        | r `B.isPrefixOf` pre = Just ReplyCut
        | otherwise = Nothing
    clipboardEnd j
        | j >= B.length r = ReplyCut
        | B.index r j == 0x07 = ReplyComplete (j + 1)
        | j > 4 && B.index r (j - 1) == 0x1b && B.index r j == 0x5c = ReplyComplete (j + 1)
        | otherwise = clipboardEnd (j + 1)
    -- Up to 127 bytes of parameters; an index that is no entry is no reply.
    paletteEnd = go 0
      where
        go i
            | i == 127 = NotAReply
            | 3 + i == B.length r = ReplyCut
            | i > 0 && B.index r (2 + i) == 0x1b && B.index r (3 + i) == 0x5c = done i
            | B.index r (3 + i) == 0x07 = done i
            | otherwise = go (i + 1)
        done i
            | i == 0 = ReplyComplete 4
            | Just _ <- paletteIndex (paletteBody i) = ReplyComplete (4 + i)
            | otherwise = NotAReply
        paletteBody i =
            let tmp = B.take i (B.drop 3 r)
            in fromMaybe tmp (B.stripSuffix "\ESC" tmp)

-- | A palette reply's entry, and the colour text after its @;@: C's
-- @strtol@, as tmux reads it.
paletteIndex :: ByteString -> Maybe (Int, ByteString)
paletteIndex body = case B8.uncons endp of
    Just (';', colour) | n >= 0 && n <= 255 -> Just (n, colour)
    _ -> Nothing
  where
    unspaced = B8.dropWhile isSpace body
    (negative, unsigned) = case B8.uncons unspaced of
        Just ('-', more) -> (True, more)
        Just ('+', more) -> (False, more)
        _ -> (False, unspaced)
    (digits, afterDigits) = B8.span isDigit unsigned
    endp = if B.null digits then body else afterDigits
    -- Saturated: anything past three digits is out of range already.
    magnitude = B8.foldl' (\acc d -> min 1000 (acc * 10 + digitToInt d)) 0 digits
    n = if negative then negate magnitude else magnitude

-- | The answer a complete reply carries ('scanReply'), given the bytes
-- after its ESC; 'Nothing' for one that carries none.
parseAnswer :: ByteString -> Maybe Answer
parseAnswer r
    | Just body <- B.stripPrefix "]52;" r = clipboard (unterminated body)
    | Just body <- B.stripPrefix "]4;" r = palette (unterminated body)
    | otherwise = Nothing
  where
    unterminated b
        | Just s <- B.stripSuffix "\a" b = s
        | Just s <- B.stripSuffix "\ESC\\" b = s
        | otherwise = b
    clipboard body = do
        let clip = case B8.unpack (B.take 2 body) of
                [c, ';'] | c /= ';' -> c
                _ -> '\NUL'
            afterSel = B.drop 1 (B8.dropWhile (/= ';') body)
        if B.null afterSel then Nothing else
            ClipboardAnswer clip <$> base64Decode afterSel
    palette body = do
        (n, colour) <- paletteIndex body
        c <- colourParseX11 (TE.decodeUtf8Lenient colour)
        pure (PaletteAnswer n (channel 16 c, channel 8 c, channel 0 c))
    channel s c = fromIntegral ((c `div` (2 ^ (s :: Int))) `mod` 256)
