{-# LANGUAGE OverloadedStrings #-}

-- | The host-aware terminal protocol layer that sits above the grid engine
-- (libghostty-vt).
--
-- The grid engine is a pure screen emulator: it owns what cells are where, and
-- nothing else. It has no operating system, no notion of an outer terminal,
-- no multiplexer policy. So there is a class of escape sequences an inner app
-- sends that it structurally /cannot/ answer correctly — the app asking
-- its terminal about the world around it: the OS light\/dark scheme, the real
-- background color, whether unknown sequences should be tunnelled to the
-- outer terminal.
--
-- Those belong to hat, and this module owns them. It is the deliberate seam
-- between the two:
--
--   * __the grid engine__ owns the /screen/ — SGR, wide chars, scrollback, regions.
--   * __this layer__ owns the /host protocol/ — the app's questions about its
--     environment that only the host (hat) can answer.
--
-- The charter test for whether a sequence belongs here: /does answering it
-- require knowledge the grid engine cannot have?/ The OS color scheme (OSC
-- 10\/11, DEC mode 2031), the multiplexer's passthrough policy (DCS @tmux;@),
-- routing a desktop notification (OSC 9\/777) out to the real terminal — yes.
--
-- Everything here is pure: bytes in, signals and rewritten bytes out. The
-- 'Hat.Term.Emulator' 'Hat.Term.Emulator.feed' loop is the thin IO wiring
-- that runs these over the grid engine; 'Hat.Server' is the host that answers
-- the signals with knowledge it holds (see 'Hat.Server.applyScheme').
module Hat.Term.HostProtocol
    ( -- * OSC color-query vocabulary
      OscColorTarget (..)
    , OscTerm (..)
      -- * DCS tmux passthrough
    , PassState (..)
    , scrubPassthrough
      -- * Sequences hat handles above the grid engine
    , HostCarry (..)
    , HostPiece (..)
    , HostSignal (..)
    , HostPolicy (..)
    , scrubHost
    , cleanName
      -- * Color queries answered by the host
    , QuerySignal (..)
    , CsSignal (..)
    , nextQuery
    , normaliseColourSets
    , honoredSignals
    , partitionPassthrough
    ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Char8 qualified as BC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)

import Hat.Colour (colourParseX11)

-- | Which color an OSC query asks about: OSC 10 (foreground) or 11
-- (background).
-- | Rewrite each OSC 10\/11\/12 colour a program sets to the colour tmux
-- reads from its spec ('colourParseX11'), spelled @rgb:rr\/gg\/bb@; a spec
-- tmux cannot read is dropped, as tmux ignores it. Queries pass untouched.
normaliseColourSets :: ByteString -> ByteString
normaliseColourSets bs = case B.breakSubstring "\ESC]1" bs of
    (before, rest)
        | B.null rest -> bs
        | Just (code, afterCode) <- B.uncons (B.drop 3 rest)
        , code `elem` [0x30, 0x31, 0x32]
        , Just afterSemi <- B.stripPrefix ";" afterCode
        , Just (spec, term, more) <- terminated afterSemi
        , spec /= "?" ->
            let colour = colourParseX11 (TE.decodeUtf8Lenient spec)
                rewritten = case colour of
                    Nothing -> ""
                    Just c -> "\ESC]1" <> B.singleton code <> ";rgb:"
                        <> hex2 (c `div` 65536) <> "/" <> hex2 (c `div` 256 `mod` 256)
                        <> "/" <> hex2 (c `mod` 256) <> term
            in before <> rewritten <> normaliseColourSets more
        | otherwise -> before <> B.take 3 rest <> normaliseColourSets (B.drop 3 rest)
  where
    terminated r = case B.elemIndex 0x07 r of
        Just i | noEsc (B.take i r) -> Just (B.take i r, "\a", B.drop (i + 1) r)
        _ -> case B.breakSubstring "\ESC\\" r of
            (spec, st) | not (B.null st), noEsc spec -> Just (spec, "\ESC\\", B.drop 2 st)
            _ -> Nothing
    noEsc = B.notElem 0x1b
    hex2 n = BC.pack [digit (n `div` 16), digit (n `mod` 16)]
    digit d = "0123456789abcdef" !! d

data OscColorTarget = Foreground | Background
    deriving (Eq, Show)

-- | The string terminator an OSC query ended with; replies must echo it
-- (xterm answers BEL-terminated queries with BEL, ST with ST).
data OscTerm = TermBel | TermSt
    deriving (Eq, Show)

-- | Scrubber state for DCS tmux passthrough, carried across 'feed' chunks:
-- outside a wrapper (holding back a partial @ESC Ptmux;@ prefix that ends
-- the chunk), or inside one (discarding until its ST while accumulating the
-- un-doubled payload, so a wrapper that spans reads still yields its payload
-- whole).
data PassState
    = Outside ByteString
        -- ^ carry: proper prefix of @ESC Ptmux;@ at chunk end
    | Inside ByteString ByteString
        -- ^ carry: a trailing @ESC@ that may start the ST, and the un-doubled
        --   payload accumulated so far

-- | Strip DCS tmux passthrough (@ESC Ptmux; … ESC \\@) from a pane's output
-- before the emulator parses it, returning the scrubbed bytes and each completed
-- wrapper's rebuilt payload (ESC-undoubled) for the caller to honour.
--
-- A tmux-aware app (claude) sees $TMUX set and wraps sequences meant for the
-- outer terminal in this DCS; a VT parser mis-handles the wrapper's
-- doubled inner ESCs and spills the payload onto the screen as text (the
-- \"11;?9;4;0;\" garbage). So the wrapper never reaches the emulator. tmux's
-- default (@allow-passthrough off@) then discards the payload entirely; hat
-- goes one step further and answers the host queries it recognises out of the
-- payload (see 'honoredSignals'), discarding the rest. Framing only lives
-- here; the caller decides what to do with each payload. A wrapper can span
-- pty reads, so the state — including the partial payload — carries across
-- 'feed' chunks.
scrubPassthrough :: PassState -> ByteString -> (PassState, ByteString, [ByteString])
scrubPassthrough st0 chunk = case st0 of
    Outside carry        -> outside [] [] (carry <> chunk)
    Inside carry payload -> inside [] [] [payload] (carry <> chunk)
  where
    intro = "\ESCPtmux;"
    finish = B.concat . reverse
    outside oacc payloads bs = case B.breakSubstring intro bs of
        (before, r)
            | B.null r ->
                -- No wrapper here; hold back a chunk-final partial intro
                -- (e.g. a trailing bare ESC) until the next read decides.
                let held = introSuffix before
                    emit = B.take (B.length before - B.length held) before
                in (Outside held, finish (emit : oacc), reverse payloads)
            | otherwise -> inside (before : oacc) payloads [] (B.drop (B.length intro) r)
    -- Inside the wrapper nothing reaches the emulator; the payload is rebuilt
    -- (un-doubling the ESCs the wrapper doubled) until the ST — a lone ESC
    -- followed by backslash — closes it and emits the completed payload.
    inside oacc payloads pacc bs = case B.elemIndex 0x1b bs of
        Nothing -> (Inside "" (finish (bs : pacc)), finish oacc, reverse payloads)
        Just i ->
            let pacc' = B.take i bs : pacc
            in case B.uncons (B.drop (i + 1) bs) of
                Nothing           -> (Inside "\ESC" (finish pacc'), finish oacc, reverse payloads)
                Just (0x1b, rest) -> inside oacc payloads ("\ESC" : pacc') rest
                Just (0x5c, rest) -> outside oacc (finish pacc' : payloads) rest
                Just (c, rest)    -> inside oacc payloads (B.singleton c : pacc') rest
    -- The longest proper prefix of the intro that this chunk ends with.
    introSuffix bs =
        let cap = min (B.length intro - 1) (B.length bs)
            ks = [ k | k <- [cap, cap - 1 .. 1]
                 , B.take k intro `B.isSuffixOf` bs ]
        in case ks of
            (k : _) -> B.drop (B.length bs - k) bs
            []      -> ""

-- | Scanner state for 'scrubHost', carried across 'feed' chunks.
data HostCarry
    = HostCarry ByteString
        -- ^ an unfinished sequence the previous chunk ended in
    | HostDiscarding
        -- ^ inside a string that outgrew the input buffer, dropping it up to
        --   its terminator

-- | What 'scrubHost' makes of a pane's output: bytes for the grid engine, or
-- a sequence hat acts on itself, in stream order.
data HostPiece
    = HostBytes ByteString
    | HostSignal HostSignal
    deriving (Eq, Show)

-- | A sequence hat handles above the grid engine.
data HostSignal
    = HostTitle ByteString
        -- ^ OSC 0\/2 or APC: the app sets the pane title
    | HostRename ByteString
        -- ^ @ESC k … ST@: the app renames the window
    | HostClipboard ByteString OscTerm
        -- ^ OSC 52: the app sets or queries the clipboard; the parameter
        --   text after @52;@ and the terminator it used
    | HostKeyMode Int
        -- ^ @CSI > 4 ; n m@ (or @CSI > 4 n@, as 0): the extended-key mode the
        --   app asks for
    | HostErase Int ByteString
        -- ^ ED 0 or 2 (the parameter, and the sequence itself), which may push
        --   the screen into the history first
    deriving (Eq, Show)

-- | The pane options that shape how its output is taken.
data HostPolicy = HostPolicy
    { allowAltScreen :: Bool
        -- ^ @alternate-screen@: the app may switch to the alternate screen
    , scrollOnClear :: Bool
        -- ^ @scroll-on-clear@: clearing the screen keeps its lines in the
        --   history
    , bufferSize :: Int
        -- ^ @input-buffer-size@: the longest string payload kept
    }
    deriving (Eq, Show)

-- | Pick the sequences hat handles itself out of a pane's output (see
-- 'HostSignal'), dropping the alternate-screen switches @alternate-screen
-- off@ forbids, and pass everything else on as bytes. A sequence torn across
-- reads is held until it completes; a string payload longer than the
-- @input-buffer-size@ is dropped whole, as tmux discards it.
scrubHost :: HostPolicy -> HostCarry -> ByteString -> (HostCarry, [HostPiece])
scrubHost pol carry0 chunk = case carry0 of
    HostCarry held -> scan (held <> chunk)
    HostDiscarding -> case stringEnd chunk 0 of
        Just (_, end, _) -> scan (B.drop end chunk)
        Nothing -> (HostDiscarding, [])
  where
    scan bs = go 0 0 []
      where
        len = B.length bs
        at i = B.index bs i
        emit s e acc = if e > s then HostBytes (B.take (e - s) (B.drop s bs)) : acc else acc
        finish carry s e acc = (carry, reverse (emit s e acc))
        go s i acc = case B.elemIndex 0x1b (B.drop i bs) of
            Nothing -> finish (HostCarry "") s len acc
            Just d ->
                let p = i + d
                in case classify p of
                    Incomplete
                        | len - p > pol.bufferSize + 16 ->
                            finish HostDiscarding s p acc
                        | otherwise -> finish (HostCarry (B.drop p bs)) s p acc
                    Skip n -> go s (p + n) acc
                    Found end piece -> go end end (piece : emit s p acc)
                    Dropped end -> go end end (emit s p acc)
        classify p
            | p + 1 >= len = Incomplete
            | otherwise = case at (p + 1) of
                0x5d -> osc p          -- ']'
                0x5f -> str p HostTitle          -- '_' APC
                0x6b -> str p HostRename         -- 'k'
                0x5b -> csi p          -- '['
                _ -> Skip 1
        -- An OSC this layer takes: a title or the clipboard.
        osc p =
            let ds = B.takeWhile isDigitB (B.drop (p + 2) bs)
                q = p + 2 + B.length ds
            in if q >= len then Incomplete else
                case (ds, at q) of
                    ("0", c) -> titleAt c q
                    ("2", c) -> titleAt c q
                    ("52", 0x3b) -> case stringEnd bs (q + 1) of
                        Nothing -> Incomplete
                        Just (payload, end, term) -> keep end payload
                            (HostSignal (HostClipboard payload term))
                    _ -> Skip 2
          where
            -- tmux takes the title after the @;@, or an empty one when the
            -- number is the whole string.
            titleAt c q
                | c == 0x3b = str' (q + 1) HostTitle
                | c == 0x07 || c == 0x1b = str' q HostTitle
                | otherwise = Skip 2
        str p mk = str' (p + 2) mk
        str' q mk = case stringEnd bs q of
            Nothing -> Incomplete
            Just (payload, end, _) -> keep end payload (HostSignal (mk payload))
        keep end payload piece
            | B.length payload > pol.bufferSize = Dropped end
            | otherwise = Found end piece
        csi p = case csiAt bs (p + 2) of
            Nothing -> Incomplete
            Just (prefix, params, inter, final, end) ->
                let raw = B.take (end - p) (B.drop p bs)
                    nums = map readParam (B.split 0x3b params)
                in case (prefix, inter, final, nums) of
                    (Just 0x3e, "", 0x6d, 4 : rest) ->
                        Found end (HostSignal (HostKeyMode (headOr 0 rest)))
                    (Just 0x3e, "", 0x6e, [4]) -> Found end (HostSignal (HostKeyMode 0))
                    (Just 0x3f, "", f, _)
                        | not pol.allowAltScreen, f == 0x68 || f == 0x6c
                        , any (`elem` altModes) nums ->
                            case filter (`notElem` altModes) nums of
                                [] -> Dropped end
                                kept -> Found end (HostBytes (decset kept f))
                    (Nothing, "", 0x4a, ns) | ns `elem` [[], [0], [2]] ->
                        Found end (HostSignal (HostErase (headOr 0 ns) raw))
                    _ -> Skip (end - p)
        altModes = [47, 1047, 1049]
        decset ns f = "\ESC[?" <> B.intercalate ";" (map (B8.pack . show) ns) <> B.singleton f
        headOr d = \case
            (x : _) -> x
            [] -> d
        readParam f = case B8.readInt f of
            Just (n, "") -> n
            _ -> if B.null f then 0 else -1
    isDigitB b = b >= 0x30 && b <= 0x39

-- | tmux's @clean_name@ for a title or window name an app sent: refused
-- unless valid UTF-8; @#(@ defused to @_(@ so no format ever runs a command
-- from it; control characters and backslash made visible as @vis(3)@ does.
cleanName :: ByteString -> Maybe Text
cleanName raw = case TE.decodeUtf8' raw of
    Left _ -> Nothing
    Right t -> Just (T.concatMap visible (T.replace "#(" "_(" t))
  where
    visible c = case c of
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        '\a' -> "\\a"
        '\b' -> "\\b"
        '\v' -> "\\v"
        '\f' -> "\\f"
        _ | c < ' ' || c == '\DEL' -> T.pack ('\\' : octal (fromEnum c))
          | otherwise -> T.singleton c
    octal n = [ toEnum (fromEnum '0' + d) | d <- [n `div` 64, n `div` 8 `mod` 8, n `mod` 8] ]

-- | What 'scrubHost' makes of the escape sequence at one offset.
data Scanned
    = Incomplete           -- ^ it runs past the end of the bytes so far
    | Skip Int             -- ^ not this layer's: scan on this many bytes later
    | Found Int HostPiece  -- ^ taken, up to this offset, as this piece
    | Dropped Int          -- ^ taken out of the stream, up to this offset

-- | The payload of a string sequence starting at an offset, the offset just
-- past it, and its terminator: BEL, ST, or an ESC that starts something else
-- (left in the stream). 'Nothing' when the string has not ended yet.
stringEnd :: ByteString -> Int -> Maybe (ByteString, Int, OscTerm)
stringEnd bs q = go q
  where
    len = B.length bs
    go j = case B.findIndex (\b -> b == 0x07 || b == 0x1b) (B.drop j bs) of
        Nothing -> Nothing
        Just d ->
            let k = j + d
                payload = B.take (k - q) (B.drop q bs)
            in if B.index bs k == 0x07
                then Just (payload, k + 1, TermBel)
                else if k + 1 >= len
                    then Nothing
                    else if B.index bs (k + 1) == 0x5c
                        then Just (payload, k + 2, TermSt)
                        else Just (payload, k, TermSt)

-- | A CSI's parts from just past its @ESC [@: a private prefix byte, the
-- parameter bytes, the intermediates, the final byte, and the offset past
-- it. 'Nothing' when the sequence has not ended yet; a byte no CSI may hold
-- ends it at that byte, as no final.
csiAt :: ByteString -> Int -> Maybe (Maybe Word8, ByteString, ByteString, Word8, Int)
csiAt bs q0 = do
    let len = B.length bs
    (prefix, q1) <- if q0 >= len then Nothing else
        let b = B.index bs q0
        in Just (if b >= 0x3c && b <= 0x3f then (Just b, q0 + 1) else (Nothing, q0))
    let params = B.takeWhile (\b -> (b >= 0x30 && b <= 0x3b)) (B.drop q1 bs)
        q2 = q1 + B.length params
        inter = B.takeWhile (\b -> b >= 0x20 && b <= 0x2f) (B.drop q2 bs)
        q3 = q2 + B.length inter
    if q3 >= len then Nothing else
        let f = B.index bs q3
        in pure (prefix, params, inter, f, q3 + 1)

-- | Every host query hat recognises in a byte string, in order. Used to
-- honour the payload of a DCS tmux passthrough: a tmux-aware app wraps a
-- query meant for the outer terminal in passthrough, and hat — the terminal
-- that holds the answer (the OS scheme) — answers the same set it answers
-- inline, so a wrapped OSC 10\/11 or DEC 2031 query does not stall the app
-- until its slow poll fallback. Unrecognised payload bytes yield nothing, as
-- tmux's @allow-passthrough off@ discards them.
honoredSignals :: ByteString -> [QuerySignal]
honoredSignals = fst . partitionPassthrough

-- | Split a DCS tmux passthrough payload into the queries hat answers and the
-- bytes left over — the sequences it does not recognise (OSC 52 clipboard,
-- OSC 12 cursor color, OSC 4 palette, …). @allow-passthrough off@ would drop
-- the leftover silently; hat surfaces it instead so the reader can log it
-- (see 'Hat.Term.Emulator.feed' / 'Hat.Server''s @UnhandledPassthrough@) and
-- we stop discarding payloads without a trace.
partitionPassthrough :: ByteString -> ([QuerySignal], ByteString)
partitionPassthrough = go
  where
    go bs = case nextQuery bs of
        Just (before, sig, more) ->
            let (sigs, rest) = go more
            in (sig : sigs, before <> rest)
        Nothing -> ([], bs)

-- | A color control the inner app sent to its terminal, which hat answers
-- itself: neither the emulator nor the outer terminal knows the OS scheme hat
-- tracks (see 'Hat.Server.applyScheme').
data QuerySignal
    = SigColor CsSignal              -- ^ DEC mode 2031 subscribe/query
    | SigOsc OscColorTarget OscTerm  -- ^ OSC 10/11 color query
    | SigPalette Int OscTerm         -- ^ OSC 4 query of one palette entry
    | SigNotify ByteString           -- ^ OSC 9/777 desktop notification, raw
                                     --   (whole sequence) to forward verbatim

-- | DEC-mode-2031 color-scheme controls: @CSI ? 2031 h@/@l@ subscribe or
-- unsubscribe from light/dark change reports, @CSI ? 996 n@ queries the
-- current scheme.
data CsSignal = CsEnable | CsDisable | CsQuery
    deriving (Eq, Show)

-- | Find the first sequence in a chunk that hat handles itself rather than
-- the emulator: a DEC 2031 control (only the standalone form; one folded into a
-- multi-parameter DECSET is left for the emulator), an OSC 10/11 color query
-- (@OSC 1x ; ? BEL@ or @… ESC \\@; color *set* sequences like @OSC 11;rgb:…@
-- are not queries and pass through untouched), or an OSC 9/777 desktop
-- notification (captured whole to forward to the outer terminal). Returns
-- the bytes before it, the signal, and the bytes after it, with the
-- sequence itself stripped.
nextQuery :: ByteString -> Maybe (ByteString, QuerySignal, ByteString)
nextQuery bs = go 0
  where
    go i = case B.elemIndex 0x1b (B.drop i bs) of
        Nothing -> Nothing
        Just d ->
            let p = i + d
                r = B.drop p bs
            in case tryCsi r <|> tryOsc r <|> tryPalette r <|> tryNotify r of
                Just (sig, more) -> Just (B.take p bs, sig, more)
                Nothing -> go (p + 1)
    tryCsi r = do
        afterIntro <- B.stripPrefix "\ESC[?" r
        let (params, tailB) = B.span isParam afterIntro
        (final, more) <- B.uncons tailB
        sig <- classify params final
        pure (SigColor sig, more)
    tryOsc r = do
        (target, afterIntro) <-
            (Foreground,) <$> B.stripPrefix "\ESC]10;?" r
            <|> (Background,) <$> B.stripPrefix "\ESC]11;?" r
        (term, more) <-
            (TermBel,) <$> B.stripPrefix "\a" afterIntro
            <|> (TermSt,) <$> B.stripPrefix "\ESC\\" afterIntro
        pure (SigOsc target term, more)
    tryPalette r = do
        afterIntro <- B.stripPrefix "\ESC]4;" r
        let (digits, rest) = B.span (\b -> b >= 0x30 && b <= 0x39) afterIntro
        n <- case B.foldl' (\acc b -> acc * 10 + fromIntegral (b - 0x30)) (0 :: Int) digits of
            v | not (B.null digits), B.length digits <= 3, v < 256 -> Just v
            _ -> Nothing
        afterQuery <- B.stripPrefix ";?" rest
        (term, more) <-
            (TermBel,) <$> B.stripPrefix "\a" afterQuery
            <|> (TermSt,) <$> B.stripPrefix "\ESC\\" afterQuery
        pure (SigPalette n term, more)
    tryNotify r = do
        afterParams <-
            (do a <- B.stripPrefix "\ESC]9;" r
                -- @OSC 9 ; 4@ is ConEmu progress, not a desktop notification.
                if "4;" `B.isPrefixOf` a then Nothing else Just a)
            <|> B.stripPrefix "\ESC]777;notify;" r
        more <- afterTerminator afterParams
        pure (SigNotify (B.take (B.length r - B.length more) r), more)
    isParam b = (b >= 0x30 && b <= 0x39) || b == 0x3b     -- 0-9 or ';'
    classify params final = case (params, final) of
        ("2031", 0x68) -> Just CsEnable    -- 'h'
        ("2031", 0x6c) -> Just CsDisable   -- 'l'
        ("996",  0x6e) -> Just CsQuery     -- 'n'
        _              -> Nothing
    -- Bytes after an OSC string terminator (BEL or ST), or Nothing if the
    -- chunk ends before one — an unterminated notify is left for the emulator
    -- (which drops it) rather than forwarded half-formed.
    afterTerminator r = case B.uncons r of
        Nothing -> Nothing
        Just (0x07, more) -> Just more                      -- BEL
        Just (0x1b, more)
            | Just (0x5c, rest) <- B.uncons more -> Just rest  -- ST (ESC \)
        Just (_, more) -> afterTerminator more
