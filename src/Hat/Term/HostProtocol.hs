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
      -- * screen/tmux window title
    , StitleState (..)
    , scrubStitle
      -- * Color queries answered by the host
    , QuerySignal (..)
    , CsSignal (..)
    , nextQuery
    , honoredSignals
    , partitionPassthrough
    ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

-- | Which color an OSC query asks about: OSC 10 (foreground) or 11
-- (background).
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

-- | Scrubber state for the screen/tmux window-title escape, carried across
-- 'feed' chunks: outside a title (holding back a chunk-final bare @ESC@ that
-- might begin @ESC k@) or inside one (accumulating the name until its
-- terminator, holding back a trailing @ESC@ that might begin the ST).
data StitleState
    = StOutside ByteString
        -- ^ carry: a trailing bare @ESC@ at chunk end
    | StInside ByteString ByteString
        -- ^ carry: a trailing @ESC@ that may start the ST, and the name so far

-- | Strip the screen/tmux window-title escape @ESC k <name> ST@ (also
-- accepting a BEL terminator) before the emulator parses it. hat advertises
-- @TERM=tmux-256color@, so tmux-aware apps set the title with this escape
-- instead of an OSC; the emulator does not know it and would spill the name onto
-- the screen as text. Returns the scrubbed bytes and every completed name, in
-- order. A title can span pty reads, so the state carries across 'feed' chunks.
scrubStitle :: StitleState -> ByteString -> (StitleState, ByteString, [ByteString])
scrubStitle st0 chunk = case st0 of
    StOutside carry     -> outside [] [] (carry <> chunk)
    StInside carry name -> inside [] [] [name] (carry <> chunk)
  where
    intro = "\ESCk"
    finish = B.concat . reverse
    outside oacc titles bs = case B.breakSubstring intro bs of
        (before, r)
            | B.null r ->
                let held = if "\ESC" `B.isSuffixOf` before then "\ESC" else ""
                    emit = B.take (B.length before - B.length held) before
                in (StOutside held, finish (emit : oacc), reverse titles)
            | otherwise -> inside (before : oacc) titles [] (B.drop (B.length intro) r)
    -- Inside the wrapper nothing reaches the emulator; the name accumulates until
    -- a BEL or an ST (a lone @ESC@ then backslash) closes it. An @ESC@ that is
    -- not the ST ends the name and is left for the emulator from the ESC onward.
    inside oacc titles nacc bs = case B.elemIndex 0x07 bs of
        Just j | maybe True (j <) (B.elemIndex 0x1b bs) ->
            outside oacc (finish (B.take j bs : nacc) : titles) (B.drop (j + 1) bs)
        _ -> case B.elemIndex 0x1b bs of
            Nothing -> (StInside "" (finish (bs : nacc)), finish oacc, reverse titles)
            Just i  ->
                let nacc' = B.take i bs : nacc
                in case B.uncons (B.drop (i + 1) bs) of
                    Nothing           -> (StInside "\ESC" (finish nacc'), finish oacc, reverse titles)
                    Just (0x5c, rest) -> outside oacc (finish nacc' : titles) rest
                    Just (_, _)       -> outside oacc (finish nacc' : titles) (B.drop i bs)

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
            in case tryCsi r <|> tryOsc r <|> tryNotify r of
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
