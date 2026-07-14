{-# LANGUAGE OverloadedStrings #-}

-- | The host-aware terminal protocol layer that sits above libvterm.
--
-- libvterm is a pure screen-grid emulator: it owns what cells are where, and
-- nothing else. It has no operating system, no notion of an outer terminal,
-- no multiplexer policy. So there is a class of escape sequences an inner app
-- sends that libvterm structurally /cannot/ answer correctly — the app asking
-- its terminal about the world around it: the OS light\/dark scheme, the real
-- background color, whether unknown sequences should be tunnelled to the
-- outer terminal.
--
-- Those belong to hat, not libvterm, and this module owns them. It is the
-- deliberate seam between the two:
--
--   * __libvterm__ owns the /screen/ — SGR, wide chars, scrollback, regions.
--   * __this layer__ owns the /host protocol/ — the app's questions about its
--     environment that only the host (hat) can answer.
--
-- It is not the beginning of replacing libvterm; the grid engine is exactly
-- where libvterm is excellent and where hat has no bugs. The charter test for
-- whether a sequence belongs here: /does answering it require knowledge
-- libvterm cannot have?/ The OS color scheme (OSC 10\/11, DEC mode 2031), the
-- multiplexer's passthrough policy (DCS @tmux;@), routing a desktop
-- notification (OSC 9\/777) out to the real terminal — yes. One neighbour,
-- 'dropDecxcprReply', is a different category: not host knowledge but a
-- libvterm quirk-correction (it answers a query real terminals ignore); it
-- lives here because it too is peripheral rewriting of the byte stream that
-- crosses the emulator boundary.
--
-- Everything here is pure: bytes in, signals and rewritten bytes out. The
-- 'Hat.Term.Emulator' 'Hat.Term.Emulator.feed' loop is the thin IO wiring
-- that runs these over libvterm; 'Hat.Server' is the host that answers the
-- signals with knowledge it holds (see 'Hat.Server.applyScheme').
module Hat.Term.HostProtocol
    ( -- * OSC color-query vocabulary
      OscColorTarget (..)
    , OscTerm (..)
      -- * DCS tmux passthrough
    , PassState (..)
    , scrubPassthrough
      -- * DEC private-mode cursor reports
    , dropDecxcprReply
      -- * Color queries answered by the host
    , QuerySignal (..)
    , CsSignal (..)
    , nextQuery
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
-- before libvterm parses it, returning the scrubbed bytes and any desktop
-- notifications carried inside the wrappers.
--
-- A tmux-aware app (claude) sees $TMUX set and wraps sequences meant for the
-- outer terminal in this DCS; libvterm's parser aborts on the wrapper's
-- doubled inner ESCs and spills the payload onto the screen as text (the
-- \"11;?9;4;0;\" garbage). So the wrapper never reaches libvterm. tmux's
-- default (@allow-passthrough off@) then discards the payload entirely; hat
-- goes one step further and honours just OSC 9\/777 desktop notifications out
-- of it (the point of a passthrough the user actually wants delivered),
-- discarding everything else. A wrapper can span pty reads, so the state —
-- including the partial payload — carries across 'feed' chunks.
scrubPassthrough :: PassState -> ByteString -> (PassState, ByteString, [ByteString])
scrubPassthrough st0 chunk = case st0 of
    Outside carry        -> outside [] [] (carry <> chunk)
    Inside carry payload -> inside [] [] [payload] (carry <> chunk)
  where
    intro = "\ESCPtmux;"
    finish = B.concat . reverse
    outside oacc notifs bs = case B.breakSubstring intro bs of
        (before, r)
            | B.null r ->
                -- No wrapper here; hold back a chunk-final partial intro
                -- (e.g. a trailing bare ESC) until the next read decides.
                let held = introSuffix before
                    emit = B.take (B.length before - B.length held) before
                in (Outside held, finish (emit : oacc), reverse notifs)
            | otherwise -> inside (before : oacc) notifs [] (B.drop (B.length intro) r)
    -- Inside the wrapper nothing reaches libvterm; the payload is rebuilt
    -- (un-doubling the ESCs the wrapper doubled) until the ST — a lone ESC
    -- followed by backslash — closes it and its notifications are harvested.
    inside oacc notifs pacc bs = case B.elemIndex 0x1b bs of
        Nothing -> (Inside "" (finish (bs : pacc)), finish oacc, reverse notifs)
        Just i ->
            let pacc' = B.take i bs : pacc
            in case B.uncons (B.drop (i + 1) bs) of
                Nothing           -> (Inside "\ESC" (finish pacc'), finish oacc, reverse notifs)
                Just (0x1b, rest) -> inside oacc notifs ("\ESC" : pacc') rest
                Just (0x5c, rest) ->
                    let found = extractNotifies (finish pacc')
                    in outside oacc (reverse found <> notifs) rest
                Just (c, rest)    -> inside oacc notifs (B.singleton c : pacc') rest
    -- The longest proper prefix of the intro that this chunk ends with.
    introSuffix bs =
        let cap = min (B.length intro - 1) (B.length bs)
            ks = [ k | k <- [cap, cap - 1 .. 1]
                 , B.take k intro `B.isSuffixOf` bs ]
        in case ks of
            (k : _) -> B.drop (B.length bs - k) bs
            []      -> ""

-- | Every OSC 9/777 desktop notification found in a byte string, in order.
extractNotifies :: ByteString -> [ByteString]
extractNotifies bs = case nextQuery bs of
    Just (_, SigNotify raw, more) -> raw : extractNotifies more
    Just (_, _, more)             -> extractNotifies more
    Nothing                       -> []

-- | Strip DEC-private cursor reports (DECXCPR, @CSI ? … R@) from the
-- emulator's replies. Most terminals — ghostty included — ignore
-- @CSI ? 6 n@, so libvterm's answer to it arrives unexpected at the shell
-- when the inner app exits or resumes, and the line editor spills its bare
-- parameters as visible \"9;4;0\" garbage. Plain CPR (@CSI … R@) and every
-- other reply pass through untouched.
dropDecxcprReply :: ByteString -> ByteString
dropDecxcprReply bs =
    case B.breakSubstring "\ESC[?" bs of
        (before, rest)
            | B.null rest -> bs
            | otherwise ->
                let afterIntro = B.drop 3 rest            -- past ESC [ ?
                    (_params, tailB) = B.span isParam afterIntro
                in case B.uncons tailB of
                    Just (0x52, more) -> before <> dropDecxcprReply more
                    _ -> before <> "\ESC[?" <> dropDecxcprReply afterIntro
  where
    isParam b = (b >= 0x30 && b <= 0x39) || b == 0x3b   -- 0-9 or ';'

-- | A color control the inner app sent to its terminal, which hat answers
-- itself: neither libvterm nor the outer terminal knows the OS scheme hat
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
-- libvterm: a DEC 2031 control (only the standalone form; one folded into a
-- multi-parameter DECSET is left for libvterm), an OSC 10/11 color query
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
    -- chunk ends before one — an unterminated notify is left for libvterm
    -- (which drops it) rather than forwarded half-formed.
    afterTerminator r = case B.uncons r of
        Nothing -> Nothing
        Just (0x07, more) -> Just more                      -- BEL
        Just (0x1b, more)
            | Just (0x5c, rest) <- B.uncons more -> Just rest  -- ST (ESC \)
        Just (_, more) -> afterTerminator more
