{-# LANGUAGE RoleAnnotations #-}

-- | Copy-mode motion, selection, and extraction. Motions are a
-- faithful port of tmux's @grid-reader.c@ and the selection extraction
-- of @window-copy.c@, expressed over an abstract 'Grid' so they can be
-- exercised as pure state transitions in tests.
--
-- Coordinate space: rows are ABSOLUTE within the pane's grid. Row @0@
-- is the oldest scrollback line; rows @hsize..hsize+sy-1@ are the live
-- screen. Columns are 0-indexed. A copy-mode cursor is @(row, col)@.
module Hat.Server.CopyMode
    ( Handler
    , handlers
    , Grid (..)
    , Motion
    , listGrid
    , gridWord
    , gridLine
    , freezeGrid
    , frozenGrid
    , runMotion
    , mNextWord
    , mNextWordEnd
    , mPreviousWord
    , mCursorLeft
    , mCursorRight
    , ColumnBound (..)
    , selColumnBound
    , cursorVertical
    , endOfLine
    , scrollUp
    , scrollDown
    , pushDigit
    , historyBottom
    , topLine
    , middleLine
    , bottomLine
    , backToIndentation
    , paragraphUp
    , paragraphDown
    , beginLineSelection
    , rectangleToggle
    , otherEnd
    , selectionEnd
    , clearSelection
    , stopSelection
    , scrollUpCmd
    , scrollCursorTo
    , recentreTopBottom
    , charSearch
    , SearchPattern
    , compilePattern
    , findMatch
    , SearchWrap (..)
    , extractSelection
    , yankSelection
    , overlaySelection
    , copyCursorPos
    , copyModeFormats
    , startDragSelection
    , dragSelectionTo
    , cursorToScreen
    , scrollToCursor
    , withDevNullSink
    , pipeCommand
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, catch)
import Control.Monad (forM, forM_, void)
import Data.Array ((!))
import Data.Char (isSpace, toLower)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text.Encoding qualified as TE
import Data.Text (Text)
import Data.Text qualified as T
import System.IO (Handle, IOMode (WriteMode), hClose, hPutStr, openFile)
import Text.Regex.TDFA
    (CompOption (..), Regex, defaultCompOpt, defaultExecOpt, matchAll)
import Text.Regex.TDFA.Text qualified as Regex
import System.Process
    ( CreateProcess (..)
    , StdStream (CreatePipe, UseHandle)
    , createProcess
    , shell
    , waitForProcess
    )
import Data.Vector qualified as V

import Hat.Geometry (Pos (..), Size (..))
import Hat.Model
import Hat.Model.Options
import Hat.Server.Clipboard (pushBufferBytes, setSelection)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu

-- ---------------------------------------------------------------------
-- Grid abstraction

-- | A read-only view of a pane's absolute grid. All access is monadic
-- so the same motion code runs against the live emulator (@m ~ IO@) and
-- against a fixed test grid (@m ~ Identity@).
type role Grid representational
data Grid m = Grid
    { gHsize    :: !Int              -- ^ scrollback line count
    , gSy       :: !Int              -- ^ visible rows
    , gSx       :: !Int              -- ^ columns
    , gChar     :: Int -> Int -> m Char
        -- ^ character at (row, col); @' '@ for blank / out of bounds
    , gLineLen  :: Int -> m Int      -- ^ trimmed content length of a row
    , gPadding  :: Int -> Int -> m Bool
        -- ^ is (row, col) the padding column behind a wide character?
    , gWrapped  :: Int -> m Bool     -- ^ is the row a wrapped continuation?
    }

-- | Bottom-most absolute row.
gBottom :: Grid m -> Int
gBottom g = g.gHsize + g.gSy - 1

-- | Build a 'Grid' over a list of text rows, for tests. The grid has no
-- scrollback; wrapped is always false; line length trims trailing
-- spaces.
listGrid :: Int -> [Text] -> Grid Identity
listGrid sx rows = Grid
    { gHsize = 0
    , gSy = length rows
    , gSx = sx
    , gChar = \r c -> pure $ case drop r rows of
        (row : _) -> case T.drop c row of
            t | not (T.null t) -> T.head t
            _ -> ' '
        [] -> ' '
    , gLineLen = \r -> pure $ case drop r rows of
        (row : _) -> T.length (T.dropWhileEnd (== ' ') (T.take sx row))
        [] -> 0
    , gWrapped = \_ -> pure False
    , gPadding = \_ _ -> pure False
    }

-- | Capture a pane's absolute grid — every scrollback line, then the
-- live screen — into an immutable 'FrozenGrid'. Copy mode reads this
-- snapshot for the whole session so the program's later output stays
-- invisible until the mode exits.
freezeGrid :: Emu.Emulator -> IO FrozenGrid
freezeGrid e = do
    hsize <- Emu.scrollbackLength e
    scr <- Emu.snapshot e
    let sy = V.length scr.cells
        sx = fromIntegral scr.size.cols
    sb <- forM [0 .. hsize - 1] $ \i ->
        maybe V.empty id <$> Emu.scrollbackLine e i
    pure FrozenGrid
        { fgHsize = hsize
        , fgSy = sy
        , fgSx = sx
        , fgRows = V.fromList sb <> scr.cells
        }

-- | A read-only 'Grid' over a frozen snapshot. Blank / out-of-bounds
-- cells read as spaces; line length trims trailing blanks (a wide
-- character's padding column is content, as in tmux's
-- @grid_line_length@); rows never wrap in this model.
frozenGrid :: Monad m => FrozenGrid -> Grid m
frozenGrid fg = Grid
    { gHsize = fg.fgHsize
    , gSy = fg.fgSy
    , gSx = fg.fgSx
    , gChar = \row col -> pure $ maybe ' ' Cell.baseChar (rowAt row V.!? col)
    , gLineLen = \row ->
        let cs = rowAt row
            lastFilled = V.ifoldr keep (-1) cs
            keep i c acc
                | isPadding c = max acc i
                | isSpace (Cell.baseChar c) = acc
                | otherwise = max acc i
        in pure (min fg.fgSx (lastFilled + 1))
    , gWrapped = \_ -> pure False
    , gPadding = \row col -> pure (maybe False isPadding (rowAt row V.!? col))
    }
  where
    rowAt row = fromMaybe V.empty (fg.fgRows V.!? row)
    isPadding c = c.content == Cell.Continuation

-- ---------------------------------------------------------------------
-- Character classes (tmux WHITESPACE is "\t ")

whitespace :: Char -> Bool
whitespace c = c == ' ' || c == '\t'

inSet :: Text -> Char -> Bool
inSet set c = T.any (== c) set

-- ---------------------------------------------------------------------
-- Grid reader (port of grid-reader.c)

-- | The reader cursor plus the current line's right bound @xx@, which
-- 'handleWrap' updates as it crosses line boundaries.
data Rdr = Rdr
    { rc  :: !Int   -- ^ column
    , rr  :: !Int   -- ^ absolute row
    , rxx :: !Int   -- ^ right bound of the current line
    }

charAtR :: Grid m -> Rdr -> m Char
charAtR g s = g.gChar s.rr s.rc

-- | Right bound for a row: @sx-1@ if wrapped, else its content length.
lineBound :: Monad m => Grid m -> Int -> m Int
lineBound g row = do
    w <- g.gWrapped row
    if w then pure (g.gSx - 1) else g.gLineLen row

-- | @grid_reader_handle_wrap@: keep the cursor within the bounding area,
-- wrapping to following lines as needed. 'Nothing' means the cursor
-- would run past the bottom of the grid.
handleWrap :: Monad m => Grid m -> Rdr -> m (Maybe Rdr)
handleWrap g = loop
  where
    yy = gBottom g
    loop s
        | s.rc > s.rxx =
            if s.rr == yy
                then pure Nothing
                else do
                    let row' = s.rr + 1
                    xx' <- lineBound g row'
                    loop s { rc = 0, rr = row', rxx = xx' }
        | otherwise = pure (Just s)

-- | Seed a reader at a position, computing the initial line bound.
startRdr :: Monad m => Grid m -> Int -> Int -> m Rdr
startRdr g row col = do
    xx <- lineBound g row
    pure Rdr { rc = col, rr = row, rxx = xx }

-- | @do cx++ while (handle_wrap && p)@: pre-increment then test.
skipStepping :: Monad m => Grid m -> (Char -> Bool) -> Rdr -> m Rdr
skipStepping g p = go
  where
    go s = do
        m <- handleWrap g s { rc = s.rc + 1 }
        case m of
            Nothing -> pure s { rc = s.rc + 1 }
            Just s' -> do
                c <- charAtR g s'
                if p c then go s' else pure s'

-- | @while (handle_wrap && p) cx++@: test then increment.
skipRun :: Monad m => Grid m -> (Char -> Bool) -> Rdr -> m Rdr
skipRun g p = go
  where
    go s = do
        m <- handleWrap g s
        case m of
            Nothing -> pure s
            Just s' -> do
                c <- charAtR g s'
                if p c then go s' { rc = s'.rc + 1 } else pure s'

-- | Whether 'cursorRight' wraps onto the next line at the right bound.
data Wrap = Wrap | NoWrap
    deriving (Eq, Show)

-- | The column bound 'cursorRight' respects: 'AllColumns' allows the full
-- grid width; 'ContentColumns' stops at the line's content length.
data ColumnBound = AllColumns | ContentColumns
    deriving (Eq, Show)

-- | Whether 'cursorRight' may rest one column past the last character
-- ('OneMore', as copy mode does) or must stop on it ('Inclusive').
data PastEnd = OneMore | Inclusive
    deriving (Eq, Show)

-- | @grid_reader_cursor_right@ with (wrap, all, onemore) flags. A step
-- never comes to rest on a wide character's padding column.
cursorRight :: Monad m => Grid m -> Wrap -> ColumnBound -> PastEnd -> Rdr -> m Rdr
cursorRight g wrap columnBound pastEnd s = do
    px <- case columnBound of
        AllColumns -> pure g.gSx
        ContentColumns -> case pastEnd of
            OneMore -> g.gLineLen s.rr
            Inclusive -> lineLimit g s.rr
    if wrap == Wrap && s.rc >= px && s.rr < gBottom g
        then pure s { rc = 0, rr = s.rr + 1 }
        else if s.rc < px
            then do
                let skip c
                        | c < px = do
                            pad <- g.gPadding s.rr c
                            if pad then skip (c + 1) else pure c
                        | otherwise = pure c
                c' <- skip (s.rc + 1)
                pure s { rc = c' }
            else pure s

-- | @grid_line_limit@: the last column of a row's content, stepping back
-- off a wide character's padding.
lineLimit :: Monad m => Grid m -> Int -> m Int
lineLimit g row = do
    len <- g.gLineLen row
    if len == 0 then pure 0 else offPadding (len - 1)
  where
    offPadding c
        | c > 0 = do
            pad <- g.gPadding row c
            if pad then offPadding (c - 1) else pure c
        | otherwise = pure c

-- | @grid_reader_cursor_left@: first back off any padding column, then step
-- left — or, at column 0, onto the end of the previous line when @wrap@ (or
-- that line wrapped).
cursorLeft :: Monad m => Grid m -> Bool -> Rdr -> m Rdr
cursorLeft g wrap s0 = do
    c0 <- offPadding s0.rc
    let s = s0 { rc = c0 }
    if s.rc == 0 && s.rr > 0
        then do
            w <- g.gWrapped (s.rr - 1)
            if wrap || w
                then do
                    l <- g.gLineLen (s.rr - 1)
                    pure s { rr = s.rr - 1, rc = l }
                else pure s
        else if s.rc > 0 then pure s { rc = s.rc - 1 } else pure s
  where
    offPadding c
        | c > 0 = do
            pad <- g.gPadding s0.rr c
            if pad then offPadding (c - 1) else pure c
        | otherwise = pure c

-- | @grid_reader_cursor_next_word@.
nextWord :: Monad m => Grid m -> Text -> Rdr -> m Rdr
nextWord g seps s0 = do
    m <- handleWrap g s0
    case m of
        Nothing -> pure s0
        Just s1 -> do
            c <- charAtR g s1
            s2 <- if not (whitespace c)
                then if inSet seps c
                    then skipStepping g (\x -> inSet seps x && not (whitespace x)) s1
                    else skipStepping g (\x -> not (inSet seps x) && not (whitespace x)) s1
                else pure s1
            skipRun g whitespace s2

-- | @grid_reader_cursor_next_word_end@ (base, emacs behaviour).
nextWordEnd :: Monad m => Grid m -> Text -> Rdr -> m Rdr
nextWordEnd g seps = loop
  where
    loop s = do
        m <- handleWrap g s
        case m of
            Nothing -> pure s
            Just s' -> do
                c <- charAtR g s'
                if whitespace c
                    then loop s' { rc = s'.rc + 1 }
                    else if inSet seps c
                        then skipStepping g (\x -> inSet seps x && not (whitespace x)) s'
                        else skipStepping g (\x -> not (inSet seps x || whitespace x)) s'

-- | Whether 'previousWord' begins by stepping back off the current cell
-- ('BackStep', tmux's @already@) or only steps back when already on
-- whitespace ('NoBackStep').
data BackStep = BackStep | NoBackStep
    deriving (Eq, Show)

-- | Whether 'previousWord' stops at the start of a line when scanning back
-- ('StopAtEol', emacs) or crosses the boundary onto the previous line
-- ('CrossEol').
data EolStop = StopAtEol | CrossEol
    deriving (Eq, Show)

-- | @grid_reader_cursor_previous_word@ with @already@ and @stop_at_eol@.
previousWord :: Monad m => Grid m -> Text -> BackStep -> EolStop -> Rdr -> m Rdr
previousWord g seps backStep eolStop s0 = do
    c0 <- charAtR g s0
    step1 <- if backStep == BackStep || whitespace c0
        then phase1 s0
        else pure (Right (s0, not (inSet seps c0)))
    case step1 of
        Left done -> pure done
        Right (s1, wil) -> phase2 wil s1
  where
    -- Move back to the previous word character.
    phase1 s
        | s.rc > 0 = do
            let s1 = s { rc = s.rc - 1 }
            c <- charAtR g s1
            if not (whitespace c)
                then pure (Right (s1, not (inSet seps c)))
                else phase1 s1
        | s.rr == 0 = pure (Left s)
        | otherwise = do
            l <- g.gLineLen (s.rr - 1)
            let s1 = s { rr = s.rr - 1, rc = l }
            if eolStop == StopAtEol && s1.rc > 0
                then do
                    c <- charAtR g s1 { rc = s1.rc - 1 }
                    if whitespace c
                        then pure (Right (s1, False))
                        else phase1 s1
                else phase1 s1
    -- Move back to the beginning of this word.
    phase2 wil = go
      where
        go s = do
            let oldx = s.rc; oldy = s.rr
            afterZero <- if s.rc == 0
                then if s.rr == 0
                    then pure Nothing
                    else do
                        w <- g.gWrapped (s.rr - 1)
                        if not w
                            then pure Nothing
                            else pure (Just s { rr = s.rr - 1, rc = g.gSx })
                else pure (Just s)
            case afterZero of
                Nothing -> pure s { rc = oldx, rr = oldy }
                Just s1 -> do
                    let s2 = if s1.rc > 0 then s1 { rc = s1.rc - 1 } else s1
                    c <- charAtR g s2
                    if not (whitespace c) && (wil /= inSet seps c)
                        then go s2
                        else pure s2 { rc = oldx, rr = oldy }

-- ---------------------------------------------------------------------
-- Mode-aware motion wrappers (port of window-copy.c command handlers)

-- | A motion over reader coordinates, parameterised by mode keys and
-- word separators.
type Motion m = Grid m -> ModeKeys -> Text -> Rdr -> m Rdr

mNextWord :: Monad m => Text -> Motion m
mNextWord seps g _ _ = nextWord g seps

mNextWordEnd :: Monad m => Text -> Motion m
mNextWordEnd seps g keys _ s = case keys of
    KeysVi -> do
        c <- charAtR g s
        s1 <- if not (whitespace c) then cursorRight g NoWrap ContentColumns Inclusive s else pure s
        s2 <- nextWordEnd g seps s1
        cursorLeft g True s2
    KeysEmacs -> nextWordEnd g seps s

mPreviousWord :: Monad m => Text -> Motion m
mPreviousWord seps g keys _ =
    previousWord g seps BackStep (if keys == KeysEmacs then StopAtEol else CrossEol)

-- | Move one cell left, onto the end of the previous line from column 0.
mCursorLeft :: Monad m => Motion m
mCursorLeft g _ _ = cursorLeft g True

-- | Move one cell right, onto the next line past the right bound. Emacs
-- keys may rest one column past the last character (@onemore@); vi keys
-- stop on it.
mCursorRight :: Monad m => ColumnBound -> Motion m
mCursorRight bound g keys _ = cursorRight g Wrap bound $ case keys of
    KeysVi -> Inclusive
    KeysEmacs -> OneMore

-- | The column bound the live selection grants: a rectangular selection
-- frees the cursor to the full grid width, so a block can cover a
-- line's trailing blanks.
selColumnBound :: CopyModeState -> ColumnBound
selColumnBound st = case st.selection of
    Just (_, SelRect) -> AllColumns
    _                 -> ContentColumns

-- | Column after a vertical move: clamped to the destination line's
-- length, except under 'AllColumns' where the band crosses shorter
-- lines unclamped.
vertCol :: Monad m => Grid m -> Int -> CopyModeState -> m Int
vertCol g row st = case selColumnBound st of
    AllColumns -> pure st.cursorCol
    ContentColumns -> min st.cursorCol <$> g.gLineLen row

-- | Move the cursor @d@ rows (negative = up), clamped to the grid; the
-- column clamps per 'vertCol'.
cursorVertical :: Monad m => Int -> Grid m -> CopyModeState -> m CopyModeState
cursorVertical d g st = do
    let row' = max 0 (min (gBottom g) (st.cursorRow + d))
    col' <- vertCol g row' st
    pure st { cursorRow = row', cursorCol = col' }

-- | Move to the end of the current line — the full grid width during a
-- rectangular selection. Vi lands on the last cell (so an inclusive
-- selection covers it); emacs lands one cell past it (so an exclusive
-- selection covers it).
endOfLine :: Monad m => ModeKeys -> Grid m -> CopyModeState -> m CopyModeState
endOfLine keys g st = do
    len <- case selColumnBound st of
        AllColumns -> pure g.gSx
        ContentColumns -> g.gLineLen st.cursorRow
    pure st { cursorCol = endCol len }
  where
    endCol len = case keys of
        KeysEmacs -> len
        KeysVi -> max 0 (len - 1)

-- | Move to an absolute row, clamping to the grid and to the
-- destination line's length (columns are preserved where possible).
gotoRow :: Monad m => Int -> Grid m -> CopyModeState -> m CopyModeState
gotoRow row g st = do
    let row' = max 0 (min (gBottom g) row)
    len <- g.gLineLen row'
    pure st { cursorRow = row', cursorCol = min st.cursorCol len }

-- | Feed a digit key to the numeric prefix (vi @[count]@). A bare @0@
-- with no count in progress is not a digit but @start-of-line@, so it
-- resets the column instead of starting a count.
pushDigit :: Int -> CopyModeState -> CopyModeState
pushDigit d st = case st.numPrefix of
    Nothing | d == 0 -> st { cursorCol = 0 }
    mp -> st { numPrefix = Just (maybe 0 id mp * 10 + d) }

-- | @history-bottom@ (vi @G@): jump to the last row of the grid.
historyBottom :: Monad m => Grid m -> CopyModeState -> m CopyModeState
historyBottom g = gotoRow (gBottom g) g

-- | Scroll the viewport up by @n@ lines (vi @C-b@/@C-u@): the offset and
-- the cursor move together by however much scrollback is left, so the
-- cursor keeps its position on screen and the view actually scrolls.
scrollUp :: Monad m => Int -> Grid m -> CopyModeState -> m CopyModeState
scrollUp n g st = do
    let voY' = max 0 (min g.gHsize (st.viewportOffY + n))
        moved = voY' - st.viewportOffY
        row' = max 0 (st.cursorRow - moved)
    col' <- vertCol g row' st
    pure st { viewportOffY = voY', cursorRow = row', cursorCol = col' }

-- | Scroll the viewport down by @n@ lines (vi @C-f@/@C-d@), the inverse
-- of 'scrollUp'.
scrollDown :: Monad m => Int -> Grid m -> CopyModeState -> m CopyModeState
scrollDown n g st = do
    let voY' = max 0 (min g.gHsize (st.viewportOffY - n))
        moved = st.viewportOffY - voY'
        row' = min (gBottom g) (st.cursorRow + moved)
    col' <- vertCol g row' st
    pure st { viewportOffY = voY', cursorRow = row', cursorCol = col' }

-- | The absolute grid row currently shown at the top of the viewport.
viewportTop :: Grid m -> CopyModeState -> Int
viewportTop g st = g.gHsize - st.viewportOffY

-- | @top-line@ (vi @H@): the first visible row of the viewport.
topLine :: Monad m => Grid m -> CopyModeState -> m CopyModeState
topLine g st = gotoRow (viewportTop g st) g st

-- | @middle-line@ (vi @M@): the middle visible row of the viewport.
middleLine :: Monad m => Grid m -> CopyModeState -> m CopyModeState
middleLine g st = gotoRow (viewportTop g st + (g.gSy - 1) `div` 2) g st

-- | @bottom-line@ (vi @L@): the last visible row of the viewport.
bottomLine :: Monad m => Grid m -> CopyModeState -> m CopyModeState
bottomLine g st = gotoRow (viewportTop g st + g.gSy - 1) g st

-- | @back-to-indentation@ (vi @^@): the first non-blank column of the
-- current line, or column 0 when the line is blank.
backToIndentation :: Monad m => Grid m -> CopyModeState -> m CopyModeState
backToIndentation g st = do
    len <- g.gLineLen st.cursorRow
    col <- firstNonBlank 0 len
    pure st { cursorCol = col }
  where
    firstNonBlank i len
        | i >= len = pure 0
        | otherwise = do
            c <- g.gChar st.cursorRow i
            if whitespace c then firstNonBlank (i + 1) len else pure i

-- | vi character search on the current line (@f@/@F@/@t@/@T@). Scans from
-- the cell next to the cursor for @target@; @to@ lands on it, @till@ one
-- cell short. No move if the target is not found.
charSearch
    :: Monad m => Grid m -> CharSearch -> Char -> CopyModeState -> m CopyModeState
charSearch g cs target st = do
    len <- g.gLineLen st.cursorRow
    mcol <- case cs.direction of
        Forward  -> findFrom (st.cursorCol + 1) (\i -> i < len) (+ 1)
        Backward -> findFrom (st.cursorCol - 1) (>= 0) (subtract 1)
    pure $ case mcol of
        Nothing -> st
        Just j -> st { cursorCol = landing j }
  where
    row = st.cursorRow
    landing j = case (cs.stop, cs.direction) of
        (OnTarget, _)             -> j
        (ShortOfTarget, Forward)  -> j - 1
        (ShortOfTarget, Backward) -> j + 1
    findFrom i inBounds step
        | not (inBounds i) = pure Nothing
        | otherwise = do
            c <- g.gChar row i
            if c == target then pure (Just i) else findFrom (step i) inBounds step

-- | The trimmed text of a grid row.
rowText :: Monad m => Grid m -> Int -> m Text
rowText g r = do
    len <- g.gLineLen r
    T.pack <$> mapM (g.gChar r) [0 .. len - 1]

-- | A compiled string-search pattern, ready to scan grid rows. Built by
-- 'compilePattern'; consumed by 'findMatch'.
data SearchPattern = PatText !QueryCase !Text | PatRegex !Regex

-- | Smart-case verdict for a query: see 'compilePattern'.
data QueryCase = MatchCase | IgnoreCase

-- | Compile a search pattern per its kind: 'SearchRegex' as a POSIX
-- extended regex (tmux's @regcomp@ flavor), 'SearchText' verbatim (never
-- fails). Both kinds smart-case, per tmux: a query in which every
-- character is its own 'toLower' image matches case-insensitively; any
-- uppercase character makes the search exact. An invalid regex is a
-- 'Left' the caller must surface.
compilePattern :: SearchKind -> Text -> Either Text SearchPattern
compilePattern kind query = case kind of
    SearchText -> Right $ case qcase of
        MatchCase  -> PatText MatchCase query
        IgnoreCase -> PatText IgnoreCase (T.map toLower query)
    SearchRegex ->
        case Regex.compile compOpt defaultExecOpt query of
            Left err -> Left ("invalid regex: " <> T.pack err)
            Right re -> Right (PatRegex re)
  where
    qcase
        | T.all (\c -> c == toLower c) query = IgnoreCase
        | otherwise = MatchCase
    compOpt = case qcase of
        MatchCase  -> defaultCompOpt
        IgnoreCase -> defaultCompOpt { caseSensitive = False }

-- | Every start column of the pattern within @txt@. Literal scans resume
-- one cell past the previous start (overlapping matches found); regexes
-- run over the whole line in one pass, so @^@ anchors only at column 0.
matchCols :: SearchPattern -> Text -> [Int]
matchCols (PatRegex re) = map (fst . (! 0)) . matchAll re
matchCols (PatText qcase query) = go 0 . canon
  where
    -- per-char lowering keeps columns aligned with the original row
    canon = case qcase of
        MatchCase  -> id
        IgnoreCase -> T.map toLower
    go _ _ | T.null query = []
    go off txt = case T.breakOn query txt of
        (pre, rest)
            | T.null rest -> []
            | otherwise ->
                let i = off + T.length pre
                in i : go (i + 1) (T.drop (T.length pre + 1) txt)

-- | All matches of the pattern across the grid, in reading order.
gridMatches :: Monad m => Grid m -> SearchPattern -> m [(Int, Int)]
gridMatches g p =
    fmap concat . forM [0 .. gBottom g] $ \r -> do
        txt <- rowText g r
        pure [ (r, c) | c <- matchCols p txt ]

-- | Whether a search past the last match starts over from the other end
-- (@wrap-search@).
data SearchWrap = WrapAround | StopAtEnd
    deriving (Eq, Show)

-- | The next match of the pattern from @(row, col)@ in the given direction,
-- wrapping around the grid if allowed. 'Nothing' when there is none.
findMatch
    :: Monad m
    => Grid m -> SearchWrap -> SearchDirection -> SearchPattern -> (Int, Int)
    -> m (Maybe (Int, Int))
findMatch g wrap dir p (row, col) = do
    ms <- gridMatches g p
    pure $ case dir of
        Forward -> firstOf (filter (\(r, c) -> r > row || (r == row && c > col)) ms) ms
        Backward ->
            firstOf (reverse (filter (\(r, c) -> r < row || (r == row && c < col)) ms))
                (reverse ms)
  where
    firstOf (m : _) _ = Just m
    firstOf [] wrapped = case wrap of
        WrapAround -> case wrapped of
            (m : _) -> Just m
            [] -> Nothing
        StopAtEnd -> Nothing

-- | Whether a grid row has no non-blank content.
blankRow :: Monad m => Grid m -> Int -> m Bool
blankRow g r = (== 0) <$> g.gLineLen r

-- | @next-paragraph@ (vi @}@): move down to the next blank line, or the
-- bottom of the grid.
paragraphDown :: Monad m => Grid m -> CopyModeState -> m CopyModeState
paragraphDown g st = loop st.cursorRow >>= \row' -> gotoRow row' g st
  where
    bot = gBottom g
    loop r
        | r >= bot = pure bot
        | otherwise = do
            b <- blankRow g (r + 1)
            if b then pure (r + 1) else loop (r + 1)

-- | @previous-paragraph@ (vi @{@): move up to the previous blank line, or
-- the top of the grid.
paragraphUp :: Monad m => Grid m -> CopyModeState -> m CopyModeState
paragraphUp g st = loop st.cursorRow >>= \row' -> gotoRow row' g st
  where
    loop r
        | r <= 0 = pure 0
        | otherwise = do
            b <- blankRow g (r - 1)
            if b then pure (r - 1) else loop (r - 1)

-- ---------------------------------------------------------------------
-- Selection extraction (port of window_copy_get_selection)

-- | Extract the text covered by the current selection. Rows are joined
-- with @\\n@; end-inclusiveness follows 'ModeKeys' (vi keeps the cell
-- under the cursor, emacs drops it).
extractSelection
    :: Monad m => Grid m -> ModeKeys -> CopyModeState -> m (Maybe Text)
extractSelection g keys st = case st.selection of
    Nothing -> pure Nothing
    -- Line-wise (vi @V@): whole lines from the anchor row to the cursor row.
    Just ((ar, _), SelLine) -> do
        let sy = min ar er
            ey = max ar er
        pieces <- mapM (\i -> g.gLineLen i >>= copyLine g i 0) [sy .. ey]
        let joined = T.concat pieces
        pure . Just $
            if not (T.null joined) && T.last joined == '\n'
                then T.init joined else joined
    -- Rectangular (vi @C-v@): every row clamped to the same column band.
    Just ((ar, ac), SelRect) -> do
        let sy = min ar er
            ey = max ar er
            loCol = min ac ec
            hiCol = case keys of
                KeysEmacs -> max ac ec       -- exclusive end column
                KeysVi    -> max ac ec + 1    -- inclusive end column
        pieces <- mapM (\i -> copyLine g i loCol hiCol) [sy .. ey]
        let joined = T.concat pieces
        pure . Just $
            if not (T.null joined) && T.last joined == '\n'
                then T.init joined else joined
    Just ((ar, ac), _kind) -> do
        let selx = ac; sely = ar
            endx = ec; endy = er
            (sx, sy, ex0, ey)
                | endy < sely || (endy == sely && endx < selx) =
                    (endx, endy, selx, sely)
                | otherwise = (selx, sely, endx, endy)
        eyLast <- g.gLineLen ey
        let ex = min ex0 eyLast
            fullw = g.gSx
            lastex = case keys of
                KeysEmacs -> ex
                KeysVi -> ex + 1
            lineOf i = copyLine g i
                (if i == sy then sx else 0)
                (if i == ey then lastex else fullw)
        pieces <- mapM lineOf [sy .. ey]
        let joined = T.concat pieces
            body
                | (keys == KeysEmacs || lastex <= eyLast)
                    && not (T.null joined) && T.last joined == '\n'
                    = T.init joined
                | otherwise = joined
        pure (Just body)
  where
    (er, ec) = selectionEnd st

-- | One line of a selection, always terminated with @\\n@ (rows never
-- wrap in this model). Returns the empty text if @sx > ex@.
copyLine :: Monad m => Grid m -> Int -> Int -> Int -> m Text
copyLine g row sx ex
    | sx > ex = pure ""
    | otherwise = do
        xx <- g.gLineLen row
        let ex' = min ex xx
            sx' = min sx xx
        cells <- if sx' < ex'
            then mapM (g.gChar row) [sx' .. ex' - 1]
            else pure []
        pure (T.pack cells <> "\n")

-- ---------------------------------------------------------------------
-- Viewport rendering (selection highlight + copy cursor placement)

-- | True if the absolute cell @(row, col)@ lies within the current
-- selection. Honors vi's inclusive end cell; @sx@ bounds the row width.
cellSelected :: ModeKeys -> CopyModeState -> Int -> Int -> Int -> Bool
cellSelected keys st sx row col = case st.selection of
    Nothing -> False
    -- Line-wise: every column of every row between anchor and cursor.
    Just ((ar, _), SelLine) ->
        row >= min ar er && row <= max ar er && col < sx
    -- Rectangular: the same column band on every row between the ends.
    Just ((ar, ac), SelRect) ->
        let inclusive = case keys of KeysVi -> 1; KeysEmacs -> 0
            lo = min ac ec
            hi = min sx (max ac ec + inclusive)
        in row >= min ar er && row <= max ar er
            && col >= lo && col < hi
    Just ((ar, ac), _) ->
        let (sRow, sCol, eRow, eCol)
                | (er, ec) `before` (ar, ac) =
                    (er, ec, ar, ac)
                | otherwise = (ar, ac, er, ec)
            inclusive = case keys of KeysVi -> 1; KeysEmacs -> 0
            lo = if row == sRow then sCol else 0
            hi = min sx (if row == eRow then eCol + inclusive else sx)
        in row >= sRow && row <= eRow && col >= lo && col < hi
  where
    (er, ec) = selectionEnd st
    before (a, b) (x, y) = a < x || (a == x && b < y)

-- | Paint @mode-style@ over the selected cells of an assembled viewport
-- grid. @topRow@ is the absolute grid row shown at visible row 0.
overlaySelection
    :: Cell.Style -> ModeKeys -> Int -> CopyModeState
    -> V.Vector (V.Vector Cell.Cell) -> V.Vector (V.Vector Cell.Cell)
overlaySelection modeStyle keys topRow st = V.imap overRow
  where
    overRow i row = V.imap (overCell (topRow + i) (V.length row)) row
    overCell a sx col cell
        | cellSelected keys st sx a col =
            cell { Cell.style = applyModeStyle modeStyle cell.style }
        | otherwise = cell

-- | Lay the @mode-style@ over a cell's own style: a concrete colour in the
-- mode style replaces the cell's, a set attribute is added, and @reverse@
-- toggles. So the default @reverse@-only mode style keeps flipping the
-- cell's own colours (classic reverse-video selection), while a themed
-- @bg=…,fg=…@ overrides them with the highlight colour.
applyModeStyle :: Cell.Style -> Cell.Style -> Cell.Style
applyModeStyle ms s = s
    { Cell.fg        = pick ms.fg s.fg
    , Cell.bg        = pick ms.bg s.bg
    , Cell.bold      = s.bold || ms.bold
    , Cell.underline = s.underline || ms.underline
    , Cell.italic    = s.italic || ms.italic
    , Cell.reverse   = s.reverse /= ms.reverse
    , Cell.strike    = s.strike || ms.strike
    , Cell.blink     = s.blink || ms.blink
    }
  where
    pick Cell.DefaultColor old = old
    pick new              _   = new

-- | Where the copy cursor sits within a viewport of @sy@ rows whose top
-- line is absolute row @topRow@; 'Nothing' when it is scrolled off.
copyCursorPos :: Int -> Int -> CopyModeState -> Maybe Pos
copyCursorPos topRow sy st =
    let r = st.cursorRow - topRow
    in if r >= 0 && r < sy
        then Just Pos { row = r, col = st.cursorCol }
        else Nothing

-- | tmux's @window_copy_formats@: the copy-mode variables of a pane in the
-- mode, words split by the given separators.
copyModeFormats :: Text -> PaneMode -> [(Text, Text)]
copyModeFormats seps pm =
    [ ("scroll_position", tshow s.viewportOffY)
    , ("copy_position", tshow s.viewportOffY)
    , ("copy_position_limit", tshow hsize)
    , ("rectangle_toggle", flag (fmap snd s.selection == Just SelRect))
    , ("copy_cursor_x", tshow s.cursorCol)
    , ("copy_cursor_y", tshow (s.cursorRow - top))
    , ("copy_cursor_line", runIdentity (gridLine g s.cursorRow))
    , ("copy_cursor_word", runIdentity (gridWord seps g s.cursorRow s.cursorCol))
    , ("selection_mode", case fmap snd s.selection of
        Just SelLine -> "line"
        Just SelWord -> "word"
        _ -> "char")
    ] <> case s.selection of
        Nothing -> [("selection_active", "0"), ("selection_present", "0")]
        Just (anchor, _) ->
            let far = selectionEnd s
                ((sy, sx), (ey, ex)) = case s.anchorAt of
                    AnchorAtStart -> (anchor, far)
                    AnchorAtEnd -> (far, anchor)
            in [ ("selection_start_x", tshow sx)
               , ("selection_start_y", tshow sy)
               , ("selection_end_x", tshow ex)
               , ("selection_end_y", tshow ey)
               , ("selection_active", flag (isNothing s.pinnedEnd))
               , ("selection_present", flag (anchor /= far))
               ]
  where
    s = pm.copyState
    g = frozenGrid pm.frozen :: Grid Identity
    hsize = pm.frozen.fgHsize
    top = hsize - s.viewportOffY
    flag b = if b then "1" else "0"

-- | Adjust @viewportOffY@ (lines scrolled up from the live screen) so the
-- copy cursor stays visible in an @sy@-row viewport over a grid with
-- @hsize@ scrollback lines. Preserves the offset when the cursor is
-- already on screen.
scrollToCursor :: Int -> Int -> CopyModeState -> CopyModeState
scrollToCursor hsize sy st =
    let top0 = hsize - st.viewportOffY
        top | st.cursorRow < top0 = st.cursorRow
            | st.cursorRow > top0 + sy - 1 = st.cursorRow - sy + 1
            | otherwise = top0
        voY = max 0 (min hsize (hsize - top))
    in st { viewportOffY = voY }

-- ---------------------------------------------------------------------
-- Wiring to the live pane

-- | The 'Grid' copy mode reads: the frozen snapshot captured when the
-- pane entered copy mode, so program output after entry stays invisible.
-- An empty grid when the pane is not in copy mode (handlers only reach
-- this while a mode is active).
paneGrid :: Pane -> IO (Grid IO)
paneGrid pane = do
    mmode <- readTVarIO pane.mode
    pure $ case mmode of
        Just pm -> frozenGrid pm.frozen
        Nothing -> frozenGrid emptyFrozen
  where
    emptyFrozen = FrozenGrid
        { fgHsize = 0, fgSy = 0, fgSx = 0, fgRows = V.empty }

-- | Run a reader motion against a pane grid, updating the cursor. Outside a
-- rectangle selection the cursor lands no further right than the line
-- allows — the last character under vi keys, one past it under emacs —
-- as tmux's @window_copy_update_cursor@ clamps every motion.
runMotion
    :: Monad m
    => Grid m -> ModeKeys -> Text -> Motion m -> CopyModeState
    -> m CopyModeState
runMotion g keys seps motion st = do
    s0 <- startRdr g st.cursorRow st.cursorCol
    s1 <- motion g keys seps s0
    col <- case st.selection of
        Just (_, SelRect) -> pure s1.rc
        _ -> do
            maxx <- case keys of
                KeysVi -> lineLimit g s1.rr
                KeysEmacs -> g.gLineLen s1.rr
            pure (min maxx s1.rc)
    pure st { cursorRow = s1.rr, cursorCol = col }

-- ---------------------------------------------------------------------
-- Handler table

-- | A copy-mode @-X@ handler. Receives the command's positional args
-- (e.g. the shell command for @copy-pipe@). 'Right Nothing' means \"exit
-- copy mode\"; 'Left' is an error to show the client, mode unchanged.
type Handler
    = ServerState -> Pane -> CopyModeState -> [Text]
    -> IO (Either Text (Maybe CopyModeState))

handlers :: Map Text Handler
handlers = Map.fromList
    [ ("cancel",            \_ _ _ _ -> pure (Right Nothing))
    , ("start-of-line",     pureH (\s -> s { cursorCol = 0 }))
    , ("history-top",       pureH (\s -> s { cursorRow = 0, cursorCol = 0 }))
    , ("begin-selection",   pureH beginSelection)
    , ("select-line",       pureH beginLineSelection)
    , ("rectangle-toggle",  pureH rectangleToggle)
    , ("other-end",         pureH otherEnd)
    , ("clear-selection",   pureH clearSelection)
    , ("stop-selection",    gridH stopSelection)
    , ("scroll-top",        gridH (\g -> pure . scrollCursorTo 0 g))
    , ("scroll-middle",     gridH (\g -> pure . scrollCursorTo ((g.gSy - 1) `div` 2) g))
    , ("scroll-bottom",     gridH (\g -> pure . scrollCursorTo (g.gSy - 1) g))
    , ("recentre-top-bottom", gridH (\g -> pure . recentreTopBottom g))
    , ("next-word",         motionH (\_ seps -> mNextWord seps))
    , ("next-word-end",     motionH (\_ seps -> mNextWordEnd seps))
    , ("previous-word",     motionH (\_ seps -> mPreviousWord seps))
    , ("next-space",        motionH (\_ _ -> mNextWord ""))
    , ("next-space-end",    motionH (\_ _ -> mNextWordEnd ""))
    , ("previous-space",    motionH (\_ _ -> mPreviousWord ""))
    , ("cursor-left",       motionH (\_ _ -> mCursorLeft))
    , ("cursor-right",      motionH (\st _ -> mCursorRight (selColumnBound st)))
    , ("cursor-up",         gridH (cursorVertical (-1)))
    , ("cursor-down",       gridH (cursorVertical 1))
    , ("page-up",           gridH (\g -> scrollUp g.gSy g))
    , ("page-down",         gridH (\g -> scrollDown g.gSy g))
    , ("halfpage-up",       gridH (\g -> scrollUp (g.gSy `div` 2) g))
    , ("halfpage-down",     gridH (\g -> scrollDown (g.gSy `div` 2) g))
    , ("history-bottom",    gridH historyBottom)
    , ("top-line",          gridH topLine)
    , ("middle-line",       gridH middleLine)
    , ("bottom-line",       gridH bottomLine)
    , ("back-to-indentation", gridH backToIndentation)
    , ("next-paragraph",    gridH paragraphDown)
    , ("previous-paragraph", gridH paragraphUp)
    -- Char search: f/F/t/T arm a pending search; the next key (captured in
    -- handleKeys) runs @apply-search@; ; and , repeat it.
    , ("jump-forward",      pureH (armSearch Forward OnTarget))
    , ("jump-backward",     pureH (armSearch Backward OnTarget))
    , ("jump-to-forward",   pureH (armSearch Forward ShortOfTarget))
    , ("jump-to-backward",  pureH (armSearch Backward ShortOfTarget))
    , ("jump-again",        gridH (jumpRepeat id))
    , ("jump-reverse",      gridH (jumpRepeat flipDir))
    , ("apply-search",      applySearchH)
    , ("cancel-search",     pureH (\s -> s { pendingSearch = Nothing }))
    -- String search: / and ? submit a query via command-prompt; n/N
    -- repeat. The default commands take a regex (tmux >= 3.1); the
    -- -text variants match the pattern literally.
    , ("search-forward",        searchStringH SearchRegex Forward)
    , ("search-backward",       searchStringH SearchRegex Backward)
    , ("search-forward-text",   searchStringH SearchText Forward)
    , ("search-backward-text",  searchStringH SearchText Backward)
    , ("search-again",      searchRepeatH id)
    , ("search-reverse",    searchRepeatH flipDirection)
    , ("end-of-line",       endOfLineH)
    , ("copy-selection",
        \sst p s _ -> yankSelection sst p s
            >> pure (Right (Just (clearSelection s))))
    , ("copy-selection-no-clear",
        \sst p s _ -> yankSelection sst p s >> pure (Right (Just s)))
    , ("copy-selection-and-cancel",
        \sst p s _ -> yankSelection sst p s >> pure (Right Nothing))
    , ("copy-pipe",            pipeH False)
    , ("copy-pipe-and-cancel", pipeH True)
    , ("scroll-up",            gridH scrollUpCmd)
    , ("scroll-down",          scrollDownH)
    , ("select-word",          selectWordH)
    ]
  where
    -- tmux's scroll-down: at the bottom, copy-mode -e exits (unless
    -- selecting); with a selection only the view moves; otherwise the
    -- cursor goes down a line, exiting at the bottom under -e.
    scrollDownH _ pane st _ = do
        g <- paneGrid pane
        let exitHere s = s.scrollExit && s.viewportOffY == 0 && isNothing s.selection
        if st.viewportOffY == 0
            then pure (Right (if exitHere st then Nothing else Just st))
            else case st.selection of
                Just _ -> Right . Just <$> scrollDown 1 g (pinSelection st)
                Nothing -> do
                    st' <- cursorVertical 1 g st
                    let st'' = scrollToCursor g.gHsize g.gSy st'
                    pure (Right (if exitHere st'' then Nothing else Just st''))
    -- tmux's select-word: the word under the cursor, selected.
    selectWordH sst pane st _ = do
        opts <- paneOpts sst pane
        g <- paneGrid pane
        let seps = opts.wordSeparators
            motion m = runMotion g opts.modeKeys seps (m seps)
        atEnd <- motion mNextWordEnd =<< motion mPreviousWord st
        start <- motion mPreviousWord st
        pure (Right (Just atEnd
            { selection = Just ((start.cursorRow, start.cursorCol), SelChar)
            , pinnedEnd = Nothing, anchorAt = AnchorAtStart }))
    pureH f _ _ s _ = pure (Right (Just (f s)))
    motionH mk sst pane st _ = do
        opts <- paneOpts sst pane
        g <- paneGrid pane
        st' <- runMotion g opts.modeKeys opts.wordSeparators
            (mk st opts.wordSeparators) st
        pure (Right (Just st'))
    gridH f _ pane st _ = do
        g <- paneGrid pane
        Right . Just <$> f g st
    endOfLineH sst pane st _ = do
        opts <- paneOpts sst pane
        g <- paneGrid pane
        Right . Just <$> endOfLine opts.modeKeys g st
    armSearch dir stop' s = s { pendingSearch = Just (CharSearch dir stop') }
    flipDir cs = cs { direction = flipDirection cs.direction }
    jumpRepeat modify g st = case st.lastSearch of
        Nothing -> pure st
        Just (cs, c) -> charSearch g (modify cs) c st
    -- The captured target character arrives as the sole argument.
    applySearchH _ pane st args = case (st.pendingSearch, args) of
        (Just cs, a : _) | Just (c, _) <- T.uncons a -> do
            g <- paneGrid pane
            st' <- charSearch g cs c st
            pure (Right (Just st'
                { pendingSearch = Nothing, lastSearch = Just (cs, c) }))
        _ -> pure (Right (Just st { pendingSearch = Nothing }))
    -- / and ? submit their query as the command args.
    searchStringH kind dir sst pane st args = do
        let query = T.unwords args
        if T.null query
            then pure (Right (Just st))
            else runSearch sst dir kind query pane
                st { lastQuery = Just (query, dir, kind) }
    -- n repeats in the stored direction; N (flipDirection) reverses it.
    searchRepeatH reverseDir sst pane st _ = case st.lastQuery of
        Nothing -> pure (Right (Just st))
        Just (query, dir, kind) -> runSearch sst (reverseDir dir) kind query pane st
    runSearch sst dir kind query pane st = case compilePattern kind query of
        Left err -> pure (Left err)
        Right p -> do
            g <- paneGrid pane
            opts <- paneOpts sst pane
            let wrap = if optionFlag "wrap-search" opts then WrapAround else StopAtEnd
            m <- findMatch g wrap dir p (st.cursorRow, st.cursorCol)
            pure . Right . Just $ maybe st
                (\(r, c) -> st { cursorRow = r, cursorCol = c }) m
    -- copy-pipe [-flags] <shell command>: yank the selection into a
    -- buffer AND feed it to @sh -c <command>@ on stdin.
    pipeH cancelAfter sst pane s args = do
        let cmd = T.strip (T.unwords (filter (not . T.isPrefixOf "-") args))
        opts <- paneOpts sst pane
        global <- readTVarIO sst.options
        g <- paneGrid pane
        mtext <- extractSelection g opts.modeKeys s
        forM_ mtext $ \body -> do
            setSelection sst pane (TE.encodeUtf8 body)
            forM_ (pipeCommand global cmd) $ \c -> runPipeCommand (T.unpack c) body
            pushBuffer sst body
        pure . Right $
            if cancelAfter then Nothing else Just (clearSelection s)

-- | The command a copy-pipe runs: its own, else @copy-command@; 'Nothing'
-- when both are empty.
pipeCommand :: Options -> Text -> Maybe Text
pipeCommand opts cmd
    | not (T.null cmd) = Just cmd
    | T.null fallback = Nothing
    | otherwise = Just fallback
  where
    fallback = optionText "copy-command" opts

-- | The options copy mode reads for a pane: its own, else the global ones
-- while it has no home.
paneOpts :: ServerState -> Pane -> IO Options
paneOpts st pane = paneOptionsNow st pane >>= maybe (readTVarIO st.options) pure

-- | Feed a copy-mode selection to @sh -c cmd@ on stdin and return without
-- waiting for the command to finish. The command's stdout and stderr are
-- discarded and the child is reaped on its own thread: xclip forks a
-- selection-owner daemon that inherits stdout and never closes it, so
-- reading that stream to EOF (as 'readCreateProcess' does) would block the
-- caller — here the client's input loop — indefinitely. @close_fds@ keeps
-- that daemon from also inheriting the pane's pty master.
runPipeCommand :: String -> Text -> IO ()
runPipeCommand cmd body = act `catch` \(_ :: SomeException) -> pure ()
  where
    act = withDevNullSink $ \devnull -> do
        (mIn, _, _, ph) <- createProcess (shell cmd)
            { std_in   = CreatePipe
            , std_out  = UseHandle devnull
            , std_err  = UseHandle devnull
            , close_fds = True
            }
        forM_ mIn $ \hIn ->
            (hPutStr hIn (T.unpack body) >> hClose hIn)
                `catch` \(_ :: SomeException) -> pure ()
        void . forkIO $
            void (waitForProcess ph)
                `catch` \(_ :: SomeException) -> pure ()

-- | Run @body@ with a write handle on @\/dev\/null@ for a child's
-- stdout\/stderr, closing it on every exit path. 'bracket' is load-bearing:
-- a 'createProcess' that throws would otherwise orphan the fd (leaking one
-- per copy-pipe on a long-lived server); on the success path 'createProcess'
-- has already closed the handle after dup'ing it into the child, so the
-- releasing 'hClose' is a harmless no-op.
withDevNullSink :: (Handle -> IO a) -> IO a
withDevNullSink = bracket (openFile "/dev/null" WriteMode) hClose

-- | tmux's scroll-up: nothing at the top; with a selection only the view
-- moves, the selection staying put; otherwise the cursor goes up a line.
scrollUpCmd :: Monad m => Grid m -> CopyModeState -> m CopyModeState
scrollUpCmd g st
    | st.viewportOffY >= g.gHsize = pure st
    | Just _ <- st.selection = scrollUp 1 g (pinSelection st)
    | otherwise = cursorVertical (-1) g st

-- | tmux's @window_copy_cmd_scroll_to@: scroll so the cursor's line sits on
-- screen row @to@, keeping the cursor on that line; nothing when the history
-- cannot scroll that far.
scrollCursorTo :: Int -> Grid m -> CopyModeState -> CopyModeState
scrollCursorTo to g st
    | delta > 0, st.viewportOffY >= delta = st { viewportOffY = st.viewportOffY - delta }
    | delta < 0, g.gHsize - st.viewportOffY >= negate delta =
        st { viewportOffY = st.viewportOffY - delta }
    | otherwise = st
  where
    delta = st.cursorRow - viewportTop g st - to

-- | @recentre-top-bottom@: the cursor's line to the middle row, then the
-- top, then the bottom on repeated presses, scrolling as far as the history
-- allows; another line starts over at the middle.
recentreTopBottom :: Grid m -> CopyModeState -> CopyModeState
recentreTopBottom g st = (towards target) { recentre = Just (st.cursorRow, next) }
  where
    now = case st.recentre of
        Just (row, r) | row == st.cursorRow -> r
        _ -> RecentreMiddle
    (target, next) = case now of
        RecentreMiddle -> ((g.gSy - 1) `div` 2, RecentreTop)
        RecentreTop -> (0, RecentreBottom)
        RecentreBottom -> (g.gSy - 1, RecentreMiddle)
    cy = st.cursorRow - viewportTop g st
    towards to
        | cy > to = st { viewportOffY = st.viewportOffY - min (cy - to) st.viewportOffY }
        | cy < to, to - cy <= g.gHsize =
            st { viewportOffY = st.viewportOffY + min (to - cy) (g.gHsize - st.viewportOffY) }
        | otherwise = st

-- | tmux's @window_copy_start_drag@: the cursor to a pane position, with a
-- character selection starting there.
startDragSelection :: Int -> Int -> PaneMode -> PaneMode
startDragSelection px py pm =
    let pm' = cursorToScreen px py pm
    in pm' { copyState = beginSelection pm'.copyState }

-- | tmux's @window_copy_drag_update@: the cursor follows the mouse, the
-- view scrolling a line when it reaches the top or bottom row.
dragSelectionTo :: Int -> Int -> PaneMode -> PaneMode
dragSelectionTo px py pm = cursorToScreen px py pm { copyState = s { viewportOffY = off } }
  where
    s = pm.copyState
    off | py <= 0 = min pm.frozen.fgHsize (s.viewportOffY + 1)
        | py >= pm.frozen.fgSy - 1 = max 0 (s.viewportOffY - 1)
        | otherwise = s.viewportOffY

-- | The copy-mode cursor at a position on the pane's screen.
cursorToScreen :: Int -> Int -> PaneMode -> PaneMode
cursorToScreen px py pm = pm { copyState = s
    { cursorRow = pm.frozen.fgHsize - s.viewportOffY + py, cursorCol = px } }
  where
    s = pm.copyState

beginSelection :: CopyModeState -> CopyModeState
beginSelection s = s
    { selection = Just ((s.cursorRow, s.cursorCol), SelChar), pinnedEnd = Nothing
    , anchorAt = AnchorAtStart }

-- | @select-line@ (vi @V@): start a line-wise selection anchored at the
-- current line; moving up/down then extends it whole lines at a time.
beginLineSelection :: CopyModeState -> CopyModeState
beginLineSelection s = s
    { selection = Just ((s.cursorRow, s.cursorCol), SelLine), pinnedEnd = Nothing
    , anchorAt = AnchorAtStart }

-- | @rectangle-toggle@ (vi @C-v@, emacs @R@): flip the selection between
-- rectangular and non-rectangular, keeping both endpoints. With no
-- selection yet, begin a rectangular one anchored at the cursor.
rectangleToggle :: CopyModeState -> CopyModeState
rectangleToggle s = case s.selection of
    Just (a, SelRect) -> s { selection = Just (a, SelChar) }
    Just (a, _)       -> s { selection = Just (a, SelRect) }
    Nothing           -> (beginSelection s) { selection = Just ((s.cursorRow, s.cursorCol), SelRect) }

-- | @other-end@ (vi @o@): swap the cursor with the selection anchor, so
-- motions extend the opposite end; a stopped selection's far end is picked
-- back up instead. A no-op without a selection.
otherEnd :: CopyModeState -> CopyModeState
otherEnd s = case (s.selection, s.pinnedEnd) of
    (Nothing, _) -> s
    -- tmux then drives the selection's end, wherever the anchor is
    (Just _, Just (r, c))
        | AnchorAtStart <- s.anchorAt ->
            s { cursorRow = r, cursorCol = c, pinnedEnd = Nothing }
    (Just (a, kind), Just p) -> (swapTo a kind p) { pinnedEnd = Nothing }
    (Just (a, kind), Nothing) -> swapTo a kind (s.cursorRow, s.cursorCol)
  where
    swapTo (ar, ac) kind far = s
        { cursorRow = ar, cursorCol = ac
        , selection = Just (far, kind)
        , anchorAt = case s.anchorAt of
            AnchorAtStart -> AnchorAtEnd
            AnchorAtEnd -> AnchorAtStart }

-- | The selection's far end: where it was stopped, else the cursor.
selectionEnd :: CopyModeState -> (Int, Int)
selectionEnd s = fromMaybe (s.cursorRow, s.cursorCol) s.pinnedEnd

clearSelection :: CopyModeState -> CopyModeState
clearSelection s = s { selection = Nothing, pinnedEnd = Nothing }

-- | Leave the selection's far end where it is while the cursor moves on.
pinSelection :: CopyModeState -> CopyModeState
pinSelection s = case s.selection of
    Nothing -> s
    Just _ -> s { pinnedEnd = Just (selectionEnd s) }

-- | @stop-selection@: the cursor no longer extends the selection. A line
-- selection becomes the character span over its lines, as tmux keeps it.
stopSelection :: Monad m => Grid m -> CopyModeState -> m CopyModeState
stopSelection g s = case s.selection of
    Just ((ar, _), SelLine) -> do
        let (er, _) = selectionEnd s
            hi = max ar er
        len <- g.gLineLen hi
        pure s
            { selection = Just ((min ar er, 0), SelChar), pinnedEnd = Just (hi, len)
            , anchorAt = AnchorAtStart }
    _ -> pure (pinSelection s)

-- | Extract the current selection and push it onto the buffer store as
-- a fresh, auto-named buffer at the front of the stack, and onto the
-- clipboard ('setSelection').
yankSelection :: ServerState -> Pane -> CopyModeState -> IO ()
yankSelection st pane s = do
    opts <- paneOpts st pane
    g <- paneGrid pane
    mtext <- extractSelection g opts.modeKeys s
    forM_ mtext $ \body -> do
        setSelection st pane (TE.encodeUtf8 body)
        pushBuffer st body

-- | Push a fresh, auto-named buffer onto the buffer store.
pushBuffer :: ServerState -> Text -> IO ()
pushBuffer st = pushBufferBytes st . TE.encodeUtf8

-- | tmux's @format_grid_word@: the word around (row, col), words split by
-- the separators, spaces and tabs, following wrapped lines.
gridWord :: Monad m => Text -> Grid m -> Int -> Int -> m Text
gridWord seps g row0 col0 = do
    (found, row1, col1) <- back row0 col0
    T.pack . reverse <$> forward found row1 col1 []
  where
    isSep c = inSet seps c || whitespace c
    sepAt r c = do
        pad <- g.gPadding r c
        ch <- g.gChar r c
        pure (not pad && isSep ch)
    back r c = do
        s <- sepAt r c
        if s then pure (True, r, c) else if c == 0
            then if r == 0 then pure (False, r, c) else do
                wrapped <- g.gWrapped (r - 1)
                len <- g.gLineLen (r - 1)
                if not wrapped || len == 0 then pure (False, r, c)
                    else back (r - 1) (len - 1)
            else back r (c - 1)
    forward found r c acc = do
        next <- if not found then pure (Just (r, c)) else do
            end <- g.gLineLen r
            if end == 0 || c == end - 1
                then if r == gBottom g then pure Nothing else do
                    wrapped <- g.gWrapped r
                    pure (if wrapped then Just (r + 1, 0) else Nothing)
                else pure (Just (r, c + 1))
        case next of
            Nothing -> pure acc
            Just (r', c') -> do
                pad <- g.gPadding r' c'
                ch <- g.gChar r' c'
                if pad then forward True r' c' acc
                    else if isSep ch then pure acc
                    else forward True r' c' (ch : acc)

-- | tmux's @format_grid_line@: a row's text up to its trimmed length.
gridLine :: Monad m => Grid m -> Int -> m Text
gridLine g row = do
    len <- g.gLineLen row
    fmap (T.pack . catMaybes) . forM [0 .. len - 1] $ \c -> do
        pad <- g.gPadding row c
        if pad then pure Nothing else Just <$> g.gChar row c
