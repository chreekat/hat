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
    , runMotion
    , mNextWord
    , mNextWordEnd
    , mPreviousWord
    , mCursorLeft
    , mCursorRight
    , cursorVertical
    , endOfLine
    , scrollUp
    , scrollDown
    , historyBottom
    , topLine
    , middleLine
    , bottomLine
    , backToIndentation
    , extractSelection
    , yankSelection
    , overlaySelection
    , copyCursorPos
    , scrollToCursor
    ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readCreateProcess, shell)
import qualified Data.Vector as V

import Hat.Geometry (Pos (..), Size (..))
import Hat.Model
import Hat.Model.Options
import qualified Hat.Term.Cell as Cell
import qualified Hat.Term.Emulator as Emu

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
    }

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

-- | @grid_reader_cursor_right@ with (wrap, all, onemore) flags.
cursorRight :: Monad m => Grid m -> Bool -> Bool -> Bool -> Rdr -> m Rdr
cursorRight g wrap allCols onemore s = do
    px <- if allCols
        then pure g.gSx
        else if onemore
            then g.gLineLen s.rr
            else do
                l <- g.gLineLen s.rr
                pure (if l /= 0 then l - 1 else 0)
    if wrap && s.rc >= px && s.rr < gBottom g
        then pure s { rc = 0, rr = s.rr + 1 }
        else if s.rc < px
            then pure s { rc = s.rc + 1 }
            else pure s

-- | @grid_reader_cursor_left@.
cursorLeft :: Monad m => Grid m -> Bool -> Rdr -> m Rdr
cursorLeft g wrap s
    | s.rc == 0 && s.rr > 0 = do
        w <- g.gWrapped (s.rr - 1)
        if wrap || w
            then do
                l <- g.gLineLen (s.rr - 1)
                pure s { rr = s.rr - 1, rc = l }
            else pure s
    | s.rc > 0 = pure s { rc = s.rc - 1 }
    | otherwise = pure s

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

-- | @grid_reader_cursor_previous_word@ with @already@ and @stop_at_eol@.
previousWord :: Monad m => Grid m -> Text -> Bool -> Bool -> Rdr -> m Rdr
previousWord g seps already stopAtEol s0 = do
    c0 <- charAtR g s0
    step1 <- if already || whitespace c0
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
            if stopAtEol && s1.rc > 0
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
        s1 <- if not (whitespace c) then cursorRight g False False False s else pure s
        s2 <- nextWordEnd g seps s1
        cursorLeft g True s2
    KeysEmacs -> nextWordEnd g seps s

mPreviousWord :: Monad m => Text -> Motion m
mPreviousWord seps g keys _ = previousWord g seps True (keys == KeysEmacs)

-- | Move one cell left, staying on the line (or onto the previous line
-- when it wrapped).
mCursorLeft :: Monad m => Motion m
mCursorLeft g _ _ = cursorLeft g False

-- | Move one cell right, allowing one column past the last character
-- (@onemore@) as copy mode does.
mCursorRight :: Monad m => Motion m
mCursorRight g _ _ = cursorRight g False False True

-- | Move the cursor @d@ rows (negative = up), clamped to the grid, and
-- clamp the column to the destination line's length.
cursorVertical :: Monad m => Int -> Grid m -> CopyModeState -> m CopyModeState
cursorVertical d g st = do
    let row' = max 0 (min (gBottom g) (st.cursorRow + d))
    len <- g.gLineLen row'
    pure st { cursorRow = row', cursorCol = min st.cursorCol len }

-- | Move to the end of the current line. Vi lands on the last cell (so an
-- inclusive selection covers it); emacs lands one cell past it (so an
-- exclusive selection covers it).
endOfLine :: Monad m => ModeKeys -> Grid m -> CopyModeState -> m CopyModeState
endOfLine keys g st = do
    len <- g.gLineLen st.cursorRow
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
    len <- g.gLineLen row'
    pure st { viewportOffY = voY', cursorRow = row', cursorCol = min st.cursorCol len }

-- | Scroll the viewport down by @n@ lines (vi @C-f@/@C-d@), the inverse
-- of 'scrollUp'.
scrollDown :: Monad m => Int -> Grid m -> CopyModeState -> m CopyModeState
scrollDown n g st = do
    let voY' = max 0 (min g.gHsize (st.viewportOffY - n))
        moved = st.viewportOffY - voY'
        row' = min (gBottom g) (st.cursorRow + moved)
    len <- g.gLineLen row'
    pure st { viewportOffY = voY', cursorRow = row', cursorCol = min st.cursorCol len }

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

-- ---------------------------------------------------------------------
-- Selection extraction (port of window_copy_get_selection, no rectangle)

-- | Extract the text covered by the current selection. Rows are joined
-- with @\\n@; end-inclusiveness follows 'ModeKeys' (vi keeps the cell
-- under the cursor, emacs drops it).
extractSelection
    :: Monad m => Grid m -> ModeKeys -> CopyModeState -> m (Maybe Text)
extractSelection g keys st = case st.selection of
    Nothing -> pure Nothing
    Just ((ar, ac), _kind) -> do
        let selx = ac; sely = ar
            endx = st.cursorCol; endy = st.cursorRow
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
    Just ((ar, ac), _) ->
        let (sRow, sCol, eRow, eCol)
                | (st.cursorRow, st.cursorCol) `before` (ar, ac) =
                    (st.cursorRow, st.cursorCol, ar, ac)
                | otherwise = (ar, ac, st.cursorRow, st.cursorCol)
            inclusive = case keys of KeysVi -> 1; KeysEmacs -> 0
            lo = if row == sRow then sCol else 0
            hi = min sx (if row == eRow then eCol + inclusive else sx)
        in row >= sRow && row <= eRow && col >= lo && col < hi
  where
    before (a, b) (x, y) = a < x || (a == x && b < y)

-- | Reverse-video the selected cells of an assembled viewport grid.
-- @topRow@ is the absolute grid row shown at visible row 0.
overlaySelection
    :: ModeKeys -> Int -> CopyModeState
    -> V.Vector (V.Vector Cell.Cell) -> V.Vector (V.Vector Cell.Cell)
overlaySelection keys topRow st = V.imap overRow
  where
    overRow i row = V.imap (overCell (topRow + i) (V.length row)) row
    overCell a sx col cell
        | cellSelected keys st sx a col =
            cell { Cell.style =
                (cell.style) { Cell.reverse = not cell.style.reverse } }
        | otherwise = cell

-- | Where the copy cursor sits within a viewport of @sy@ rows whose top
-- line is absolute row @topRow@; 'Nothing' when it is scrolled off.
copyCursorPos :: Int -> Int -> CopyModeState -> Maybe Pos
copyCursorPos topRow sy st =
    let r = st.cursorRow - topRow
    in if r >= 0 && r < sy
        then Just Pos { row = r, col = st.cursorCol }
        else Nothing

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

-- | Build a 'Grid' backed by a pane's emulator.
paneGrid :: Pane -> IO (Grid IO)
paneGrid pane = do
    hsize <- Emu.scrollbackLength pane.emulator
    scr <- Emu.snapshot pane.emulator
    let sz = scr.size
        sy = fromIntegral sz.rows :: Int
        sx = fromIntegral sz.cols :: Int
        rowCells row
            | row < 0 = pure []
            | row < hsize = maybe [] id <$> Emu.scrollbackLine pane.emulator row
            | otherwise = pure $ case scr.cells V.!? (row - hsize) of
                Nothing -> []
                Just v -> V.toList v
    pure Grid
        { gHsize = hsize
        , gSy = sy
        , gSx = sx
        , gChar = \row col -> do
            cs <- rowCells row
            pure $ case drop col cs of
                (c : _) -> case T.uncons c.text of
                    Just (ch, _) -> ch
                    Nothing -> ' '
                [] -> ' '
        , gLineLen = \row -> do
            cs <- rowCells row
            let lastFilled = foldr keep (-1) (zip [0 ..] cs)
                keep (i, c) acc
                    | T.strip c.text == "" = acc
                    | otherwise = max acc i
            pure (min sx (lastFilled + 1))
        , gWrapped = \_ -> pure False
        }

-- | Run a reader motion against a pane grid, updating the cursor.
runMotion
    :: Monad m
    => Grid m -> ModeKeys -> Text -> Motion m -> CopyModeState
    -> m CopyModeState
runMotion g keys seps motion st = do
    s0 <- startRdr g st.cursorRow st.cursorCol
    s1 <- motion g keys seps s0
    pure st { cursorRow = s1.rr, cursorCol = s1.rc }

-- ---------------------------------------------------------------------
-- Handler table

-- | A copy-mode @-X@ handler. Receives the command's positional args
-- (e.g. the shell command for @copy-pipe@). 'Nothing' means \"exit copy
-- mode\".
type Handler
    = ServerState -> Pane -> CopyModeState -> [Text] -> IO (Maybe CopyModeState)

handlers :: Map Text Handler
handlers = Map.fromList
    [ ("cancel",            \_ _ _ _ -> pure Nothing)
    , ("start-of-line",     pureH (\s -> s { cursorCol = 0 }))
    , ("history-top",       pureH (\s -> s { cursorRow = 0, cursorCol = 0 }))
    , ("begin-selection",   pureH beginSelection)
    , ("clear-selection",   pureH (\s -> s { selection = Nothing }))
    , ("next-word",         motionH (\seps -> mNextWord seps))
    , ("next-word-end",     motionH (\seps -> mNextWordEnd seps))
    , ("previous-word",     motionH (\seps -> mPreviousWord seps))
    , ("next-space",        motionH (\_ -> mNextWord ""))
    , ("next-space-end",    motionH (\_ -> mNextWordEnd ""))
    , ("previous-space",    motionH (\_ -> mPreviousWord ""))
    , ("cursor-left",       motionH (\_ -> mCursorLeft))
    , ("cursor-right",      motionH (\_ -> mCursorRight))
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
    , ("end-of-line",       endOfLineH)
    , ("copy-selection",
        \sst p s _ -> yankSelection sst p s
            >> pure (Just s { selection = Nothing }))
    , ("copy-selection-and-cancel",
        \sst p s _ -> yankSelection sst p s >> pure Nothing)
    , ("copy-pipe",            pipeH False)
    , ("copy-pipe-and-cancel", pipeH True)
    ]
  where
    pureH f _ _ s _ = pure (Just (f s))
    motionH mk sst pane st _ = do
        opts <- readTVarIO sst.options
        g <- paneGrid pane
        st' <- runMotion g opts.modeKeys opts.wordSeparators
            (mk opts.wordSeparators) st
        pure (Just st')
    gridH f _ pane st _ = do
        g <- paneGrid pane
        Just <$> f g st
    endOfLineH sst pane st _ = do
        opts <- readTVarIO sst.options
        g <- paneGrid pane
        Just <$> endOfLine opts.modeKeys g st
    -- copy-pipe [-flags] <shell command>: yank the selection into a
    -- buffer AND feed it to @sh -c <command>@ on stdin.
    pipeH cancelAfter sst pane s args = do
        let cmd = T.strip (T.unwords (filter (not . T.isPrefixOf "-") args))
        opts <- readTVarIO sst.options
        g <- paneGrid pane
        mtext <- extractSelection g opts.modeKeys s
        forM_ mtext $ \body -> do
            unless (T.null cmd) . void $
                (try (readCreateProcess (shell (T.unpack cmd)) (T.unpack body))
                    :: IO (Either SomeException String))
            pushBuffer sst body
        pure (if cancelAfter then Nothing else Just s { selection = Nothing })

beginSelection :: CopyModeState -> CopyModeState
beginSelection s = s
    { selection = Just ((s.cursorRow, s.cursorCol), SelChar) }

-- | Extract the current selection and push it onto the buffer store as
-- a fresh, auto-named buffer at the front of the stack.
yankSelection :: ServerState -> Pane -> CopyModeState -> IO ()
yankSelection st pane s = do
    opts <- readTVarIO st.options
    g <- paneGrid pane
    mtext <- extractSelection g opts.modeKeys s
    forM_ mtext (pushBuffer st)

-- | Push a fresh, auto-named buffer onto the front of the buffer store.
pushBuffer :: ServerState -> Text -> IO ()
pushBuffer st body = atomically $ do
    n <- readTVar st.nextBuffer
    writeTVar st.nextBuffer (n + 1)
    let name = "buffer" <> T.pack (show n)
    modifyTVar' st.buffers ((name, body) Seq.<|)
    bumpDirty st
