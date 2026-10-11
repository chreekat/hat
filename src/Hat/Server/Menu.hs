-- | A window's menu (tmux's @menu.c@): the items @display-menu@ builds, the
-- menu shown over a window and the keys and mouse that drive it, and the
-- private screen it draws into. Pure: "Hat.Server.Command.Menu" opens a
-- menu and runs what is chosen.
module Hat.Server.Menu
    ( -- * Items
      MenuItem (..)
    , Menu (..)
    , ItemSpec (..)
    , menuCreate
    , menuAddItem
      -- * A window's menu
    , WindowMenu (..)
    , StayOpen (..)
    , MenuMouse (..)
    , menuWidth
    , menuHeight
    , placeMenu
    , refitMenu
    , startingChoice
      -- * Input
    , MenuInput (..)
    , MenuOutcome (..)
    , menuKey
      -- * Drawing
    , MenuLook (..)
    , overrideColours
    , menuGrid
    ) where

import Data.Bits ((.&.))
import Data.ByteString qualified as B
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Hat.Model.Ids (PaneId, SessionId, WindowId)
import Hat.Server.Box
import Hat.Server.FormatDraw
    (DrawStyle (..), formatTrimRight, formatWidth, styleParse, themedDrawStyle)
import Hat.Server.Mouse (MouseEvent)
import Hat.Server.Theme (Theme)
import Hat.Term.Cell qualified as Cell

-- | A built item: its name as drawn (a leading @-@ draws it disabled), the
-- command it runs, and its shortcut key.
data MenuItem = MenuItem
    { name :: Text
    , command :: Text
    , key :: Maybe Text  -- ^ a canonical key name
    }
    deriving (Eq, Show)

-- | tmux's @struct menu@: items in order, 'Nothing' for a separator line.
data Menu = Menu
    { title :: Text
    , items :: [Maybe MenuItem]
    , width :: Int  -- ^ the widest item or the title, in cells
    }
    deriving (Eq, Show)

-- | An item as @display-menu@ gives it, its name and command already
-- format-expanded.
data ItemSpec = ItemSpec
    { name :: Text
    , key :: Maybe Text
    , command :: Text
    }
    deriving (Eq, Show)

-- | tmux's @menu_create@.
menuCreate :: Text -> Menu
menuCreate title = Menu { title, items = [], width = formatWidth title }

-- | tmux's @menu_add_item@ for a client @clientCols@ wide: a separator
-- ('Nothing') never leads nor doubles; an item expanding to nothing is
-- dropped; a long name keeps its right end (marked @>@) and loses its key
-- when the key would not fit.
menuAddItem :: Int -> Maybe ItemSpec -> Menu -> Menu
menuAddItem clientCols mspec menu = case mspec of
    Nothing
        | null menu.items -> menu
        | Just Nothing <- lastMay menu.items -> menu
        | otherwise -> menu { items = menu.items <> [Nothing] }
    Just spec
        | T.null spec.name -> menu
        | otherwise ->
            let item = MenuItem { name = drawn spec, command = spec.command, key = spec.key }
                w = formatWidth item.name - (if "-" `T.isPrefixOf` item.name then 1 else 0)
            in menu { items = menu.items <> [Just item], width = max menu.width w }
  where
    maxWidth0 = if clientCols >= 4 then clientCols - 4 else maxBound
    drawn spec =
        let s = spec.name
            slen = B.length (TE.encodeUtf8 s)
            withKey = case spec.key of
                Just k | not ("-" `T.isPrefixOf` s) ->
                    let keylen = T.length k + 3
                    in if keylen <= maxWidth0 `div` 4 then Just (k, maxWidth0 - keylen)
                       else if keylen >= maxWidth0 || slen >= maxWidth0 - keylen
                           then Nothing
                           else Just (k, maxWidth0)
                _ -> Nothing
            maxWidth1 = maybe maxWidth0 snd withKey
            (maxWidth, suffix) =
                if slen > maxWidth1 then (maxWidth1 - 1, ">") else (maxWidth1, "")
            trimmed = formatTrimRight maxWidth s
        in case withKey of
            Just (k, _) -> trimmed <> suffix <> "#[default] #[align=right](" <> k <> ")"
            Nothing -> trimmed <> suffix
    lastMay xs = if null xs then Nothing else Just (last xs)

-- | Whether choosing or clicking away keeps the menu open (@-O@).
data StayOpen = CloseOnChoice | StayOpen
    deriving (Eq, Show)

-- | Whether the mouse drives the menu: with no mouse event behind it and no
-- @-M@, it takes keys only (tmux's @MENU_NOMOUSE@).
data MenuMouse = MenuMouseOn | MenuNoMouse
    deriving (Eq, Show)

-- | tmux's @struct menu_data@: the menu shown over a window.
data WindowMenu = WindowMenu
    { menu :: Menu
    , choice :: Int  -- ^ the highlighted item, or -1
    , px :: Int  -- ^ the box's top-left, in window cells
    , py :: Int
    , stayOpen :: StayOpen
    , mouse :: MenuMouse
    , lines :: BoxLines
    , style :: Maybe Text  -- ^ @-s@, over @menu-style@'s colours
    , selectedStyle :: Maybe Text  -- ^ @-H@
    , borderStyle :: Maybe Text  -- ^ @-S@
    , target :: (SessionId, WindowId, PaneId)
        -- ^ what the chosen command acts on by default (tmux's @md->fs@)
    , event :: Maybe MouseEvent
        -- ^ the mouse event that opened it, which the chosen command sees
    }
    deriving (Eq, Show)

-- | The box's size: tmux's @menu_width@ and @menu_height@.
menuWidth, menuHeight :: Menu -> Int
menuWidth m = m.width + 4
menuHeight m = length m.items + 2

-- | tmux's @menu_display@ placement in a window @wsx@ by @wsy@: a box too
-- big for the window starts at its edge, one that would run off the far
-- edge is pulled back inside.
placeMenu :: Int -> Int -> Menu -> (Int, Int) -> (Int, Int)
placeMenu wsx wsy m (x, y) = (fit wsx (menuWidth m) x, fit wsy (menuHeight m) y)
  where
    fit limit size p
        | size >= limit = 0
        | p + size > limit = limit - size
        | otherwise = p

-- | tmux's @menu_resize@: the menu pulled back inside a window resized to
-- @wsx@ by @wsy@.
refitMenu :: Int -> Int -> WindowMenu -> WindowMenu
refitMenu wsx wsy wm = wm { px = fit wsx (menuWidth wm.menu) wm.px, py = fit wsy (menuHeight wm.menu) wm.py }
  where
    fit limit size p
        | p + size > limit = if limit <= size then 0 else limit - size
        | otherwise = p

-- | The item a keyboard menu (@-C@) opens on (tmux's @menu_display@): the
-- first enabled item from the one asked for, searching down and wrapping;
-- from past the end, the last enabled item searching up. -1 for none.
startingChoice :: MenuMouse -> Int -> Menu -> Int
startingChoice MenuMouseOn _ _ = -1
startingChoice MenuNoMouse start m
    | start >= n = search (n - 1) (\c -> if c == 0 then n - 1 else c - 1)
    | start >= 0 = search start (\c -> if c + 1 == n then 0 else c + 1)
    | otherwise = -1
  where
    n = length m.items
    enabled c = case m.items List.!! c of
        Just it -> not ("-" `T.isPrefixOf` it.name)
        Nothing -> False
    search from next = go from
      where
        go c
            | enabled c = c
            | next c == from = -1
            | otherwise = go (next c)

-- | What reaches a menu: a key (by canonical name) or a mouse report, its
-- button byte and where it is in the window ('Nothing' over a status line).
data MenuInput
    = MenuKeyPress Text
    | MenuMouseAt Int (Maybe (Int, Int))
    deriving (Eq, Show)

-- | What a key or report does to a menu.
data MenuOutcome
    = MenuStays WindowMenu
    | MenuCloses
    | MenuChose MenuItem  -- ^ the menu closes and the item's command runs
    deriving (Eq, Show)

-- | tmux's @menu_key@.
menuKey :: MenuInput -> WindowMenu -> MenuOutcome
menuKey input wm = case input of
    MenuMouseAt b at
        | wm.mouse == MenuNoMouse ->
            if b .&. mouseButtons /= 0 then MenuCloses else MenuStays wm
        | otherwise -> mouseKey b at
    MenuKeyPress k -> case List.findIndex (shortcut k) items of
        Just i -> chosen wm { choice = i }
        Nothing -> namedKey k
  where
    items = wm.menu.items
    n = length items
    old = wm.choice
    skipped c = case items List.!! c of
        Just it -> "-" `T.isPrefixOf` it.name
        Nothing -> True
    shortcut k = \case
        Just it -> not ("-" `T.isPrefixOf` it.name) && it.key == Just k
        Nothing -> False
    stay c = MenuStays wm { choice = c }

    mouseKey b at =
        let drag = b .&. 32 /= 0
            release = b .&. mouseButtons == 3
            wheel = b .&. mouseButtons `elem` [64, 65]
            move = drag && release
            inside = case at of
                Just (x, y) -> x >= wm.px && x <= wm.px + 4 + wm.menu.width
                    && y >= wm.py + 1 && y <= wm.py + n
                Nothing -> False
        in if not inside
            then case wm.stayOpen of
                CloseOnChoice | not move && release -> MenuCloses
                StayOpen | not release && not wheel && not drag -> MenuCloses
                _ -> stay (-1)
            else case wm.stayOpen of
                CloseOnChoice | not move && release -> chosen wm
                StayOpen | not wheel && not drag -> chosen wm
                _ -> stay (maybe old (\(_, y) -> y - (wm.py + 1)) at)

    namedKey k
        | k `elem` ["BTab", "Up", "k"] = stay (stepWhile up (if old == -1 then 0 else old) old)
        | k `elem` ["Down", "j"] = stay (stepWhile down (if old == -1 then 0 else old) old)
        | k `elem` ["PgUp", "C-b"] = stay pageUp
        | k == "PgDn" = stay pageDown
        | k `elem` ["g", "Home"] = stay (firstFrom 0 (+ 1) (n - 1))
        | k `elem` ["G", "End"] = stay (firstFrom (n - 1) (subtract 1) 0)
        | k == "Enter" = chosen wm
        | k `elem` ["Escape", "C-c", "C-g", "q"] = MenuCloses
        | otherwise = MenuStays wm
    up c = if c == -1 || c == 0 then n - 1 else c - 1
    down c = if c == -1 || c == n - 1 then 0 else c + 1
    -- Step once, then on past separators and disabled items, stopping
    -- should the steps come back round to @stop@.
    stepWhile step stop c0 =
        let go c = let c' = step c in if skipped c' && c' /= stop then go c' else c'
        in go c0
    firstFrom c next end
        | skipped c && c /= end = firstFrom (next c) next end
        | otherwise = c
    pageUp
        | old < 6 = 0
        | otherwise = go old (5 :: Int)
      where
        go c i
            | i <= 0 = c
            | otherwise =
                let c' = c - 1
                in if c' /= 0 && not (skipped c') then go c' (i - 1)
                   else if c' == 0 then c' else go c' i
    pageDown =
        let c1 | old > n - 6 = n - 1
               | otherwise = go old (5 :: Int)
            go c i
                | i <= 0 = c
                | otherwise =
                    let c' = c + 1
                    in if c' /= n - 1 && not (skipped c') then go c' (i - 1)
                       else if c' == n - 1 then c' else go c' i
            back c = if skipped c && c /= 0 then back (c - 1) else c
        in back c1

    chosen w
        | w.choice == -1 = MenuCloses
        | otherwise = case items List.!! w.choice of
            Just it | not ("-" `T.isPrefixOf` it.name) -> MenuChose it
            _ -> if w.stayOpen == StayOpen then MenuStays w else MenuCloses

mouseButtons :: Int
mouseButtons = 195

-- | The styles a menu draws in, as its options and flags resolve them.
data MenuLook = MenuLook
    { style :: Cell.Style
    , selected :: Cell.Style
    , border :: Cell.Style
    }
    deriving (Eq, Show)

-- | A style flag's colours over an option's style (as @menu_reapply_styles@
-- and @popup_reapply_styles@ apply @-s@ and its kin): only the foreground
-- and background, and only when the flag parses.
overrideColours :: Theme -> Cell.Style -> Maybe Text -> Cell.Style
overrideColours th base = \case
    Nothing -> base
    Just t -> case styleParse base (themedDrawStyle th) t of
        Just ds -> base { Cell.fg = ds.cell.fg, Cell.bg = ds.cell.bg }
        Nothing -> base

-- | tmux's @menu_update@: the menu's box, title and items on its own
-- screen.
menuGrid :: Theme -> MenuLook -> WindowMenu -> Grid
menuGrid th look wm = List.foldl' item boxed (zip [0 ..] m.items)
  where
    m = wm.menu
    w = menuWidth m
    boxed = drawBox th wm.lines look.border m.title w (menuHeight m) (blankGrid w (menuHeight m))
    item g (i, mit) = case mit of
        Nothing -> drawHLine wm.lines look.border w (i + 1) g
        Just it ->
            let disabled = "-" `T.isPrefixOf` it.name
                gc = if i == wm.choice && not disabled then look.selected else look.style
                padded = List.foldl' (\acc x -> setCell x (i + 1) (Cell.glyphCell ' ' gc) acc) g
                    [1 .. m.width + 2]
                text = if disabled then T.drop 1 it.name else it.name
                base = if disabled then gc { Cell.faint = True } else gc
            in drawFormatAt th base text 2 (i + 1) m.width padded
