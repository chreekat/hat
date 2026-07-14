-- | The desktop title bar: least specific on the left, most specific on
-- the right, collapsing from the left when space is tight so the
-- specific parts stay visible.
module Hat.Server.Title
    ( TitleParts (..)
    , composeTitle
    ) where

import Data.Char (isDigit)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T

data TitleParts = TitleParts
    { session :: Text
    , window  :: Text
    , path    :: Text  -- ^ the active pane's cwd, absolute
    , home    :: Text  -- ^ for @~@ abbreviation; empty disables it
    , program :: Text
    }
    deriving (Eq, Show)

-- | @hat: \<session\>: \<window\>: \<path\>: \<program\>@, within a
-- character budget. Session and window names that are just numbers (the
-- defaults) say nothing and are skipped. Over budget, components
-- collapse from the left — @hat@, then session, then window, then the
-- path shortens from the left (@…\/dir@) and finally drops — down to the
-- bare program, which is never mutilated.
composeTitle :: Int -> TitleParts -> Text
composeTitle budget p =
    case filter ((<= budget) . T.length) (map join variants) of
        (t : _) -> t
        [] -> p.program
  where
    join = T.intercalate ": "
    named t | T.null t || T.all isDigit t = []
            | otherwise = [t]
    sess = named p.session
    win  = named p.window
    mpath = [abbrev | not (T.null abbrev)]
    abbrev
        | T.null p.path = ""
        | not (T.null p.home) && p.path == p.home = "~"
        | not (T.null p.home)
        , Just rest <- T.stripPrefix (p.home <> "/") p.path = "~/" <> rest
        | otherwise = p.path
    -- Left-shortenings of the path: ~/a/b/c, …/a/b/c is never shorter,
    -- so the useful steps are …/b/c, …/c.
    shortenings = case mpath of
        [] -> []
        (t : _) ->
            [ "…/" <> T.intercalate "/" segs
            | segs <- drop 1 (List.tails (T.splitOn "/" t))
            , not (null segs) ]
    variants =
        [ ["hat"] <> sess <> win <> mpath <> [p.program]
        , sess <> win <> mpath <> [p.program]
        , win <> mpath <> [p.program]
        , mpath <> [p.program]
        ]
        <> [ [s, p.program] | s <- shortenings ]
        <> [ [p.program] ]
