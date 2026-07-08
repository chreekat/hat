-- | Routing of raw client bytes: everything flows to the active pane
-- except the prefix key, which arms a one-key command state. What the
-- prefixed key *does* is the binding table's business, not ours.
module Hat.Server.Input
    ( KeyState (..)
    , InputAction (..)
    , routeInput
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)

data KeyState
    = Normal
    | AwaitingPrefixed
    deriving (Eq, Show)

data InputAction
    = ToPane ByteString
    | Prefixed Word8
    deriving (Eq, Show)

routeInput :: Word8 -> KeyState -> ByteString -> (KeyState, [InputAction])
routeInput prefix = go
  where
    go st bs | B.null bs = (st, [])
    go Normal bs =
        let (plain, rest) = B.break (== prefix) bs
            plainActs = [ToPane plain | not (B.null plain)]
        in if B.null rest
            then (Normal, plainActs)
            else
                let (st', acts) = go AwaitingPrefixed (B.drop 1 rest)
                in (st', plainActs <> acts)
    go AwaitingPrefixed bs =
        let (st', acts) = go Normal (B.drop 1 bs)
        in (st', Prefixed (B.head bs) : acts)
