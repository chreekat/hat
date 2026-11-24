-- | The command line of @hat attach --rex@.
module Hat.Client.RexArgs
    ( RexTarget (..)
    , rexAttachTarget
    ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text.Encoding qualified as TE

-- | What to attach to on a Rex server.
data RexTarget = RexTarget
    { socket :: ByteString     -- ^ the server's Unix socket path, as argv gave it
    , session :: Maybe Text    -- ^ a session's label or id; 'Nothing' for the first listed
    }
    deriving (Eq, Show)

-- | Read command words as an attach to a Rex server: 'Nothing' when they
-- are not one (no @--rex@ on an @attach@), and a 'Left' saying why for one
-- hat cannot carry out.
rexAttachTarget :: [ByteString] -> Maybe (Either Text RexTarget)
rexAttachTarget = \case
    verb : args | verb `elem` ["attach", "attach-session"], "--rex" `elem` args ->
        Just (go Nothing Nothing args)
    _ -> Nothing
  where
    go mendpoint msession = \case
        [] -> case mendpoint of
            Nothing -> Left needsEndpoint
            Just socket -> Right RexTarget { socket, session = msession }
        "--rex" : ep : rest | not ("-" `B.isPrefixOf` ep) -> case mendpoint of
            Nothing -> endpoint ep >>= \s -> go (Just s) msession rest
            Just _ -> Left "--rex given twice"
        "--rex" : _ -> Left needsEndpoint
        "-t" : name : rest -> go mendpoint (Just (TE.decodeUtf8Lenient name)) rest
        ["-t"] -> Left "-t needs a session"
        arg : rest
            | Just name <- B.stripPrefix "-t" arg, not (B.null name) ->
                go mendpoint (Just (TE.decodeUtf8Lenient name)) rest
            | otherwise ->
                Left ("attach --rex does not take " <> TE.decodeUtf8Lenient arg)
    needsEndpoint = "--rex needs an endpoint: the path of a Rex server's Unix socket"
    endpoint ep
        | Just path <- B.stripPrefix "unix://" ep = unixPath path
        | "://" `B.isInfixOf` ep =
            Left ("only Unix socket endpoints are supported, not "
                <> TE.decodeUtf8Lenient ep)
        | otherwise = unixPath ep
    unixPath path
        | B.null path = Left needsEndpoint
        | otherwise = Right path
