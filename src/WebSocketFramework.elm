module WebSocketFramework exposing (..)

{-| Expose the most-used functions from `WebSocketFramework.ServerInterface`.
-}

import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( MessageEncoder
        , ServerInterface
        , ServerMessageProcessor
        )


makeProxyServer : ServerMessageProcessor gamestate player message -> (ServerInterface gamestate player message msg -> message -> msg) -> ServerInterface gamestate player message msg
makeProxyServer =
    ServerInterface.makeProxyServer


makeServer : MessageEncoder message -> String -> msg -> ServerInterface gamestate player message msg
makeServer =
    ServerInterface.makeServer


send : ServerInterface gamestate player message msg -> message -> Cmd msg
send =
    ServerInterface.send
