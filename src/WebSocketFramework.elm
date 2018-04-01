---------------------------------------------------------------------
--
-- WebSocketFramework.elm
-- Most-used functions for WebSocket client/server framework.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module WebSocketFramework
    exposing
        ( decodePlist
        , makeProxyServer
        , makeServer
        , send
        , unknownMessage
        )

{-| Expose the most-used functions from `WebSocketFramework.ServerInterface`.
-}

import Json.Decode as JD exposing (Decoder)
import WebSocketFramework.EncodeDecode as EncodeDecode
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( MessageEncoder
        , Plist
        , ReqRsp(..)
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


decodePlist : Decoder message -> Plist -> Result String message
decodePlist =
    EncodeDecode.decodePlist


unknownMessage : ReqRsp -> Result String message
unknownMessage reqrsp =
    let
        ( typ, message ) =
            case reqrsp of
                Req m ->
                    ( "request", m )

                Rsp m ->
                    ( "response", m )

        msg =
            "Unknown " ++ typ ++ " message: '" ++ message ++ "'"
    in
    Err msg
