---------------------------------------------------------------------
--
-- EncodeDecode.elm
-- JSON encoder and decoder for Archmage server wire protocol.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module WebSocketFramework.EncodeDecode
    exposing
        ( decodeMessage
        , decodePlist
        , decodeRawMessage
        , decodeServerInterface
        , encodeMessage
        , encodeServerInterface
        , messageDecoder
        , messageEncoder
        , rawMessageDecoder
        , rawMessageEncoder
        , serverInterfaceDecoder
        , serverInterfaceEncoder
        )

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import WebSocketFramework.Types as Types
    exposing
        ( MessageDecoder
        , MessageEncoder
        , Plist
        , RawMessage
        , ReqRsp(..)
        , ServerInterface(..)
        )


---
--- ServerInterface
---


makeServerInterface : msg -> String -> ServerInterface gamestate player message msg
makeServerInterface noop server =
    ServerInterface
        { server = server
        , wrapper = \_ _ -> noop
        , state = Nothing
        , sender = \_ _ -> Cmd.none
        }


decodeServerInterface : msg -> String -> Result String (ServerInterface gamestate player message msg)
decodeServerInterface msg json =
    JD.decodeString (serverInterfaceDecoder msg) json


serverInterfaceDecoder : msg -> Decoder (ServerInterface gamestate player message msg)
serverInterfaceDecoder msg =
    JD.map (makeServerInterface msg)
        JD.string


encodeServerInterface : ServerInterface gamestate player message msg -> String
encodeServerInterface interface =
    JE.encode 0 <| serverInterfaceEncoder interface


serverInterfaceEncoder : ServerInterface gamestate player message msg -> Value
serverInterfaceEncoder (ServerInterface interface) =
    JE.string interface.server



-- Message


decodeRawMessage : String -> Result String RawMessage
decodeRawMessage string =
    JD.decodeString rawMessageDecoder string


rawMessageDecoder : Decoder RawMessage
rawMessageDecoder =
    JD.map3 RawMessage
        (JD.index 0 JD.string)
        (JD.index 1 JD.string)
        (JD.index 2 (JD.keyValuePairs JD.value))


decodeReqRsp : String -> String -> Result String ReqRsp
decodeReqRsp reqrsp message =
    case reqrsp of
        "req" ->
            Ok <| Req message

        "rsp" ->
            Ok <| Rsp message

        _ ->
            Err <| "Expecting 'req' or 'rsp', got: '" ++ reqrsp ++ "'"


decodeMessage : MessageDecoder message -> String -> Result String message
decodeMessage decoder json =
    JD.decodeString (messageDecoder decoder) json


messageDecoder : MessageDecoder message -> Decoder message
messageDecoder decoder =
    rawMessageDecoder
        |> JD.andThen
            (\m ->
                case decodeReqRsp m.reqrsp m.msg of
                    Ok reqrsp ->
                        case decoder ( reqrsp, m.plist ) of
                            Ok message ->
                                JD.succeed message

                            Err msg ->
                                JD.fail msg

                    Err msg ->
                        JD.fail msg
            )


encodeMessage : MessageEncoder message -> message -> String
encodeMessage encoder message =
    JE.encode 0 <| messageEncoder encoder message


messageEncoder : MessageEncoder message -> message -> Value
messageEncoder encoder message =
    let
        ( reqrsp, plist ) =
            encoder message

        ( rr, msg ) =
            case reqrsp of
                Req m ->
                    ( "req", m )

                Rsp m ->
                    ( "rsp", m )
    in
    rawMessageEncoder
        { reqrsp = rr
        , msg = msg
        , plist = plist
        }


rawMessageEncoder : RawMessage -> Value
rawMessageEncoder message =
    JE.list
        [ JE.string message.reqrsp
        , JE.string message.msg
        , JE.object message.plist
        ]


decodePlist : Decoder message -> Plist -> Result String message
decodePlist decoder plist =
    JD.decodeValue decoder <| JE.object plist
