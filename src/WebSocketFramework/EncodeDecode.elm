---------------------------------------------------------------------
--
-- EncodeDecode.elm
-- JSON encoder and decoder for WebSocketFramework
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module WebSocketFramework.EncodeDecode exposing
    ( decodeMessage, genericMessageDecoder, encodeMessage, messageDecoder, messageEncoder
    , encodeStatistics, statisticsDecoder
    , decodeRawMessage, rawMessageDecoder, rawMessageEncoder, decodePlist
    )

{-| The `EncodeDecode` module handles translation of strings sent over the wire to and from your message types.


# High-level functions

Your WebSocket subscription message handler will need to use `decodeMessage`, you will often use `genericMessageDecoder`, and, if you track statistics, you'll likely need the statistics encoder & decoder, but you'll rarely directly use the rest of the functions in this module.

@docs decodeMessage, genericMessageDecoder, encodeMessage, messageDecoder, messageEncoder
@docs encodeStatistics, statisticsDecoder


# Low-level functions

@docs decodeRawMessage, rawMessageDecoder, rawMessageEncoder, decodePlist

-}

import Dict
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import WebSocketFramework.Types as Types
    exposing
        ( DecoderPlist
        , MessageDecoder
        , MessageEncoder
        , Plist
        , RawMessage
        , ReqRsp(..)
        , ServerInterface(..)
        , Statistics
        )



-- Message


convertErr : Result JD.Error a -> Result String a
convertErr result =
    case result of
        Ok a ->
            Ok a

        Err err ->
            Err <| JD.errorToString err


decodeString : Decoder a -> String -> Result String a
decodeString decoder string =
    JD.decodeString decoder string
        |> convertErr


decodeValue : Decoder a -> Value -> Result String a
decodeValue decoder value =
    JD.decodeValue decoder value
        |> convertErr


{-| Turn a string into a raw message.
-}
decodeRawMessage : String -> Result String RawMessage
decodeRawMessage string =
    decodeString rawMessageDecoder string


{-| The `Decoder` used by `decodeRawMessage`
-}
rawMessageDecoder : Decoder RawMessage
rawMessageDecoder =
    JD.map3 RawMessage
        (JD.index 0 JD.string)
        (JD.index 1 JD.string)
        (JD.index 2 (JD.keyValuePairs JD.value))


{-| Decode "req" or "rsp" and a `message` name into either `Req message` or `Rsp message`.
-}
decodeReqRsp : String -> String -> Result String ReqRsp
decodeReqRsp reqrsp message =
    case reqrsp of
        "req" ->
            Ok <| Req message

        "rsp" ->
            Ok <| Rsp message

        _ ->
            Err <| "Expecting 'req' or 'rsp', got: '" ++ reqrsp ++ "'"


{-| Use a `MessageDecoder` that you write to turn a string into a message.
-}
decodeMessage : MessageDecoder message -> String -> Result String message
decodeMessage decoder json =
    decodeString (messageDecoder decoder) json


{-| Create a `Decoder` for `decodeMessage`.
-}
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


{-| Use a `MessageEncoder` that you write to turn a message into a string.
-}
encodeMessage : MessageEncoder message -> message -> String
encodeMessage encoder message =
    JE.encode 0 <| messageEncoder encoder message


{-| Use a `MessageEncoder` that you write to turn a message into a `Value`.

Rarely used by anything but `encodeMessage`.

-}
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


{-| Turn a raw message into a `Value`.
-}
rawMessageEncoder : RawMessage -> Value
rawMessageEncoder message =
    JE.list identity
        [ JE.string message.reqrsp
        , JE.string message.msg
        , JE.object message.plist
        ]


{-| Decode a list of key/value pairs into a message.
-}
decodePlist : Decoder message -> Plist -> Result String message
decodePlist decoder plist =
    decodeValue decoder <| JE.object plist


{-| Simplify the writing of `MessageDecoder` definitions.

Takes two plists mapping request names to decoders and response names to decoders.

Returns a `MessageDecoder` that uses them.

-}
genericMessageDecoder : DecoderPlist msg -> DecoderPlist msg -> MessageDecoder msg
genericMessageDecoder reqPlist rspPlist ( reqrsp, plist ) =
    case reqrsp of
        Req msg ->
            decoderPlistDecoder reqPlist "request" msg plist

        Rsp msg ->
            decoderPlistDecoder rspPlist "response" msg plist


decoderPlistDecoder : DecoderPlist msg -> String -> String -> Plist -> Result String msg
decoderPlistDecoder decoderPlist typ msg plist =
    let
        dict =
            Dict.fromList decoderPlist
    in
    case Dict.get msg dict of
        Nothing ->
            Err <| "Unknown " ++ typ ++ "message: '" ++ msg ++ "'"

        Just decoder ->
            decodePlist decoder plist


{-| Encode a `Statistics` record.
-}
encodeStatistics : Statistics -> Value
encodeStatistics statistics =
    JE.dict identity JE.int statistics


{-| Decoder for the `Statistics` record.
-}
statisticsDecoder : Decoder Statistics
statisticsDecoder =
    JD.dict JD.int
