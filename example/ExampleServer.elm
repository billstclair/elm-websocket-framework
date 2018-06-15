port module ExampleServer exposing (..)

import ExampleInterface
    exposing
        ( GameState
        , Message
        , Player
        , messageDecoder
        , messageEncoder
        , messageProcessor
        )
import Platform exposing (Program)
import WebSocketFramework.Server
    exposing
        ( Msg
        , ServerMessageSender
        , UserFunctions
        , WrappedModel
        , program
        , sendToOne
        )
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , InputPort
        , OutputPort
        , ServerState
        )


type alias ServerModel =
    ()


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = messageEncoder
    , decoder = messageDecoder
    , errorWrapper = Nothing
    }


messageSender : ServerMessageSender (Maybe ServerModel) Message GameState Player
messageSender model socket state request response =
    ( model, sendToOne messageEncoder response outputPort socket )


userFunctions : UserFunctions (Maybe ServerModel) Message GameState Player
userFunctions =
    { encodeDecode = encodeDecode
    , messageProcessor = messageProcessor
    , messageSender = messageSender
    , messageToGameid = Nothing
    , inputPort = inputPort
    , outputPort = outputPort
    }


main =
    program Nothing userFunctions Nothing



-- PORTS


port inputPort : InputPort msg


port outputPort : OutputPort msg
