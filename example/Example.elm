---------------------------------------------------------------------
--
-- Example.elm
-- Top-level example UI for WebSocket client/server framework.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Example exposing (..)

import ExampleServer
    exposing
        ( GameState
        , Message(..)
        , Player
        , messageDecoder
        , messageEncoder
        , messageProcessor
        )
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , h2
        , input
        , p
        , table
        , td
        , text
        , tr
        )
import Html.Attributes exposing (href, style, value)
import Html.Events exposing (onClick, onInput)
import WebSocketFramework.ServerInterface as ServerInterface
    exposing
        ( fullMessageProcessor
        , makeProxyServer
        , send
        )
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , MessageDecoder
        , MessageEncoder
        , ReqRsp(..)
        , ServerInterface(..)
        , ServerMessageProcessor
        )


type alias Model =
    { interface : ServerInterface GameState Player Message Msg
    , gameid : String
    , playerid : String
    , name : String
    , x : Int
    , y : Int
    , result : String
    }


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = messageEncoder
    , decoder = messageDecoder
    , errorWrapper = Nothing
    }


fullProcessor : ServerMessageProcessor GameState Player Message
fullProcessor =
    fullMessageProcessor encodeDecode messageProcessor


init : ( Model, Cmd msg )
init =
    { interface = makeProxyServer fullProcessor IncomingMessage
    , gameid = ""
    , playerid = ""
    , name = "Bob"
    , x = 1
    , y = 2
    , result = ""
    }
        ! []


type Msg
    = IncomingMessage (ServerInterface GameState Player Message Msg) Message
    | SetName String
    | SetX String
    | SetY String
    | Add
    | Multiply


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncomingMessage interface message ->
            case message of
                ResultMessage result ->
                    { model | result = result } ! []

                _ ->
                    model ! []

        SetName name ->
            { model | name = name } ! []

        SetX str ->
            case String.toInt str of
                Ok x ->
                    { model | x = x } ! []

                Err _ ->
                    model ! []

        SetY str ->
            case String.toInt str of
                Ok y ->
                    { model | y = y } ! []

                Err _ ->
                    model ! []

        Add ->
            model ! [ send model.interface (AddMessage model.x model.y) ]

        Multiply ->
            model ! [ send model.interface (MultiplyMessage model.x model.y) ]


br : Html msg
br =
    Html.br [] []


view : Model -> Html Msg
view model =
    div [ style [ ( "margin-left", "2em" ) ] ]
        [ h2 [] [ text "WebSocketFramework Example" ]
        , p []
            [ table []
                [ tr []
                    [ td [] [ text "x:" ]
                    , td []
                        [ input
                            [ onInput SetX
                            , value <| toString model.x
                            ]
                            []
                        ]
                    ]
                , tr []
                    [ td [] [ text "y:" ]
                    , td []
                        [ input
                            [ onInput SetY
                            , value <| toString model.y
                            ]
                            []
                        ]
                    ]
                , tr []
                    [ td [] []
                    , td []
                        [ button [ onClick Add ]
                            [ text "Add" ]
                        , text " "
                        , button [ onClick Multiply ]
                            [ text "Multiply" ]
                        ]
                    ]
                , tr []
                    [ td [] [ text "Result: " ]
                    , td [] [ text model.result ]
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
