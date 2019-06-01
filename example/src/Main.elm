---------------------------------------------------------------------
--
-- Main.elm
-- Top-level shared example UI for WebSocket client/server framework.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Main exposing (Model, Msg(..), br, encodeDecode, fullProcessor, init, main, send, subscriptions, textAndBr, update, view)

import Browser
import Cmd.Extra exposing (addCmd, addCmds, withCmd, withCmds, withNoCmd)
import Debug exposing (log)
import ExampleInterface
    exposing
        ( GameState
        , Message(..)
        , Player
        , messageDecoder
        , messageEncoder
        , messageProcessor
        , toString
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
        , span
        , table
        , td
        , text
        , tr
        )
import Html.Attributes exposing (disabled, href, size, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as JE exposing (Value)
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..), State)
import WebSocketFramework.EncodeDecode exposing (decodeMessage, encodeMessage)
import WebSocketFramework.ServerInterface as ServerInterface
    exposing
        ( fullMessageProcessor
        , makeProxyServer
        , makeServer
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


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { interface : ServerInterface GameState Player Message Msg
    , urlString : String
    , serverUrl : Maybe String
    , gameid : String
    , playerid : String
    , name : String
    , x : Int
    , y : Int
    , result : String
    , messages : List String
    , portState : State
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


init : () -> ( Model, Cmd msg )
init _ =
    ( { interface = makeProxyServer fullProcessor (IncomingMessage True)
      , urlString = "ws://localhost:8081/"
      , serverUrl = Nothing
      , gameid = ""
      , playerid = ""
      , name = "Bob"
      , x = 1
      , y = 2
      , result = ""
      , messages = []
      , portState = PortFunnels.initialState
      }
    , Cmd.none
    )


type Msg
    = IncomingMessage Bool (ServerInterface GameState Player Message Msg) Message
    | Process Value
    | SetName String
    | SetX String
    | SetY String
    | SetUrl String
    | Add
    | Multiply
    | ToggleConnection
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncomingMessage addToMessages interface message ->
            case message of
                ResultMessage result ->
                    ( { model
                        | result = result
                        , interface = interface
                        , messages =
                            if addToMessages then
                                let
                                    text =
                                        encodeMessage messageEncoder message
                                in
                                ("recv: " ++ text) :: model.messages

                            else
                                model.messages
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        Process value ->
            let
                model2 =
                    { model
                        | messages =
                            ("sock: " ++ JE.encode 0 value) :: model.messages
                    }
            in
            case PortFunnels.processValue funnelDict value model2.portState model2 of
                Err error ->
                    ( { model2 | messages = ("err:  " ++ error) :: model2.messages }
                    , Cmd.none
                    )

                Ok res ->
                    res

        SetName name ->
            ( { model | name = name }
            , Cmd.none
            )

        SetX str ->
            case String.toInt str of
                Just x ->
                    ( { model | x = x }
                    , Cmd.none
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        SetY str ->
            case String.toInt str of
                Just y ->
                    ( { model | y = y }
                    , Cmd.none
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        SetUrl str ->
            ( { model | urlString = str }
            , Cmd.none
            )

        Add ->
            send model (AddMessage model.x model.y)

        Multiply ->
            send model (MultiplyMessage model.x model.y)

        ToggleConnection ->
            let
                ( disconnect, url ) =
                    case model.serverUrl of
                        Just u ->
                            ( True, u )

                        Nothing ->
                            ( False, "" )
            in
            { model
                | serverUrl =
                    if disconnect then
                        Nothing

                    else
                        Just model.urlString
                , interface =
                    if disconnect then
                        makeProxyServer fullProcessor (IncomingMessage True)

                    else
                        makeServer cmdPort messageEncoder model.urlString Noop
            }
                |> withCmd
                    (WebSocket.send cmdPort <|
                        if disconnect then
                            WebSocket.makeClose url

                        else
                            WebSocket.makeOpen model.urlString
                    )

        Noop ->
            ( model
            , Cmd.none
            )


send : Model -> Message -> ( Model, Cmd Msg )
send model message =
    let
        text =
            encodeMessage messageEncoder message

        model2 =
            { model | messages = ("send: " ++ text) :: model.messages }
    in
    ( model2
    , ServerInterface.send model.interface message
    )


br : Html msg
br =
    Html.br [] []


view : Model -> Html Msg
view model =
    let
        connected =
            model.serverUrl /= Nothing
    in
    div [ style "margin-left" "2em" ]
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
        , p []
            [ text "URL: "
            , input
                [ onInput SetUrl
                , value model.urlString
                , disabled connected
                , size 30
                ]
                []
            , text " "
            , button [ onClick ToggleConnection ]
                [ text <|
                    if connected then
                        "Disconnect"

                    else
                        "Connect"
                ]
            ]
        , p []
            (List.map textAndBr model.messages)
        ]


textAndBr : String -> Html Msg
textAndBr string =
    span [] [ text string, br ]


socketHandler : Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            { mdl | portState = state }
    in
    case response of
        WebSocket.MessageReceivedResponse received ->
            case decodeMessage messageDecoder received.message of
                Err msg ->
                    { model
                        | messages =
                            ("err: " ++ received.message) :: model.messages
                    }
                        |> withNoCmd

                Ok message ->
                    update
                        (IncomingMessage False model.interface message)
                        model

        WebSocket.ConnectedResponse { description } ->
            { model
                | messages =
                    ("Connected, " ++ description) :: model.messages
            }
                |> withNoCmd

        WebSocket.ClosedResponse { code, wasClean, expected } ->
            { model
                | serverUrl = Nothing
                , messages =
                    ("Closed, " ++ closedString code wasClean expected)
                        :: model.messages
            }
                |> withNoCmd

        WebSocket.ErrorResponse error ->
            { model
                | messages =
                    WebSocket.errorToString error :: model.messages
            }
                |> withNoCmd

        _ ->
            case WebSocket.reconnectedResponses response of
                [] ->
                    model |> withNoCmd

                [ ReconnectedResponse r ] ->
                    { model
                        | messages =
                            ("Reconnected: " ++ r.description) :: model.messages
                    }
                        |> withNoCmd

                list ->
                    { model
                        | messages =
                            Debug.toString list :: model.messages
                    }
                        |> withNoCmd


closedString : WebSocket.ClosedCode -> Bool -> Bool -> String
closedString code wasClean expected =
    "code: "
        ++ WebSocket.closedCodeToString code
        ++ ", "
        ++ (if wasClean then
                "clean"

            else
                "not clean"
           )
        ++ ", "
        ++ (if expected then
                "expected"

            else
                "NOT expected"
           )



{- This section contains boilerplate that you'll always need.

   First, copy PortFunnels.elm into your project, and modify it
   to support all the funnel modules you use.

   Then update the `handlers` list with an entry for each funnel.

   Those handler functions are the meat of your interaction with each
   funnel module.
-}


handlers : List (Handler Model Msg)
handlers =
    [ WebSocketHandler socketHandler
    ]


subscriptions : Model -> Sub Msg
subscriptions =
    PortFunnels.subscriptions Process


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict handlers getCmdPort


{-| Get the (non-simulated) Cmd port.
-}
getCmdPort : String -> Model -> (Value -> Cmd Msg)
getCmdPort moduleName model =
    PortFunnels.getCmdPort Process moduleName False


{-| The real output port.
-}
cmdPort : Value -> Cmd Msg
cmdPort =
    PortFunnels.getCmdPort Process "" False
