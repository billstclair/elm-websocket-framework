----------------------------------------------------------------------
--
-- ServerInterface.elm
-- WebSocketFramework server implementation.
-- Usable locally on the client, or via WebSocket on a server.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module WebSocketFramework.ServerInterface
    exposing
        ( appendGameList
        , dummyGameid
        , emptyServerState
        , errorRsp
        , getServer
        , makeProxyServer
        , makeServer
        , removeGameFromList
        , send
        )

import Debug exposing (log)
import Dict exposing (Dict)
import List.Extra as LE
import Task
import WebSocket
import WebSocketFramework.EncodeDecode exposing (encodeMessage)
import WebSocketFramework.Types as Types
    exposing
        ( ErrorRsp
        , MessageEncoder
        , ModeChecker
        , PlayerInfo
        , PublicGame
        , PublicGames
        , RawMessage
        , ServerInterface(..)
        , ServerMessageProcessor
        , ServerState
        , emptyPublicGames
        )


emptyServerState : ServerState gamestate player
emptyServerState =
    { gameDict = Dict.empty
    , playerDict = Dict.empty
    , publicGames = emptyPublicGames
    , state = Nothing
    }


dummyGameid : String
dummyGameid =
    "<gameid>"


makeProxyServer : ServerMessageProcessor gamestate player message -> (ServerInterface gamestate player message msg -> message -> msg) -> ServerInterface gamestate player message msg
makeProxyServer messageProcessor wrapper =
    ServerInterface
        { server = ""
        , wrapper = wrapper
        , state = Nothing
        , sender = proxySender messageProcessor
        }


makeServer : MessageEncoder message -> String -> msg -> ServerInterface gamestate player message msg
makeServer encoder server msg =
    ServerInterface
        { server = server
        , wrapper = \_ _ -> msg
        , state = Nothing
        , sender = sender encoder
        }


getServer : ServerInterface gamestate player message msg -> String
getServer (ServerInterface interface) =
    interface.server


send : ServerInterface gamestate player message msg -> message -> Cmd msg
send ((ServerInterface interface) as si) message =
    interface.sender si message


proxyCmd : ServerInterface gamestate player message msg -> message -> Cmd msg
proxyCmd ((ServerInterface interface) as si) message =
    let
        task =
            Task.succeed message

        wrapper =
            interface.wrapper si
    in
    Task.perform wrapper task


proxySender : ServerMessageProcessor gamestate player message -> ServerInterface gamestate player message msg -> message -> Cmd msg
proxySender processor (ServerInterface interface) message =
    let
        state =
            Maybe.withDefault emptyServerState interface.state

        ( s2, return ) =
            processor state message
    in
    case return of
        Nothing ->
            Cmd.none

        Just m ->
            proxyCmd (ServerInterface { interface | state = Just s2 }) m


sender : MessageEncoder message -> ServerInterface gamestate player message msg -> message -> Cmd msg
sender encoder (ServerInterface interface) message =
    WebSocket.send interface.server (encodeMessage encoder message)


errorRsp : message -> String -> ErrorRsp message
errorRsp message text =
    { request = message
    , text = text
    }


checkOnlyGameid : ServerState gamestate player -> message -> String -> Result (ErrorRsp message) gamestate
checkOnlyGameid state message gameid =
    case Dict.get gameid state.gameDict of
        Just gameState ->
            Ok gameState

        Nothing ->
            Err <| errorRsp message "Unknown gameid"


checkPlayerid : ServerState gamestate player -> message -> String -> Result (ErrorRsp message) (PlayerInfo player)
checkPlayerid state message playerid =
    case Dict.get playerid state.playerDict of
        Nothing ->
            Err <| errorRsp message ("Unknown playerid " ++ playerid)

        Just info ->
            Ok info


checkGameid : ModeChecker gamestate -> ServerState gamestate player -> message -> String -> Result (ErrorRsp message) gamestate
checkGameid checker state message gameid =
    case checkOnlyGameid state message gameid of
        Ok gamestate ->
            case checker gamestate of
                Ok _ ->
                    Ok gamestate

                Err msg ->
                    Err (errorRsp message msg)

        err ->
            err


appendGameList : PublicGame -> PublicGames -> PublicGames
appendGameList game games =
    List.append games [ game ]


removeGameFromList : String -> PublicGames -> PublicGames
removeGameFromList gameid games =
    List.filter (\game -> game.gameid /= gameid) games
