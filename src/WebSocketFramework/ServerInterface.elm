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
        ( addGame
        , addPlayer
        , appendPublicGames
        , checkGameid
        , checkOnlyGameid
        , checkPlayerid
        , dummyGameid
        , errorRsp
        , fullMessageProcessor
        , gameCount
        , getGame
        , getGamePlayers
        , getPlayer
        , getServer
        , isPrivateGame
        , isPublicGame
        , makeProxyServer
        , makeServer
        , newGameid
        , newPlayerid
        , removeGame
        , removePlayer
        , removePublicGame
        , send
        , updateGame
        , updatePlayer
        )

{-| Functions that connect the client code to the server.


# Server Constructors

@docs makeServer, makeProxyServer, fullMessageProcessor


# Sending a Message

@docs send


# State Access

@docs getServer


# GameId and PlayerId validity checking

@docs checkOnlyGameid, checkGameid, checkPlayerid, dummyGameid


# Game and Player addition and removal

@docs addGame, addPlayer, removeGame, removePlayer


# Public Games

@docs appendPublicGames, removePublicGame


# Create random game and player identifiers.

@docs newGameid, newPlayerid


# Utilities

@docs getGame, updateGame, gameCount, getPlayer, updatePlayer, getGamePlayers
@docs isPublicGame, isPrivateGame


# Errors

@docs errorRsp

-}

import Char
import Debug exposing (log)
import Dict exposing (Dict)
import List.Extra as LE
import Random exposing (Generator)
import Task
import WebSocket
import WebSocketFramework.EncodeDecode exposing (decodeMessage, encodeMessage)
import WebSocketFramework.InternalTypes exposing (DictsWrapper(..), ServerDicts)
import WebSocketFramework.Types as Types
    exposing
        ( Changes
        , EncodeDecode
        , ErrorKind(..)
        , ErrorRsp
        , GameId
        , MessageEncoder
        , ModeChecker
        , PlayerId
        , PlayerInfo
        , PublicGame
        , PublicGames
        , RawMessage
        , ServerInterface(..)
        , ServerMessageProcessor
        , ServerState
        , ServerUrl
        , emptyPublicGames
        , emptyServerState
        , printifyString
        )


{-| `"<gameid>"`
-}
dummyGameid : GameId
dummyGameid =
    "<gameid>"


{-| Make a client connection to a proxy server.

No WebSocket connection will be used to send messages.

-}
makeProxyServer : ServerMessageProcessor gamestate player message -> (ServerInterface gamestate player message msg -> message -> msg) -> ServerInterface gamestate player message msg
makeProxyServer messageProcessor wrapper =
    ServerInterface
        { server = ""
        , wrapper = wrapper
        , state = Nothing
        , sender = proxySender messageProcessor
        }


{-| Simulate a round-trip through the message encoder, decoder, and message processor.

Returns a function that takes a request message, encodes it, decodes it, processes it into a response, encodes and decodes that, then returns the result.

Usually passed as the first arg to `makeProxyServer`.

-}
fullMessageProcessor : EncodeDecode message -> ServerMessageProcessor gamestate player message -> ServerMessageProcessor gamestate player message
fullMessageProcessor encodeDecode messageProcessor state message =
    let
        err =
            \req msg ->
                case encodeDecode.errorWrapper of
                    Nothing ->
                        Nothing

                    Just wrapper ->
                        Just <|
                            wrapper
                                { kind = JsonParseError
                                , description = req
                                , message = Err msg
                                }

        req =
            encodeMessage encodeDecode.encoder message

        dbg =
            log "fullMessageProcesor, req" <|
                printifyString req
    in
    case decodeMessage encodeDecode.decoder req of
        Err msg ->
            ( state, err req msg )

        Ok message2 ->
            let
                ( state2, rspmsg ) =
                    messageProcessor state message2

                message3 =
                    case rspmsg of
                        Nothing ->
                            Nothing

                        Just r ->
                            let
                                rsp =
                                    encodeMessage encodeDecode.encoder r

                                dbg2 =
                                    log "  rsp" <|
                                        printifyString rsp
                            in
                            case decodeMessage encodeDecode.decoder rsp of
                                Err msg ->
                                    err rsp msg

                                Ok m ->
                                    Just m
            in
            ( state2, message3 )


{-| Make a client connection to a real WebSocket server.

The `msg` will usually be a no-operation message. It is only used to fill a slot in the returned `ServerInterface`. That slot is only used by the proxy server.

-}
makeServer : MessageEncoder message -> ServerUrl -> msg -> ServerInterface gamestate player message msg
makeServer encoder server msg =
    ServerInterface
        { server = server
        , wrapper = \_ _ -> msg
        , state = Nothing
        , sender = sender encoder
        }


{-| Return the server URL from inside a ServerInterface.
-}
getServer : ServerInterface gamestate player message msg -> ServerUrl
getServer (ServerInterface interface) =
    interface.server


{-| Return a `Cmd` to send a message through a server interface.
-}
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
            Maybe.withDefault (emptyServerState Nothing) interface.state

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


{-| Create the ErrorRsp record returned in the errors from `CheckOnlyGameid`, `checkGameid`, and `checkPlayerid`.
-}
errorRsp : message -> String -> ErrorRsp message
errorRsp message text =
    { request = message
    , text = text
    }


{-| Add a game to a `ServerState`.

Adds the game ID to the added games list in changes, so that the server code will update its tables.

-}
addGame : GameId -> gamestate -> ServerState gamestate player -> ServerState gamestate player
addGame gameid gamestate state =
    let
        (DictsWrapper dicts) =
            state.dicts
    in
    { state
        | dicts =
            DictsWrapper
                { dicts
                    | gameDict =
                        Dict.insert gameid gamestate dicts.gameDict
                }
        , changes =
            case state.changes of
                Nothing ->
                    Just
                        { addedGames = [ gameid ]
                        , addedPlayers = []
                        , removedGames = []
                        , removedPlayers = []
                        }

                Just changes ->
                    Just
                        { changes
                            | addedGames = gameid :: changes.addedGames
                        }
    }


{-| Remove a game and its players from a `ServerState`.

Removes the game ID from the removed games list in changes, so that the server code will update its tables.

-}
removeGame : GameId -> ServerState gamestate player -> ServerState gamestate player
removeGame gameid state =
    let
        pids =
            getGamePlayers gameid state

        (DictsWrapper dicts) =
            state.dicts

        gameDict =
            Dict.remove gameid dicts.gameDict

        playerDict =
            List.foldl Dict.remove dicts.playerDict pids

        gamePlayersDict =
            Dict.remove gameid dicts.gamePlayersDict
    in
    { state
        | dicts =
            DictsWrapper
                { dicts
                    | gameDict = gameDict
                    , playerDict = playerDict
                    , gamePlayersDict = gamePlayersDict
                }
        , publicGames = removePublicGame gameid state.publicGames
        , changes =
            case state.changes of
                Nothing ->
                    Just
                        { addedGames = []
                        , addedPlayers = []
                        , removedGames = [ ( gameid, pids ) ]
                        , removedPlayers = []
                        }

                Just changes ->
                    Just
                        { changes
                            | removedGames = ( gameid, pids ) :: changes.removedGames
                        }
    }


{-| Why is this not `List.adjoin`?
-}
adjoin : a -> List a -> List a
adjoin a list =
    if List.member a list then
        list
    else
        a :: list


{-| Add a player to a `ServerState`.

Adds the player ID to the added players list in changes, so that the server code will update its tables.

-}
addPlayer : PlayerId -> PlayerInfo player -> ServerState gamestate player -> ServerState gamestate player
addPlayer playerid info state =
    let
        gameid =
            info.gameid

        tuple =
            ( gameid, playerid )

        players =
            adjoin playerid <| getGamePlayers gameid state

        (DictsWrapper dicts) =
            state.dicts

        playerDict =
            Dict.insert playerid info dicts.playerDict

        gamePlayersDict =
            Dict.insert gameid players dicts.gamePlayersDict
    in
    { state
        | dicts =
            DictsWrapper
                { dicts
                    | playerDict = playerDict
                    , gamePlayersDict = gamePlayersDict
                }
        , changes =
            case state.changes of
                Nothing ->
                    Just
                        { addedGames = []
                        , addedPlayers = [ tuple ]
                        , removedGames = []
                        , removedPlayers = []
                        }

                Just changes ->
                    Just
                        { changes
                            | addedPlayers =
                                tuple :: changes.addedPlayers
                        }
    }


{-| Remove a player from a `ServerState`.

Adds the player ID to the removed players list in changes, so that the server code will update its tables.

-}
removePlayer : PlayerId -> ServerState gamestate player -> ServerState gamestate player
removePlayer playerid state =
    let
        (DictsWrapper dicts) =
            state.dicts
    in
    case Dict.get playerid dicts.playerDict of
        Nothing ->
            state

        Just { gameid } ->
            let
                tuple =
                    ( gameid, playerid )

                players =
                    LE.remove playerid <| getGamePlayers gameid state

                playerDict =
                    Dict.remove playerid dicts.playerDict

                gamePlayersDict =
                    Dict.insert gameid players dicts.gamePlayersDict
            in
            { state
                | dicts =
                    DictsWrapper
                        { dicts
                            | playerDict = playerDict
                            , gamePlayersDict = gamePlayersDict
                        }
                , changes =
                    case state.changes of
                        Nothing ->
                            Just
                                { addedGames = []
                                , addedPlayers = []
                                , removedGames = []
                                , removedPlayers = [ tuple ]
                                }

                        Just changes ->
                            Just
                                { changes
                                    | removedPlayers =
                                        tuple :: changes.removedPlayers
                                }
            }


{-| Check that the passed `GameId` is in the `ServerState`'s game dict.

If it is, return the `gamestate`. Otherwise wrap the message in an `ErrorRsp`.

-}
checkOnlyGameid : ServerState gamestate player -> message -> GameId -> Result (ErrorRsp message) gamestate
checkOnlyGameid state message gameid =
    let
        (DictsWrapper dicts) =
            state.dicts
    in
    case Dict.get gameid dicts.gameDict of
        Just gameState ->
            Ok gameState

        Nothing ->
            Err <| errorRsp message "Unknown gameid"


{-| Check that the passed `PlayerId` is in the `ServerState`'s player dict.

If it is, return the `PlayerInfo` record for the player.

Otherwise wrap the message in an `ErrorRsp`.

-}
checkPlayerid : ServerState gamestate player -> message -> PlayerId -> Result (ErrorRsp message) (PlayerInfo player)
checkPlayerid state message playerid =
    let
        (DictsWrapper dicts) =
            state.dicts
    in
    case Dict.get playerid dicts.playerDict of
        Nothing ->
            Err <| errorRsp message ("Unknown playerid " ++ playerid)

        Just info ->
            Ok info


{-| Check that the passed `GameId` is in the `ServerState`'s game dict and that it satisifed the `ModeChecker`.

If it does, return the `gamestate`. Otherwise wrap the message in an `ErrorRsp`.

-}
checkGameid : ModeChecker gamestate message -> ServerState gamestate player -> message -> GameId -> Result (ErrorRsp message) gamestate
checkGameid checker state message gameid =
    case checkOnlyGameid state message gameid of
        Ok gamestate ->
            case checker gamestate message of
                Ok _ ->
                    Ok gamestate

                Err msg ->
                    Err (errorRsp message msg)

        err ->
            err


{-| Push a `PublicGame` onto a list of them.
-}
appendPublicGames : PublicGame -> PublicGames -> PublicGames
appendPublicGames game games =
    List.append games [ game ]


{-| Remove the `PublicGame` with the given `GameId` from a list of games.
-}
removePublicGame : GameId -> PublicGames -> PublicGames
removePublicGame gameid games =
    List.filter (\game -> game.gameid /= gameid) games


lowercaseLetter : Generator Char
lowercaseLetter =
    Random.map (\n -> Char.fromCode (n + 97)) (Random.int 0 25)


{-| (log (expt 26 16) 2) -> 75
-}
gameidLength : Int
gameidLength =
    16


gameidGenerator : Generator GameId
gameidGenerator =
    Random.map String.fromList <|
        Random.list gameidLength lowercaseLetter


randomConstant : a -> Generator a
randomConstant value =
    Random.map (\_ -> value) Random.bool


uniqueGameidGenerator : ServerState gamestate player -> Generator GameId
uniqueGameidGenerator state =
    let
        (DictsWrapper dicts) =
            state.dicts
    in
    Random.andThen
        (\gameid ->
            case Dict.get gameid dicts.gameDict of
                Nothing ->
                    randomConstant gameid

                Just _ ->
                    uniqueGameidGenerator state
        )
        gameidGenerator


uniquePlayeridGenerator : ServerState gamestate player -> Generator PlayerId
uniquePlayeridGenerator state =
    Random.andThen
        (\gameid ->
            let
                playerid =
                    "P" ++ gameid

                (DictsWrapper dicts) =
                    state.dicts
            in
            case Dict.get playerid dicts.playerDict of
                Nothing ->
                    randomConstant playerid

                Just _ ->
                    uniquePlayeridGenerator state
        )
        gameidGenerator


{-| Generate a random GameId string, ensuring that it is not already assigned to a game.

Will not be used by servers that have no concept of a game.

-}
newGameid : ServerState gamestate player -> ( GameId, ServerState gamestate player )
newGameid state =
    let
        ( id, seed ) =
            Random.step (uniqueGameidGenerator state) state.seed
    in
    ( id, { state | seed = seed } )


{-| Generate a random PlayerId string, ensuring that it is not already assigned to a player.

Will not be used by servers that have no concept of a player.

-}
newPlayerid : ServerState gamestate player -> ( PlayerId, ServerState gamestate player )
newPlayerid state =
    let
        ( id, seed ) =
            Random.step (uniquePlayeridGenerator state) state.seed
    in
    ( id, { state | seed = seed } )


{-| Look up the gamestate for a GameId
-}
getGame : GameId -> ServerState gamestate player -> Maybe gamestate
getGame gameid state =
    let
        (DictsWrapper dicts) =
            state.dicts
    in
    Dict.get gameid dicts.gameDict


{-| Update the gamestate for a GameId.

Use `removeGame` to delete a game.

-}
updateGame : GameId -> gamestate -> ServerState gamestate player -> ServerState gamestate player
updateGame gameid gamestate state =
    let
        (DictsWrapper dicts) =
            state.dicts
    in
    { state
        | dicts =
            DictsWrapper
                { dicts
                    | gameDict =
                        Dict.insert gameid gamestate dicts.gameDict
                }
    }


{-| Return the total number of games.
-}
gameCount : ServerState gamestate player -> Int
gameCount state =
    let
        (DictsWrapper dicts) =
            state.dicts
    in
    Dict.size dicts.gameDict


{-| Look up the PlayerInfo for a PlayerId
-}
getPlayer : PlayerId -> ServerState gamestate player -> Maybe (PlayerInfo player)
getPlayer playerid state =
    let
        (DictsWrapper dicts) =
            state.dicts
    in
    Dict.get playerid dicts.playerDict


{-| Update the PlyaerInfo for a PlayerId.

Use `removePlayer` to delete a player.

-}
updatePlayer : PlayerId -> PlayerInfo player -> ServerState gamestate player -> ServerState gamestate player
updatePlayer playerid info state =
    let
        (DictsWrapper dicts) =
            state.dicts
    in
    { state
        | dicts =
            DictsWrapper
                { dicts
                    | playerDict =
                        Dict.insert playerid info dicts.playerDict
                }
    }


{-| Get the player IDs for a game ID.
-}
getGamePlayers : GameId -> ServerState gamestate player -> List PlayerId
getGamePlayers gameid state =
    let
        (DictsWrapper dicts) =
            state.dicts
    in
    case Dict.get gameid dicts.gamePlayersDict of
        Nothing ->
            []

        Just pids ->
            pids


{-| Return true if the GameId is in the public games list.
-}
isPublicGame : GameId -> ServerState gamestate player -> Bool
isPublicGame gameid state =
    case LE.find (.gameid >> (==) gameid) state.publicGames of
        Just _ ->
            True

        Nothing ->
            False


{-| Return true if the GameId is NOT in the public games list.
-}
isPrivateGame : GameId -> ServerState gamestate player -> Bool
isPrivateGame gameid state =
    not <| isPublicGame gameid state
