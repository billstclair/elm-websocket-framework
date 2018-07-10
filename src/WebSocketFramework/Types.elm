---------------------------------------------------------------------
--
-- Types.elm
-- Internal types for WebSocketFramework module.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module WebSocketFramework.Types
    exposing
        ( Changes
        , DecoderPlist
        , EncodeDecode
        , Error
        , ErrorKind(..)
        , ErrorRsp
        , GameId
        , InputPort
        , MessageDecoder
        , MessageEncoder
        , MessageToGameid
        , ModeChecker
        , OutputPort
        , PlayerId
        , PlayerInfo
        , Plist
        , PublicGame
        , PublicGames
        , RawMessage
        , ReqRsp(..)
        , ServerInterface(..)
        , ServerMessageProcessor
        , ServerState
        , ServerUrl
        , emptyPublicGames
        , emptyServerState
        , printifyString
        )

{-| Types used by the rest of the WebSocketFramework modules.


# State

@docs ServerState, ServerInterface, PlayerInfo, PublicGame, PublicGames, Changes


# Empty states

@docs emptyServerState, emptyPublicGames


# Messages

@docs RawMessage, ReqRsp, Plist, ErrorRsp


# Function Signatures

@docs ServerMessageProcessor, MessageToGameid, ModeChecker


# Message Encoding/Decoding

@docs MessageDecoder, MessageEncoder, EncodeDecode, DecoderPlist


# Utility

@docs printifyString


# Server Ports

@docs InputPort, OutputPort


# Errors

@docs ErrorKind, Error


# Aliases

@docs GameId, PlayerId, ServerUrl

-}

import Char
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random
import String.Extra as SE


{-| An alias for `String`.
-}
type alias ServerUrl =
    String


{-| An alias for `String`.
-}
type alias GameId =
    String


{-| An alias for `String`.
-}
type alias PlayerId =
    String


{-| A list of key/value pairs.
-}
type alias Plist =
    List ( String, Value )


{-| A raw request, ready to be encoded to go over the wire.

`reqrsp` is "req" or "rsp" for request or response.

`msg` is the name of the message.

`plist` is the parameters of the message.

-}
type alias RawMessage =
    { reqrsp : String
    , msg : String
    , plist : Plist
    }


{-| An error response that encapsulates a request message and an error string.
-}
type alias ErrorRsp message =
    { request : message
    , text : String
    }


{-| A type safe representation of the `reqrsp` and `msg` fields of a `RawMessage`.
-}
type ReqRsp
    = Req String
    | Rsp String


{-| Type signature for a function that turns a request or response and a key/value list into a message.
-}
type alias MessageDecoder message =
    ( ReqRsp, Plist ) -> Result String message


{-| Type signature for a function that turns a message into request or response and a key/value list.
-}
type alias MessageEncoder message =
    message -> ( ReqRsp, Plist )


{-| The kind of error being reported in an `Error` instance.
-}
type ErrorKind
    = JsonParseError
    | UnspecifiedError


{-| Description of an error passed to `EncodeDecode.errorWrapper`.

If `kind` is `JsonParseError`, then `description` is the message text,
and `message` is the error returned from parsing it.

If `kind` is `UnspecifiedError`, then `description` is user-level text,
and `message` is `Ok` wrapped around the relevant message, if there
is one, or `Err` wrapped around some likely useless string.

-}
type alias Error message =
    { kind : ErrorKind
    , description : String
    , message : Result String message
    }


{-| Wrapping of encoder, decoder, and error message creator.
-}
type alias EncodeDecode message =
    { encoder : MessageEncoder message
    , decoder : MessageDecoder message
    , errorWrapper : Maybe (Error message -> message)
    }


{-| Map a list of request or response names to decoders for their messages.
-}
type alias DecoderPlist msg =
    List ( String, Decoder msg )


{-| Type signature of a function that checks if a message is a legal request for a gamestate.

An `Err` result has a message string. An `Ok` result maps an unusable value.

-}
type alias ModeChecker gamestate message =
    gamestate -> message -> Result String Never


{-| Type signature of a function which turns a server state and a request message into a ServerState and maybe a response message.
-}
type alias ServerMessageProcessor gamestate player message =
    ServerState gamestate player -> message -> ( ServerState gamestate player, Maybe message )


{-| Type signature of a function that extracts the game id from a message, if it has one.
-}
type alias MessageToGameid message =
    message -> Maybe GameId


{-| Information about a player in a game
-}
type alias PlayerInfo player =
    { gameid : GameId
    , player : player
    }


{-| If your server supports public games, this represents the game id and name of the creator of the game.
-}
type alias PublicGame =
    { gameid : GameId
    , playerName : String
    }


{-| A list of pubic games.
-}
type alias PublicGames =
    List PublicGame


{-| An empty list of public games.
-}
emptyPublicGames : PublicGames
emptyPublicGames =
    []


{-| Used to inform the server of added and removed games and players.

Interact with it via `ServerInterface.addGame`, `addPlayer`, `removeGame`, `removePlayer`.

-}
type alias Changes =
    { addedGames : List GameId
    , addedPlayers : List ( GameId, PlayerId )
    , removedGames : List ( GameId, List PlayerId )
    , removedPlayers : List ( GameId, PlayerId )
    }


{-| The part of the server state that is independent from its socket connections.

You will rarely access the three `Dict`s directly. Instead, use `addGame`, `addPlayer`, `getGame`, `getPlayer`, `updateGame`, `updatePlayer`, `removeGame`, `removePlayer` from `WebsocketFramework.ServerInterface`.

-}
type alias ServerState gamestate player =
    { gameDict : Dict GameId gamestate
    , playerDict : Dict PlayerId (PlayerInfo player)
    , gamePlayersDict : Dict GameId (List PlayerId)
    , publicGames : PublicGames
    , state : Maybe gamestate --used by servers with no concept of game
    , seed : Random.Seed
    , changes : Maybe Changes
    }


{-| Create a mostly empty `ServerState`.

You'll need to initialize the `seed` property in your real server,
if you use `newGameid` and `newPlayerid` in `WebSocketFramework.ServerInterface`.

-}
emptyServerState : Maybe gamestate -> ServerState gamestate player
emptyServerState gamestate =
    { gameDict = Dict.empty
    , playerDict = Dict.empty
    , gamePlayersDict = Dict.empty
    , publicGames = emptyPublicGames
    , state = gamestate
    , seed = Random.initialSeed 0
    , changes = Nothing
    }


{-| Everything necessary to communicate with a server, be it real or proxy.
-}
type ServerInterface gamestate player message msg
    = ServerInterface
        { server : ServerUrl
        , wrapper : ServerInterface gamestate player message msg -> message -> msg
        , state : Maybe (ServerState gamestate player)
        , sender : ServerInterface gamestate player message msg -> message -> Cmd msg
        }


{-| The input port from the Node.js code to the WebSocket server.
-}
type alias InputPort msg =
    (Value -> msg) -> Sub msg


{-| The output port from the WebSocket server to the Node.js code.
-}
type alias OutputPort msg =
    Value -> Cmd msg


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


closingQuote : String
closingQuote =
    stringFromCode 0x201D


{-| Convert ASCII double-quote characters to curly end quotes.
-}
printifyString : String -> String
printifyString string =
    SE.replace "\"" closingQuote string
