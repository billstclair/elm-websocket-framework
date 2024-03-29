---------------------------------------------------------------------
--
-- Types.elm
-- Shared types for WebSocketFramework module.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module WebSocketFramework.Types exposing
    ( ServerState, Dicts, ServerInterface(..)
    , PlayerInfo, PublicGame, PublicGames, Changes
    , emptyServerState, emptyPublicGames
    , RawMessage, ReqRsp(..), Plist, ErrorRsp
    , ServerMessageProcessor, MessageToGameid, ModeChecker
    , MessageDecoder, MessageEncoder, EncodeDecode, DecoderPlist
    , Statistics, emptyStatistics
    , printifyString
    , InputPort, OutputPort
    , ErrorKind(..), Error
    , GameId, PlayerId, ServerUrl
    )

{-| Types used by the rest of the WebSocketFramework modules.


# State

@docs ServerState, Dicts, ServerInterface
@docs PlayerInfo, PublicGame, PublicGames, Changes


# Empty states

@docs emptyServerState, emptyPublicGames


# Messages

@docs RawMessage, ReqRsp, Plist, ErrorRsp


# Function Signatures

@docs ServerMessageProcessor, MessageToGameid, ModeChecker


# Message Encoding/Decoding

@docs MessageDecoder, MessageEncoder, EncodeDecode, DecoderPlist


# Statistics

@docs Statistics, emptyStatistics


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
import Time exposing (Posix)
import WebSocketFramework.InternalTypes as IT
    exposing
        ( DictsWrapper(..)
        , ServerDicts
        , emptyServerDicts
        )


{-| An alias for `String`.
-}
type alias ServerUrl =
    String


{-| An alias for `String`.
-}
type alias GameId =
    IT.GameId


{-| An alias for `String`.
-}
type alias PlayerId =
    IT.PlayerId


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


{-| Information about a player in a game.
-}
type alias PlayerInfo player =
    -- This is a copy of `InternalTypes.PlayerInfo`.
    -- It is copied here for the documentation
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


{-| Opaque type used to store game and player state.
-}
type alias Dicts gamestate player =
    DictsWrapper (ServerDicts gamestate player)


{-| Track statistics about connections and games.
-}
type alias Statistics =
    Dict String Int


{-| Return a new `Statistics` record, with your value for the `statistics` field.

Until you populate the ServerState.statistics field, no connection
statistics will be gathered.

-}
emptyStatistics : Statistics
emptyStatistics =
    Dict.empty


{-| The part of the server state that is independent from its socket connections.

To access the opaque `dicts`, use `addGame`, `addPlayer`, `getGame`, `getPlayer`, `getGamePlayers`, `updateGame`, `updatePlayer`, `removeGame`, `removePlayer` from `WebsocketFramework.ServerInterface`.

-}
type alias ServerState gamestate player =
    { dicts : Dicts gamestate player
    , publicGames : PublicGames
    , state : Maybe gamestate --used by servers with no concept of game
    , seed : Random.Seed
    , changes : Maybe Changes
    , statistics : Maybe Statistics
    , time : Posix
    }


{-| Create a mostly empty `ServerState`.

You'll need to initialize the `seed` property in your real server,
if you use `newGameid` and `newPlayerid` in `WebSocketFramework.ServerInterface`.

You'll need to initialize the `statistics` property, if you want to
track statistics.

-}
emptyServerState : Maybe gamestate -> ServerState gamestate player
emptyServerState gamestate =
    { dicts = DictsWrapper emptyServerDicts
    , publicGames = emptyPublicGames
    , state = gamestate
    , seed = Random.initialSeed 0
    , changes = Nothing
    , statistics = Nothing
    , time = Time.millisToPosix 0
    }


{-| Everything necessary to communicate with a server, be it real or proxy.
-}
type ServerInterface gamestate player message msg
    = ServerInterface
        { server : ServerUrl
        , serverPort : Value -> Cmd msg
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
    String.replace "\"" closingQuote string
