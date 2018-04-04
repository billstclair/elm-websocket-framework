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


module WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , ErrorRsp
        , MessageDecoder
        , MessageEncoder
        , ModeChecker
        , PlayerInfo
        , Plist
        , PublicGame
        , PublicGames
        , RawMessage
        , ReqRsp(..)
        , ServerInterface(..)
        , ServerMessageProcessor
        , ServerState
        , emptyPublicGames
        , printifyString
        )

{-| Types used by the rest of the WebSocketFramework modules.
-}

import Char
import Dict exposing (Dict)
import Json.Encode exposing (Value)
import String.Extra as SE


type alias Plist =
    List ( String, Value )


type alias RawMessage =
    { reqrsp : String
    , msg : String
    , plist : Plist
    }


type alias ErrorRsp message =
    { request : message
    , text : String
    }


type ReqRsp
    = Req String
    | Rsp String


type alias MessageDecoder message =
    ( ReqRsp, Plist ) -> Result String message


type alias MessageEncoder message =
    message -> ( ReqRsp, Plist )


type alias EncodeDecode message =
    { encoder : MessageEncoder message
    , decoder : MessageDecoder message
    , errorWrapper : Maybe (String -> message)
    }


type alias ModeChecker gamestate =
    gamestate -> Result String Never


type alias ServerMessageProcessor gamestate player message =
    ServerState gamestate player -> message -> ( ServerState gamestate player, Maybe message )


type alias PlayerInfo player =
    { gameid : String
    , player : player
    }


type alias PublicGame =
    { gameid : String
    , playerName : String
    }


type alias PublicGames =
    List PublicGame


emptyPublicGames : PublicGames
emptyPublicGames =
    []


{-| You might think that the names are per game, and you'd be right to think that.
-- They're not in the GameState, because they don't need to go over the wire,
-- except in the JoinRsp.
-- They're stored in a Dict in the Server model.
-}
type alias ServerState gamestate player =
    { gameDict : Dict String gamestate --gameid
    , playerDict : Dict String (PlayerInfo player) --playerid
    , publicGames : PublicGames
    , state : Maybe gamestate --used by servers with no concept of game or player
    }


type ServerInterface gamestate player message msg
    = ServerInterface
        { server : String
        , wrapper : ServerInterface gamestate player message msg -> message -> msg
        , state : Maybe (ServerState gamestate player)
        , sender : ServerInterface gamestate player message msg -> message -> Cmd msg
        }


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


closingQuote : String
closingQuote =
    stringFromCode 0x201D


printifyString : String -> String
printifyString string =
    SE.replace "\"" closingQuote string
