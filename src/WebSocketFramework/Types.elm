module WebSocketFramework.Types
    exposing
        ( MessageEncoder
        , MessageParser
        , Plist
        , PublicGame
        , PublicGames
        , RawMessage
        , ReqRsp(..)
        , ServerInterface(..)
        , emptyPublicGames
        )

{-| Types used by the rest of the WebSocketFramework modules.
-}

import Dict exposing (Dict)
import Json.Encode exposing (Value)


type alias Plist =
    List ( String, Value )


type alias RawMessage =
    { reqrsp : String
    , msg : String
    , plist : Plist
    }


type ReqRsp
    = Req String
    | Rsp String


type alias MessageParser message =
    ReqRsp -> Plist -> Result String message


type alias MessageEncoder message =
    message -> ( ReqRsp, Plist )


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
    }


type ServerInterface gamestate player message msg
    = ServerInterface
        { server : String
        , wrapper : ServerInterface msg -> message -> msg
        , state : Maybe (ServerState gamestate player)
        , sender : ServerInterface msg -> message -> Cmd msg
        }
