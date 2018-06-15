port module WebSocketFramework.Server exposing (..)

import Char
import Debug exposing (log)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import List.Extra as LE
import Platform exposing (Program)
import Random exposing (Generator, Seed)
import Task
import Time exposing (Time)
import WebSocketFramework.EncodeDecode exposing (decodeMessage, encodeMessage)
import WebSocketFramework.ServerInterface
    exposing
        ( errorRsp
        , send
        )
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , GameId
        , MessageDecoder
        , MessageEncoder
        , MessageToGameid
        , PlayerId
        , PublicGames
        , ServerMessageProcessor
        , ServerState
        , ServerUrl
        , emptyServerState
        )
import WebSocketServer as WSS exposing (Socket)


program : servermodel -> UserFunctions servermodel message gamestate player -> Maybe gamestate -> Program Never (Model servermodel message gamestate player) Msg
program servermodel userfunctions gamestate =
    Platform.program
        { init = init servermodel userfunctions gamestate
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port inputPort : (JD.Value -> msg) -> Sub msg


port outputPort : JE.Value -> Cmd msg



-- MODEL


type alias DeathWatch =
    ( Time, GameId )


type alias DeathWatchGameids =
    Dict GameId Bool


type alias ServerMessageSender servermodel message gamestate player =
    WrappedModel servermodel message gamestate player -> Socket -> ServerState gamestate player -> message -> message -> ( WrappedModel servermodel message gamestate player, Cmd Msg )


type alias Model servermodel message gamestate player =
    { servermodel : servermodel
    , encodeDecode : EncodeDecode message
    , messageProcessor : ServerMessageProcessor gamestate player message
    , messageSender : ServerMessageSender servermodel message gamestate player
    , messageToGameid : Maybe (MessageToGameid message)
    , state : ServerState gamestate player
    , gameidDict : Dict Socket GameId
    , playeridDict : Dict GameId (List PlayerId)
    , socketsDict : Dict GameId (List Socket)
    , deathWatch : List DeathWatch
    , deathWatchGameids : DeathWatchGameids
    , time : Time
    , seed : Seed
    }


type WrappedModel servermodel message gamestate player
    = WrappedModel (Model servermodel message gamestate player)


lowercaseLetter : Generator Char
lowercaseLetter =
    Random.map (\n -> Char.fromCode (n + 97)) (Random.int 0 25)


gameidLength : Int
gameidLength =
    16



--(log (expt 26 16) 2) -> 75


gameidGenerator : Generator GameId
gameidGenerator =
    Random.map String.fromList <|
        Random.list gameidLength lowercaseLetter


newGameid : Model servermodel message gamestate player -> ( GameId, Model servermodel message gamestate player )
newGameid model =
    let
        ( res, seed ) =
            Random.step gameidGenerator model.seed

        mdl2 =
            { model | seed = seed }
    in
    case Dict.get res model.state.gameDict of
        Nothing ->
            ( res, mdl2 )

        Just _ ->
            newGameid mdl2


newPlayerid : Model servermodel message gamestate player -> ( PlayerId, Model servermodel message gamestate player )
newPlayerid model =
    let
        ( gameid, mod ) =
            newGameid model

        playerid =
            "P" ++ gameid
    in
    case Dict.get playerid model.state.playerDict of
        Nothing ->
            ( playerid, mod )

        Just _ ->
            newPlayerid mod


type alias UserFunctions servermodel message gamestate player =
    { encodeDecode : EncodeDecode message
    , messageProcessor : ServerMessageProcessor gamestate player message
    , messageSender : ServerMessageSender servermodel message gamestate player
    , messageToGameid : Maybe (MessageToGameid message)
    }


init : servermodel -> UserFunctions servermodel message gamestate player -> Maybe gamestate -> ( Model servermodel message gamestate player, Cmd Msg )
init servermodel userFunctions gamestate =
    ( { servermodel = servermodel
      , encodeDecode = userFunctions.encodeDecode
      , messageProcessor = userFunctions.messageProcessor
      , messageSender = userFunctions.messageSender
      , messageToGameid = userFunctions.messageToGameid
      , state = emptyServerState gamestate
      , gameidDict = Dict.empty
      , playeridDict = Dict.empty
      , socketsDict = Dict.empty
      , deathWatch = []
      , deathWatchGameids = Dict.empty
      , time = 0
      , seed = Random.initialSeed 0
      }
    , Task.perform FirstTick Time.now
    )



-- UPDATE


type Msg
    = Connection WSS.Socket
    | Disconnection WSS.Socket
    | SocketMessage Socket String
    | FirstTick Time
    | Tick Time
    | Noop


maybeLog : Msg -> Msg
maybeLog msg =
    case msg of
        Tick _ ->
            msg

        x ->
            log "Msg" x


update : Msg -> Model servermodel message gamestate player -> ( Model servermodel message gamestate player, Cmd Msg )
update message model =
    case maybeLog message of
        Connection socket ->
            ( model, Cmd.none )

        Disconnection socket ->
            disconnection model socket

        SocketMessage socket message ->
            socketMessage model socket message

        FirstTick time ->
            let
                seed =
                    Random.initialSeed <| round time
            in
            ( { model
                | time = time
                , seed = seed
              }
            , Cmd.none
            )

        Tick time ->
            ( doExecutions { model | time = time }
            , Cmd.none
            )

        Noop ->
            ( model, Cmd.none )


removeField : value -> (record -> value) -> List record -> List record
removeField value accessor records =
    List.filter (\record -> accessor record /= value) records


killGame : Model servermodel message gamestate player -> GameId -> Model servermodel message gamestate player
killGame model gameid =
    let
        state =
            model.state

        playerDict =
            case Dict.get (log "killgame" gameid) model.playeridDict of
                Nothing ->
                    state.playerDict

                Just ids ->
                    List.foldl Dict.remove state.playerDict ids
    in
    { model
        | state =
            { state
                | gameDict = Dict.remove gameid state.gameDict
                , playerDict = playerDict
                , publicGames = removeField gameid .gameid state.publicGames
            }
        , playeridDict = Dict.remove gameid model.playeridDict
    }


deathRowDuration : Time
deathRowDuration =
    2 * Time.minute


doExecutions : Model servermodel message gamestate player -> Model servermodel message gamestate player
doExecutions model =
    let
        time =
            model.time

        loop =
            \mod watches ->
                case watches of
                    [] ->
                        mod

                    ( tim, gid ) :: tail ->
                        if time >= tim then
                            loop
                                (killGame
                                    { mod
                                        | deathWatch = tail
                                        , deathWatchGameids =
                                            Dict.remove gid mod.deathWatchGameids
                                    }
                                    gid
                                )
                                tail
                        else
                            mod
    in
    loop model model.deathWatch


deathWatch : GameId -> Model servermodel message gamestate player -> Model servermodel message gamestate player
deathWatch gameid model =
    let
        gameids =
            model.deathWatchGameids
    in
    case Dict.get (log "deathWatch" gameid) gameids of
        Just _ ->
            model

        Nothing ->
            { model
                | deathWatchGameids = Dict.insert gameid True gameids
                , deathWatch =
                    List.append
                        model.deathWatch
                        [ ( model.time + deathRowDuration, gameid ) ]
            }


reprieve : GameId -> Model servermodel message gamestate player -> Model servermodel message gamestate player
reprieve gameid model =
    let
        gameids =
            model.deathWatchGameids
    in
    case Dict.get gameid gameids of
        Nothing ->
            model

        Just _ ->
            { model
                | deathWatchGameids = Dict.remove (log "reprieve" gameid) gameids
                , deathWatch =
                    List.filter (\( _, gid ) -> gid /= gameid) model.deathWatch
            }


disconnection : Model servermodel message gamestate player -> Socket -> ( Model servermodel message gamestate player, Cmd Msg )
disconnection model socket =
    case Dict.get socket model.gameidDict of
        Nothing ->
            ( model, Cmd.none )

        Just gameid ->
            let
                model2 =
                    { model | gameidDict = Dict.remove socket model.gameidDict }

                socketsDict =
                    model.socketsDict
            in
            case Dict.get gameid model.socketsDict of
                Nothing ->
                    ( model2, Cmd.none )

                Just sockets ->
                    let
                        socks =
                            List.filter (\s -> s /= socket) sockets

                        model3 =
                            { model2
                                | socketsDict =
                                    Dict.insert gameid socks socketsDict
                            }
                    in
                    ( if socks == [] then
                        deathWatch gameid model3
                      else
                        model3
                    , Cmd.none
                    )


sendToOne : MessageEncoder message -> message -> Socket -> Cmd Msg
sendToOne encoder message socket =
    WSS.sendToOne outputPort
        (log "send" <| encodeMessage encoder message)
        (log "  " socket)


sendToMany : MessageEncoder message -> message -> List Socket -> Cmd Msg
sendToMany encoder message sockets =
    WSS.sendToMany outputPort
        (log "send" (encodeMessage encoder message))
        (log "  " sockets)
        |> Cmd.batch


socketMessage : Model servermodel message gamestate player -> Socket -> String -> ( Model servermodel message gamestate player, Cmd Msg )
socketMessage model socket request =
    case decodeMessage model.encodeDecode.decoder request of
        Err msg ->
            case model.encodeDecode.errorWrapper of
                Nothing ->
                    ( model, Cmd.none )

                Just wrapper ->
                    let
                        response =
                            wrapper <| "Can't parse request: " ++ request
                    in
                    ( model
                    , sendToOne model.encodeDecode.encoder response socket
                    )

        Ok message ->
            let
                ( state, rsp ) =
                    model.messageProcessor model.state message
            in
            case rsp of
                Nothing ->
                    ( { model | state = state }
                    , Cmd.none
                    )

                Just response ->
                    let
                        mod =
                            case model.messageToGameid of
                                Nothing ->
                                    model

                                Just messageGameid ->
                                    case messageGameid response of
                                        Nothing ->
                                            model

                                        Just gameid ->
                                            reprieve gameid model

                        ( WrappedModel mod2, cmd ) =
                            mod.messageSender
                                (WrappedModel mod)
                                socket
                                state
                                message
                                response
                    in
                    ( mod2, cmd )



{-

   updatePublicGameId : PublicGames -> GameId -> GameId -> PublicGames
   updatePublicGameId games gameid gid =
       let
           gameList =
               games
       in
       case LE.find (\game -> game.gameid == gameid) gameList of
           Nothing ->
               games

           Just game ->
               { game | gameid = gid }
                   :: List.filter (\game -> game.gameid /= gameid) gameList


   updatePlayerid : GameId -> PlayerId -> Player -> Model -> ( Model, PlayerId )
   updatePlayerid gameid playerid player model =
       let
           ( pid, model2 ) =
               newPlayerid model

           state =
               model2.state

           ids =
               case Dict.get gameid model2.playeridDict of
                   Nothing ->
                       [ pid ]

                   Just ids ->
                       pid :: ids

           playeridDict =
               Dict.insert gameid ids model2.playeridDict

           info =
               { gameid = gameid, player = player }

           playerDict =
               Dict.insert pid info <|
                   Dict.remove playerid state.playerDict
       in
       ( { model2
           | state = { state | playerDict = playerDict }
           , playeridDict = playeridDict
         }
       , pid
       )


   processResponse : Model -> Socket -> ServerState -> message -> message -> ( Model, Cmd Msg )
   processResponse model socket state message response =
       case response of
           NewRsp { gameid, playerid, name } ->
               let
                   ( model2, _ ) =
                       disconnection model socket

                   ( gid, model3 ) =
                       newGameid model2

                   state2 =
                       case Dict.get gameid state.gameDict of
                           Nothing ->
                               state

                           --can't happen
                           Just gs ->
                               let
                                   gs2 =
                                       gs

                                   gameDict =
                                       Dict.remove gameid state.gameDict
                               in
                               { state
                                   | gameDict = Dict.insert gid gs2 gameDict
                                   , publicGames =
                                       updatePublicGameId state.publicGames
                                           gameid
                                           gid
                               }

                   model4 =
                       { model3
                           | state = state2
                           , gameidDict =
                               Dict.insert socket gid model3.gameidDict
                           , socketsDict =
                               Dict.insert gid [ socket ] model3.socketsDict
                       }

                   ( model5, pid ) =
                       updatePlayerid gid playerid WhitePlayer model4

                   response =
                       NewRsp
                           { gameid = gid
                           , playerid = pid
                           , name = name
                           }
               in
               ( model5
               , sendToOne response socket
               )

           JoinRsp { playerid, names, gameState } ->
               case messageGameid message of
                   Nothing ->
                       ( model
                       , sendToOne (errorRsp message "Can't find gameid") socket
                       )

                   Just gameid ->
                       let
                           ( model2, _ ) =
                               disconnection model socket

                           sockets =
                               case Dict.get gameid model2.socketsDict of
                                   Nothing ->
                                       []

                                   --better not happen
                                   Just socks ->
                                       socks

                           newSockets =
                               socket :: sockets

                           model3 =
                               { model2
                                   | state = state
                                   , gameidDict =
                                       Dict.insert socket gameid model2.gameidDict
                                   , socketsDict =
                                       Dict.insert gameid newSockets model2.socketsDict
                               }

                           ( model4, pid ) =
                               updatePlayerid gameid playerid BlackPlayer model3

                           rec =
                               { playerid = pid
                               , names = newNames
                               , gameState = gameState
                               }

                           rsp =
                               JoinRsp rec

                           whiteRsp =
                               JoinRsp { rec | playerid = "" }
                       in
                       ( model4
                       , Cmd.batch
                           [ sendToOne rsp socket
                           , case sockets of
                               [] ->
                                   Cmd.none

                               s :: _ ->
                                   sendToOne whiteRsp s
                           ]
                       )

           LeaveRsp { gameid } ->
               -- game is already removed from state.gameDict & state.publicGames
               -- one playerid is removed from state.playerDict
               let
                   sockets =
                       case Dict.get gameid model.socketsDict of
                           Nothing ->
                               []

                           Just socks ->
                               socks

                   pids =
                       case Dict.get gameid model.playeridDict of
                           Nothing ->
                               []

                           Just ids ->
                               ids

                   gameidDict =
                       List.foldr Dict.remove model.gameidDict sockets

                   playerDict =
                       List.foldr Dict.remove state.playerDict pids
               in
               ( { model
                   | socketsDict = Dict.remove gameid model.socketsDict
                   , playeridDict = Dict.remove gameid model.playeridDict
                   , gameidDict = gameidDict
                   , state = { state | playerDict = playerDict }
                 }
               , sendToMany response sockets
               )

           ErrorRsp _ ->
               ( model
               , sendToOne response socket
               )

           _ ->
               let
                   ( gameid, model2 ) =
                       case Dict.get socket model.gameidDict of
                           Just gid ->
                               ( gid, model )

                           Nothing ->
                               case messageGameid response of
                                   Nothing ->
                                       ( "", model )

                                   --I don't think this can happen
                                   Just gid ->
                                       let
                                           socks =
                                               case Dict.get gid model.socketsDict of
                                                   Nothing ->
                                                       [ socket ]

                                                   Just ss ->
                                                       socket :: ss
                                       in
                                       ( gid
                                       , { model
                                           | gameidDict =
                                               Dict.insert socket
                                                   gid
                                                   model.gameidDict
                                           , socketsDict =
                                               Dict.insert gid
                                                   socks
                                                   model.socketsDict
                                         }
                                       )

                   sockets =
                       case Dict.get gameid model2.socketsDict of
                           Nothing ->
                               [ socket ]

                           --can't happen
                           Just socks ->
                               socks
               in
               ( { model2 | state = state }
               , sendToMany response sockets
               )

-}
-- SUBSCRIPTIONS


decodeMsg : JD.Value -> Msg
decodeMsg value =
    let
        decoder =
            WSS.eventDecoder
                { onConnection = \socket _ -> Connection socket
                , onDisconnection = \socket _ -> Disconnection socket
                , onMessage = \socket _ message -> SocketMessage socket message
                }
    in
    JD.decodeValue decoder value
        |> Result.withDefault Noop


subscriptions : Model servermodel message gamestate player -> Sub Msg
subscriptions model =
    Sub.batch
        [ inputPort decodeMsg
        , Time.every Time.second Tick
        ]
