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


newGameid : WrappedModel servermodel message gamestate player -> ( GameId, WrappedModel servermodel message gamestate player )
newGameid (WrappedModel model) =
    let
        ( res, seed ) =
            Random.step gameidGenerator model.seed

        mdl2 =
            WrappedModel { model | seed = seed }
    in
    case Dict.get res model.state.gameDict of
        Nothing ->
            ( res, mdl2 )

        Just _ ->
            newGameid mdl2


newPlayerid : WrappedModel servermodel message gamestate player -> ( PlayerId, WrappedModel servermodel message gamestate player )
newPlayerid model =
    let
        ( gameid, mod ) =
            newGameid model

        playerid =
            "P" ++ gameid
    in
    case mod of
        WrappedModel mdl ->
            case Dict.get playerid mdl.state.playerDict of
                Nothing ->
                    ( playerid, mod )

                Just _ ->
                    newPlayerid mod



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
