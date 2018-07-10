---------------------------------------------------------------------
--
-- InternalTypes.elm
-- Internal types for WebSocketFramework module.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module WebSocketFramework.InternalTypes
    exposing
        ( DictsWrapper(..)
        , GameId
        , PlayerId
        , PlayerInfo
        , ServerDicts
        , emptyServerDicts
        )

import Dict exposing (Dict)


{-| An alias for `String`.
-}
type alias GameId =
    String


{-| An alias for `String`.
-}
type alias PlayerId =
    String


{-| Information about a player in a game
-}
type alias PlayerInfo player =
    { gameid : GameId
    , player : player
    }


type alias ServerDicts gamestate player =
    { gameDict : Dict GameId gamestate
    , playerDict : Dict PlayerId (PlayerInfo player)
    , gamePlayersDict : Dict GameId (List PlayerId)
    }


emptyServerDicts : ServerDicts gamestate player
emptyServerDicts =
    { gameDict = Dict.empty
    , playerDict = Dict.empty
    , gamePlayersDict = Dict.empty
    }


type DictsWrapper dicts
    = DictsWrapper dicts
