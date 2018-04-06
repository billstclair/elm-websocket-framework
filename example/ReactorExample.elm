---------------------------------------------------------------------
--
-- ReactorExample.elm
-- Top-level non-port WebSocketFramework example.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ReactorExample exposing (..)

import Html
import SharedUI exposing (Msg(..), fullProcessor, update, view)
import WebSocketFramework.ServerInterface exposing (makeProxyServer)


main =
    Html.program
        { init =
            SharedUI.init <|
                makeProxyServer fullProcessor IncomingMessage
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
