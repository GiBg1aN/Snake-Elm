module Subscriptions exposing (..)

import Model exposing (..)


--

import Keyboard.Extra as KE
import Time exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map KeyboardMsg KE.subscriptions, Time.every Time.second Tick ]
