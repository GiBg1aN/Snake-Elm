module Subscriptions exposing (..)

import Keyboard.Extra as KE
import Model exposing (..)
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map KeyboardMsg KE.subscriptions, Time.every Time.second Tick ]
