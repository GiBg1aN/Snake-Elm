module Main exposing (main)

import Html
import Model exposing (..)
import Subscriptions
import Update
import View


main : Program Never Model Msg
main =
    Html.program
        { init = Model.init
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }
