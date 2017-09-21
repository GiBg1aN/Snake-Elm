module ResetButton exposing (view)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model exposing (..)


view : Model -> Html Msg
view model =
    div [] [ button [ onClick Reset, class "btn" ] [ text "Reset" ] ]
