module ResetButton exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


--

import Model exposing (..)


view : Model -> Html Msg
view model =
    div [] [ button [ onClick Reset, class "btn" ] [ text "Reset" ] ]
