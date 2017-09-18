module MainContent exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Status exposing (..)


view : Model -> List (Html Msg) -> Html Msg
view model cells =
    if model.status == Lost then
        div [ style [ ( "display", "flex" ), ( "padding", "200px" ) ] ] [ h1 [] [ text "You Lost!" ] ]
    else
        div [ style [ ( "display", "flex" ) ] ] [ table [] cells ]
