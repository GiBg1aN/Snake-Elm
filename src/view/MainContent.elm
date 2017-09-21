module MainContent exposing (view)

import Html exposing (Html, div, table, text, h1)
import Html.Attributes exposing (style)
import Model exposing (..)
import Status exposing (..)


view : Model -> List (Html Msg) -> Html Msg
view model cells =
    if model.status == Lost then
        div [ style [ ( "display", "flex" ), ( "padding", "200px" ) ] ] [ h1 [] [ text "You Lost!" ] ]
    else
        div [ style [ ( "display", "flex" ) ] ] [ table [] cells ]
