module MainContent exposing (view)

import Html exposing (Html, div, table, text, h1)
import Html.Attributes exposing (style)
import Model exposing (..)
import Status exposing (..)


view : Model -> List (Html Msg) -> Html Msg
view model cells =
    case model.status of
        Lost ->
            div [ style [ ( "display", "flex" ), ( "padding", "200px" ) ] ] [ h1 [ style [ ( "color", "red" ), ( "background-color", "white" ) ] ] [ text "You Lost!" ] ]

        Win ->
            div [ style [ ( "display", "flex" ), ( "padding", "200px" ) ] ] [ h1 [ style [ ( "color", "green" ), ( "background-color", "white" ) ] ] [ text "You Won!" ] ]

        Moving ->
            div [ style [ ( "display", "flex" ) ] ] [ table [] cells ]
