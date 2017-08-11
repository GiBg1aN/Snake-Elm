module ViewUtils exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


--

import Cell exposing (..)


renderCell : Cell -> Html msg
renderCell cell =
    case cell of
        Present ->
            td [ class "cell", style [ ( "padding", "25px" ), ( "width", "5px" ), ( "height", "5px" ), ( "background-color", "black" ) ] ] []

        Absent ->
            td [ class "cell", style [ ( "padding", "25px" ), ( "width", "5px" ), ( "height", "5px" ), ( "background-color", "grey" ) ] ] []
