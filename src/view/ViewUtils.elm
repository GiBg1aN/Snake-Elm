module ViewUtils exposing (..)

import Cell exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


renderCell : Cell -> Html msg
renderCell cell =
    case cell of
        PresentSnake ->
            td [ class "cell", style [ ( "padding", "25px" ), ( "width", "5px" ), ( "height", "5px" ), ( "background-color", "black" ) ] ] []

        PresentFood ->
            td [ class "cell", style [ ( "padding", "25px" ), ( "width", "5px" ), ( "height", "5px" ), ( "background-color", "red" ) ] ] []

        Absent ->
            td [ class "cell", style [ ( "padding", "25px" ), ( "width", "5px" ), ( "height", "5px" ), ( "background-color", "grey" ) ] ] []
