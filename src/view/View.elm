module View exposing (..)

import Cells
import Html exposing (Html, div, text, h1, h3)
import Html.Attributes exposing (style)
import MainContent
import Model exposing (..)
import ResetButton


view : Model -> Html Msg
view model =
    let
        reset =
            ResetButton.view model

        cells =
            Cells.view model

        board =
            MainContent.view model cells

        header =
            div [] [ h1 [] [ text "Snake" ], h3 [ style [ ( "padding-bottom", "10px" ) ] ] [ text "Made with Elm" ] ]
    in
        div [] [ header, board, reset ]
