module View exposing (..)

import Cells
import Html exposing (Html, div, text, h1, h3, b)
import Html.Attributes exposing (style)
import MainContent
import Model exposing (..)
import ResetButton
import Score


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

        score =
            div [ style [ ( "font-size", "18" ) ] ] [ text "Score: ", b [] [ Score.view model ] ]

        footer =
            div [ style [ ( "margin-top", "25px" ) ] ] [ score, reset ]
    in
        div [] [ header, board, footer ]
