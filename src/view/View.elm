module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


--

import Cells
import Model exposing (..)
import ResetButton
import MainContent


view : Model -> Html Msg
view model =
    let
        reset =
            ResetButton.view model

        cells =
            Cells.view model

        board =
            MainContent.view model cells
    in
        div [] [ div [] [ (h1 [] [ text "Snake" ]), (h3 [ style [ ( "padding-bottom", "10px" ) ] ] [ text "Made with Elm" ]) ], board, reset ]
