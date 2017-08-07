module View exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Matrix exposing (..)

--

import Model exposing (..)
import Status exposing (..)
import View.Utils exposing (renderCell)

view : Model -> Html Msg
view model =
    let
        reset =
            div [] [ button [ onClick Reset, class "btn" ] [ text "Reset" ] ]

        cells =
            model.board |> Matrix.map (\c -> renderCell c) >> Matrix.toList >> List.map (\r -> tr [] r)

        board =
            if model.status == Lost then
                div [ style [ ( "display", "flex" ), ( "padding", "200px" ) ] ] [ h1 [] [ text "You Lost!" ] ]
            else
                div [ style [ ( "display", "flex" ) ] ] [ table [] cells ]
    in
        div [] [ div [] [ (h1 [] [ text "Snake" ]), (h3 [ style [ ( "padding-bottom", "10px" ) ] ] [ text "Made with Elm" ]) ], board, reset ]