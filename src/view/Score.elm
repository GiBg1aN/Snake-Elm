module Score exposing (view)

import Html exposing (text)
import Model exposing (..)
import Snake


view : Model -> Html.Html msg
view model =
    List.length model.snake |> (normalize >> toString >> text)


normalize : Int -> Int
normalize length =
    let
        startingLength =
            List.length Snake.initSnake
    in
        (length - startingLength) * 10
