module Score exposing (view)

import Html exposing (text)
import Model exposing (..)
import Snake exposing (calcScore)


view : Model -> Html.Html msg
view model =
    List.length model.snake |> (calcScore >> toString >> text)
