module Score exposing (view)

import Html exposing (text)
import Model exposing (..)
import Snake exposing (normalize)


view : Model -> Html.Html msg
view model =
    List.length model.snake |> (normalize >> toString >> text)
