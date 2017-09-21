module Cells exposing (view)

import Html exposing (Html, tr)
import Matrix
import Model exposing (..)
import ViewUtils exposing (renderCell)


view : Model -> List (Html Msg)
view model =
    model.board
        |> Matrix.map (\c -> renderCell c)
        >> Matrix.toList
        >> List.map (\r -> tr [] r)
