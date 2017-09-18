module Cells exposing (view)

import Html exposing (..)
import Matrix exposing (..)
import Model exposing (..)
import ViewUtils exposing (renderCell)


view : Model -> List (Html Msg)
view model =
    model.board |> Matrix.map (\c -> renderCell c) >> Matrix.toList >> List.map (\r -> tr [] r)
