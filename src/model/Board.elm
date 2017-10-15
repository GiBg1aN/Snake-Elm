module Board exposing (..)

import Cell exposing (..)
import Matrix exposing (Matrix, Location, set)
import Snake exposing (..)


type alias Board =
    Matrix Cell


initMatrix : Board
initMatrix =
    Matrix.square 10 (\_ -> Absent)
        |> Matrix.set ( 1, 1 ) PresentFood
        >> addSnakeAndFood initSnake ( 1, 1 )


addSnakeAndFood : Snake -> Location -> Board -> Board
addSnakeAndFood snake food board =
    let
        aux location _ =
            if List.member location snake then
                PresentSnake
            else if location == food then
                PresentFood
            else
                Absent
    in
        Matrix.mapWithLocation aux board
