module Board exposing (..)

import Cell exposing (..)
import Matrix exposing (Location, Matrix)
import Snake exposing (..)


type alias Board =
    Matrix Cell


initMatrix : Board
initMatrix =
    addSnakeAndFood initSnake ( 1, 1 ) <| Matrix.square 10 (\_ -> Absent)


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
