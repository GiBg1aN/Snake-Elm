module Board exposing (..)

import Cell exposing (..)
import Constants
import Matrix exposing (Location, Matrix)
import Snake exposing (..)


type alias Board =
    Matrix Cell


initMatrix : Board
initMatrix =
    addSnakeAndFood initSnake Constants.foodStartingPoint <| Matrix.square Constants.boardSize (\_ -> Absent)


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
