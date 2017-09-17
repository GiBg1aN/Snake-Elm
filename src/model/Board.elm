module Board exposing (..)

import Matrix exposing (..)


--

import Cell exposing (..)
import Snake exposing (..)


type alias Board =
    Matrix Cell


initMatrix : Board
initMatrix =
    Matrix.matrix 10 10 (\_ -> Absent) |> Matrix.set ( 1, 1 ) PresentFood >> addSnakeAndFood initSnake ( 1, 1 )


addSnakeAndFood : Snake -> Location -> Board -> Board
addSnakeAndFood snake food board =
    board
        |> Matrix.mapWithLocation
            (\location _ ->
                if List.member location snake then
                    PresentSnake
                else
                    Absent
            )
        |> Matrix.set food PresentFood
