module Board exposing (..)

import Matrix exposing (..)


--

import Cell exposing (..)
import Snake exposing (..)


type alias Board =
    Matrix Cell


initMatrix : Board
initMatrix =
    Matrix.matrix 10 10 (\location -> Absent) |> Matrix.set ( 1, 1 ) Present >> addSnakeAndFood initSnake ( 1, 1 )


addSnakeAndFood : Snake -> Location -> Board -> Board
addSnakeAndFood snake food board =
    board
        |> Matrix.mapWithLocation
            (\location c ->
                if List.member location snake then
                    Present
                else
                    Absent
            )
        |> Matrix.set food Present
