module Snake exposing (..)

import Matrix exposing (..)


type alias Snake =
    List Location


initSnake : Snake
initSnake =
    [ ( 3, 3 ), ( 3, 4 ), ( 3, 5 ) ]



-- For testing purposes


testingSnake : Int -> Snake
testingSnake n =
    let
        testingSnakeAux m =
            if m == 0 then
                []
            else if m % 2 == 0 then
                (List.range 0 (n - 1) |> List.map (\x -> ( m, x ))) ++ testingSnakeAux (m - 1)
            else
                (List.range 0 (n - 1) |> List.reverse |> List.map (\x -> ( m, x ))) ++ testingSnakeAux (m - 1)
    in
        testingSnakeAux <| n - 1


normalize : Int -> Int
normalize length =
    let
        startingLength =
            List.length initSnake
    in
        (length - startingLength) * 10
