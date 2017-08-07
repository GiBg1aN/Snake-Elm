module Snake exposing (..)

import Matrix exposing (..)


type alias Snake =
    List Location


initSnake : Snake
initSnake =
    [ ( 3, 3 ), ( 3, 4 ), ( 3, 5 ) ]
