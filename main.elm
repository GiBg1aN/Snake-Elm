-- import Html exposing (..)
-- import Html.Events exposing (..)
-- import Html.Attributes exposing (..)


module Main exposing (..)

import Matrix exposing (..)


-- main : Program Never Model Msg
-- main = Html.beginnerProgram { model = model, view = view, update = update }
-- MODEL


type alias Model =
    { board : Board
    , status : Status
    , snake : Snake
    }


type alias Board =
    Matrix Cell


type Status
    = Moving
    | Lost


type Cell
    = Present
    | Absent


type alias Snake =
    List Location


type Direction
    = Up
    | Down
    | Left
    | Right


initMatrix : Int -> Int -> Matrix Cell
initMatrix m n =
    Matrix.matrix m n (\loc -> Absent)



-- still a dummy function


initSnake : Int -> Snake
initSnake n =
    [ ( 3, 3 ), ( 3, 4 ), ( 3, 5 ) ]


model : Model
model =
    { board = initMatrix 10 10, status = Moving, snake = initSnake 5 }



-- UPDATE


type Msg
    = Reset
    | Move Direction



-- update : Msg -> Model -> Model
-- update msg model =
-- VIEW
-- view : Model -> Html Msg
-- view model =
