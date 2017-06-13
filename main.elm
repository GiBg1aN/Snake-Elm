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
    , lastMove : Direction
    }


type Cell
    = Present
    | Absent


type alias Board =
    Matrix Cell


type Status
    = Moving
    | Lost


type alias Snake =
    List Location


type Direction
    = Up
    | Down
    | Left
    | Right


model : Model
model =
    { board = initMatrix 10 10, status = Moving, snake = initSnake 5, lastMove = Up }


initMatrix : Int -> Int -> Matrix Cell
initMatrix m n =
    Matrix.matrix m n (\loc -> Absent)



-- still a dummy function


initSnake : Int -> Snake
initSnake n =
    [ ( 3, 3 ), ( 3, 4 ), ( 3, 5 ) ]


updateSnake : Snake -> Direction -> Board -> Maybe Snake
updateSnake l d m =
    case List.head l of
        Just x ->
            let
                ( i, j ) =
                    x
            in
            let
                newHead =
                    case d of
                        Up ->
                            ( i + 1, j )

                        Down ->
                            ( i - 1, j )

                        Left ->
                            ( i, j - 1 )

                        Right ->
                            ( i, j + 1 )
            in
            case Matrix.get newHead m of
                Just Present ->
                    Just (newHead :: l)

                Just Absent ->
                    Just (newHead :: (l |> List.take (List.length l - 1)))

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


isFailed : Location -> Snake -> Bool
isFailed c l =
    case l of
        x :: xs ->
            if x == c then
                True
            else
                isFailed c xs

        [] ->
            False



-- UPDATE


type Msg
    = Reset
    | Move Direction


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            { model | board = initMatrix 10 10, status = Moving, snake = initSnake 5 }

        Move move ->
            let
                new_snake =
                    updateSnake model.snake move model.board
            in
            case new_snake of
                Just l ->
                    case l of
                        x :: xs ->
                            if isFailed x l then
                                { model | snake = l, status = Lost }
                            else
                                { model | snake = l }

                        _ ->
                            model

                Nothing ->
                    model



-- VIEW
-- view : Model -> Html Msg
-- view model =
