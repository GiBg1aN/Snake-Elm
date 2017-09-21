module UpdateUtils exposing (handleFoodMsg, handleKeyBoardMsg, moveSnake)

import Board exposing (..)
import Cell exposing (..)
import Keyboard.Extra as KE exposing (Direction(..), Key, arrowsDirection)
import List.Extra as ListE exposing (..)
import Matrix exposing (..)
import Model exposing (..)
import Random exposing (Generator, int, pair)
import Snake exposing (..)
import Status exposing (..)


handleKeyBoardMsg : KE.Msg -> Model -> ( Model, Cmd Msg )
handleKeyBoardMsg move model =
    let
        pressedKeys =
            KE.update move model.pressedKeys

        direction =
            arrowsDirection pressedKeys

        ( newModel, newCmd ) =
            moveSnake direction model
    in
    ( newModel, newCmd )



-- Checks that the new food will be spawned in an empty cell.


handleFoodMsg : Location -> Model -> ( Model, Cmd Msg )
handleFoodMsg food model =
    if List.member food model.snake then
        ( model, Random.generate Food <| pair (int 0 9) (int 0 9) )
    else
        ( { model | foodLocation = food, board = Matrix.set food PresentFood model.board }, Cmd.none )


moveSnake : Direction -> Model -> ( Model, Cmd Msg )
moveSnake direction model =
    case updateSnake direction model of
        Just xs ->
            let
                ( isChanged, newMessage ) =
                    isEaten model.snake xs

                lastMove =
                    if direction /= NoDirection then
                        direction
                    else
                        model.lastMove

                board =
                    if isChanged then
                        ( -1, -1 )
                    else
                        model.foodLocation
            in
            if xs == model.snake then
                ( model, newMessage )
            else
                ( { model | board = addSnakeAndFood xs board model.board, snake = xs, lastMove = lastMove }, newMessage )

        Nothing ->
            ( { model | status = Lost }, Cmd.none )


isEaten : Snake -> Snake -> ( Bool, Cmd Msg )
isEaten oldSnake newSnake =
    if List.length oldSnake == List.length newSnake then
        ( False, Cmd.none )
    else
        ( True, Random.generate Food <| pair (int 0 9) (int 0 9) )


updateSnake : Direction -> Model -> Maybe Snake
updateSnake direction model =
    if direction /= NoDirection then
        List.head model.snake
            |> Maybe.andThen
                (\x ->
                    let
                        newHead =
                            parseHead direction x model
                    in
                    Matrix.get newHead model.board
                        |> Maybe.andThen
                            (\y ->
                                case y of
                                    Absent ->
                                        ListE.init <| newHead :: model.snake

                                    PresentFood ->
                                        Just <| newHead :: model.snake

                                    PresentSnake ->
                                        -- Inhibits the snake to go backwards.
                                        if List.member newHead <| List.take 2 model.snake then
                                            Just model.snake
                                        else
                                            Nothing
                            )
                )
    else
        Just model.snake


parseHead : Direction -> Location -> Model -> Location
parseHead direction location model =
    let
        ( i, j ) =
            location
    in
    case direction of
        North ->
            ( (i - 1) % 10, j )

        South ->
            ( (i + 1) % 10, j )

        West ->
            ( i, (j - 1) % 10 )

        East ->
            ( i, (j + 1) % 10 )

        _ ->
            ( i, j )
