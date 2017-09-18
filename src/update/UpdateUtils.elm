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
            resizeKeysList pressedKeys
    in
    let
        ( newModel, newCmd ) =
            moveSnake direction model
    in
    ( { newModel | pressedKeys = pressedKeys }, newCmd )



-- Checks that the new food will be spawned in an empty cell.


handleFoodMsg : Location -> Model -> ( Model, Cmd Msg )
handleFoodMsg food model =
    if List.member food model.snake then
        ( model, Random.generate Food <| pair (int 0 9) (int 0 9) )
    else
        ( { model | foodLocation = food, board = Matrix.set food PresentFood model.board }, Cmd.none )


moveSnake : Direction -> Model -> ( Model, Cmd Msg )
moveSnake direction model =
    if model.status /= Lost then
        case updateSnake direction model of
            Just xs ->
                let
                    newMessage =
                        isEaten model.snake xs

                    lastMove =
                        if direction /= NoDirection then
                            direction
                        else
                            model.lastMove
                in
                if xs == model.snake then
                    ( model, newMessage )
                else
                    ( { model | board = addSnakeAndFood xs model.foodLocation model.board, snake = xs, lastMove = lastMove }, newMessage )

            Nothing ->
                ( { model | status = Lost }, Cmd.none )
    else
        ( { model | pressedKeys = [] }, Cmd.none )



-- Flushes the key list to avoid multiple keys pressed.


resizeKeysList : List Key -> Direction
resizeKeysList keys =
    if List.length keys == 1 then
        arrowsDirection keys
    else
        arrowsDirection <| List.drop (List.length keys - 1) keys


isEaten : Snake -> Snake -> Cmd Msg
isEaten oldSnake newSnake =
    if List.length oldSnake == List.length newSnake then
        Cmd.none
    else
        Random.generate Food <| pair (int 0 9) (int 0 9)


updateSnake : Direction -> Model -> Maybe Snake
updateSnake direction model =
    if direction /= NoDirection then
        case List.head model.snake of
            Just x ->
                let
                    newHead =
                        parseHead direction x model
                in
                case Matrix.get newHead model.board of
                    Just PresentFood ->
                        Just <| newHead :: model.snake

                    Just Absent ->
                        ListE.init <| newHead :: model.snake

                    Just PresentSnake ->
                        -- Inhibits the snake to go backwards.
                        if List.member newHead <| List.take 2 model.snake then
                            Just model.snake
                        else
                            Nothing

                    Nothing ->
                        Nothing

            Nothing ->
                Nothing
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
