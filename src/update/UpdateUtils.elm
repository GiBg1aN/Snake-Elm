module UpdateUtils exposing (handleFoodMsg, handleKeyBoardMsg, moveSnake)

import Board exposing (addSnakeAndFood)
import Cell exposing (..)
import Constants
import Keyboard.Extra as KE exposing (Direction(..), Key, arrowsDirection)
import List.Extra as ListE exposing (init)
import Matrix exposing (Location)
import Model exposing (..)
import Random exposing (Generator, int, pair)
import Snake exposing (Snake, calcScore)
import Status exposing (..)
import Time


handleKeyBoardMsg : KE.Msg -> Model -> ( Model, Cmd Msg )
handleKeyBoardMsg move model =
    let
        pressedKeys =
            KE.update move model.pressedKeys

        direction =
            arrowsDirection pressedKeys
    in
        moveSnake direction model



-- Checks that the new food will be spawned in an empty cell.


handleFoodMsg : Location -> Model -> ( Model, Cmd Msg )
handleFoodMsg food model =
    if List.member food model.snake then
        ( model, Random.generate Food <| pair (int 0 (Constants.boardSize - 1)) (int 0 (Constants.boardSize - 1)) )
    else
        ( { model
            | foodLocation = food
            , board = Matrix.set food PresentFood model.board
          }
        , Cmd.none
        )


moveSnake : Direction -> Model -> ( Model, Cmd Msg )
moveSnake direction model =
    case updateSnake direction model of
        Just xs ->
            let
                newMessage =
                    isEaten model.snake xs

                score =
                    List.length >> calcScore <| xs

                newSpeed =
                    Time.second * 1 / logBase 2 (toFloat <| 5 * (score // 100) + 2)
            in
                if xs == model.snake then
                    ( model, newMessage )
                else if score == Constants.maxScore then
                    ( { model | status = Win }, Cmd.none )
                else
                    ( { model
                        | board = addSnakeAndFood xs model.foodLocation model.board
                        , snake = xs
                        , lastMove = direction
                        , speed = newSpeed
                      }
                    , newMessage
                    )

        Nothing ->
            ( { model | status = Lost }, Cmd.none )


isEaten : Snake -> Snake -> Cmd Msg
isEaten oldSnake newSnake =
    if List.length oldSnake == List.length newSnake then
        Cmd.none
    else
        Random.generate Food <| pair (int 0 (Constants.boardSize - 1)) (int 0 (Constants.boardSize - 1))


updateSnake : Direction -> Model -> Maybe Snake
updateSnake direction model =
    if direction /= NoDirection then
        List.head model.snake
            |> Maybe.andThen
                (\head ->
                    let
                        newHead =
                            parseHead direction head model

                        increaseSnake cellLocation model cellState =
                            case cellState of
                                Absent ->
                                    ListE.init <| cellLocation :: model.snake

                                PresentFood ->
                                    Just <| cellLocation :: model.snake

                                PresentSnake ->
                                    -- Inhibits the snake to go backwards.
                                    if List.member cellLocation <| List.take 2 model.snake then
                                        Just model.snake
                                    else
                                        Nothing
                    in
                        Matrix.get newHead model.board |> Maybe.andThen (increaseSnake newHead model)
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
