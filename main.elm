module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard.Extra exposing (Direction(..), Key, arrowsDirection)
import Matrix exposing (..)
import Random exposing (Generator, int, pair)


--TODO: HANDLE RANDOM SPAWN ON THE SNAKE
--TODO: OPTIMIZE FUNCTION PARAMETERS


main : Program Never Model Msg
main =
    Html.program
        { init = model
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



--MODEL


type alias Model =
    { board : Board
    , status : Status
    , snake : Snake
    , pressedKeys : List Key
    , lastMove : Direction
    , foodLocation : Location
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


type Msg
    = Reset
    | KeyboardMsg Keyboard.Extra.Msg
    | Food Location


model : ( Model, Cmd Msg )
model =
    ( { board = initMatrix 10 10 ( 1, 1 ) initSnake
      , status = Moving
      , snake = initSnake
      , pressedKeys = []
      , lastMove = West
      , foodLocation = ( 1, 1 )
      }
    , Cmd.none
    )


initMatrix : Int -> Int -> Location -> Snake -> Board
initMatrix m n foodLocation snake =
    Matrix.matrix m n (\location -> Absent) |> Matrix.set foodLocation Present >> addSnake snake foodLocation



-- still a dummy function


initSnake : Snake
initSnake =
    [ ( 3, 3 ), ( 3, 4 ), ( 3, 5 ) ]


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
                Debug.crash "INVALID KEY"


isbackwardColliding : Snake -> Snake -> Bool
isbackwardColliding oldSnake newSnake =
    case ( oldSnake, newSnake ) of
        ( x :: xx :: xs, y :: ys ) ->
            if xx == y then
                True
            else
                False

        ( _, _ ) ->
            False


updateSnake : Snake -> Direction -> Board -> Model -> Maybe Snake
updateSnake snake direction board model =
    -- TODO: collapse model and board in a single argument
    if direction /= NoDirection then
        case List.head snake of
            Just x ->
                let
                    newHead =
                        parseHead direction x model
                in
                    case Matrix.get newHead board of
                        Just Present ->
                            Just (newHead :: snake)

                        Just Absent ->
                            Just (newHead :: (snake |> List.take (List.length snake - 1)))

                        Nothing ->
                            Debug.crash "OUT OF RANGE INDEX"

            Nothing ->
                Nothing
    else
        Just snake


isFailed : Location -> Snake -> Bool
isFailed location snake =
    case snake of
        x :: xs ->
            if x == location then
                True
            else
                isFailed location xs

        [] ->
            False


isEaten : Snake -> Snake -> Cmd Msg
isEaten snake1 snake2 =
    if List.length snake1 == List.length snake2 then
        Cmd.none
    else
        Random.generate Food <| pair (int 0 10) (int 0 10)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model
                | board = initMatrix 10 10 ( 1, 1 ) initSnake
                , status = Moving
                , snake = initSnake
                , pressedKeys = []
              }
            , Cmd.none
            )

        KeyboardMsg move ->
            let
                pressedKeys =
                    Keyboard.Extra.update move model.pressedKeys

                direction =
                    if List.length pressedKeys == 1 then
                        arrowsDirection <| pressedKeys
                    else
                        arrowsDirection <| List.drop (List.length pressedKeys - 1) pressedKeys

                newSnake =
                    updateSnake model.snake direction model.board model
            in
                if model.status /= Lost then
                    case newSnake of
                        Just (x :: xs) ->
                            let
                                newMessage =
                                    isEaten model.snake (x :: xs)
                            in
                                if isbackwardColliding model.snake (x :: xs) then
                                    ( model, newMessage )
                                else if isFailed x xs then
                                    ( { model
                                        | status = Lost
                                        , pressedKeys = Keyboard.Extra.update move model.pressedKeys
                                        , lastMove = direction
                                      }
                                    , newMessage
                                    )
                                else
                                    ( { model
                                        | board = addSnake (x :: xs) model.foodLocation model.board
                                        , snake = x :: xs
                                        , pressedKeys = Keyboard.Extra.update move model.pressedKeys
                                      }
                                    , newMessage
                                    )

                        Just [] ->
                            Debug.crash "EMPTY SNAKE" ( model, Cmd.none )

                        Nothing ->
                            Debug.log "EMPTY NEWSnakeNAKE" ( model, Cmd.none )
                else
                    ( { model | pressedKeys = [] }, Cmd.none )

        Food food ->
            ( { model
                | foodLocation = food
                , board = Matrix.set food Present model.board
              }
            , Cmd.none
            )



-- VIEW


renderCell : Cell -> Html msg
renderCell cell =
    case cell of
        Present ->
            td [ style [ ( "margin", "5px" ), ( "padding", "30px" ), ( "width", "5px" ), ( "height", "5px" ), ( "background-color", "black" ) ] ] []

        Absent ->
            td [ style [ ( "margin", "5px" ), ( "padding", "30px" ), ( "width", "5px" ), ( "height", "5px" ), ( "background-color", "grey" ) ] ] []


addSnake : Snake -> Location -> Board -> Board
addSnake snake food board =
    board
        |> Matrix.mapWithLocation
            (\location c ->
                if List.member location snake then
                    Present
                else
                    Absent
            )
        >> Matrix.set food Present


view : Model -> Html Msg
view model =
    let
        reset =
            div [] [ button [ onClick Reset, class "btn" ] [ text "Reset" ] ]

        cells =
            model.board |> Matrix.map (\c -> renderCell c) >> Matrix.toList >> List.map (\r -> tr [] r)

        board =
            if model.status == Lost then
                div [ style [ ( "display", "flex" ), ( "padding", "20px" ) ] ] [ h1 [] [ text "You Lost!" ] ]
            else
                div [ style [ ( "display", "flex" ), ( "padding", "25px" ) ] ] [ table [] cells ]
    in
        div [] [ div [] [ h3 [ style [ ( "padding-bottom", "5px" ) ] ] [] ], board, reset ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map KeyboardMsg Keyboard.Extra.subscriptions
