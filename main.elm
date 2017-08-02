module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard.Extra as KE exposing (Direction(..), Key, arrowsDirection)
import List.Extra as ListE exposing (..)
import Matrix exposing (..)
import Random exposing (Generator, int, pair)
import Time exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
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
    | KeyboardMsg KE.Msg
    | Food Location
    | Tick Time


init : ( Model, Cmd Msg )
init =
    ( { board = initMatrix
      , status = Moving
      , snake = initSnake
      , pressedKeys = []
      , lastMove = West
      , foodLocation = ( 1, 1 )
      }
    , Cmd.none
    )


initMatrix : Board
initMatrix =
    Matrix.matrix 10 10 (\location -> Absent) |> Matrix.set ( 1, 1 ) Present >> addSnakeAndFood initSnake ( 1, 1 )


initSnake : Snake
initSnake =
    [ ( 3, 3 ), ( 3, 4 ), ( 3, 5 ) ]


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
                        Just Present ->
                            Just (newHead :: model.snake)

                        Just Absent ->
                            Just (newHead :: (model.snake |> List.take (List.length model.snake - 1)))

                        Nothing ->
                            Debug.crash "OUT OF RANGE INDEX"

            Nothing ->
                Nothing
    else
        Just model.snake


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
isEaten oldSnake newSnake =
    if List.length oldSnake == (ListE.unique >> List.length <| newSnake) then
        Cmd.none
    else
        Random.generate Food <| pair (int 0 9) (int 0 9)



-- UPDATE


moveSnake : Direction -> Model -> ( Model, Cmd Msg )
moveSnake direction model =
    if model.status /= Lost then
        case updateSnake direction model of
            Just (x :: xs) ->
                let
                    newMessage =
                        isEaten model.snake (x :: xs)

                    lastMove =
                        if direction /= NoDirection then
                            direction
                        else
                            model.lastMove
                in
                    if isbackwardColliding model.snake (x :: xs) then
                        ( model, newMessage )
                    else if isFailed x xs then
                        ( { model | status = Lost, lastMove = direction }, newMessage )
                    else
                        ( { model | board = addSnakeAndFood (x :: xs) model.foodLocation model.board, snake = x :: xs, lastMove = lastMove }, newMessage )

            Just [] ->
                Debug.crash "EMPTY SNAKE" ( model, Cmd.none )

            Nothing ->
                Debug.log "EMPTY NEWSnakeNAKE" ( model, Cmd.none )
    else
        ( { model | pressedKeys = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model | board = initMatrix, status = Moving, snake = initSnake, pressedKeys = [] }, Cmd.none )

        KeyboardMsg move ->
            let
                pressedKeys =
                    KE.update move model.pressedKeys

                direction =
                    if List.length pressedKeys == 1 then
                        arrowsDirection <| pressedKeys
                    else
                        arrowsDirection <| List.drop (List.length pressedKeys - 1) pressedKeys
            in
                let
                    ( newModel, newCmd ) =
                        moveSnake direction model
                in
                    ( { newModel | pressedKeys = pressedKeys }, newCmd )

        Food food ->
            if List.member food model.snake then
                ( model, Random.generate Food <| pair (int 0 9) (int 0 9) )
            else
                ( { model | foodLocation = food, board = Matrix.set food Present model.board }, Cmd.none )

        Tick tick ->
            moveSnake model.lastMove model



-- VIEW


renderCell : Cell -> Html msg
renderCell cell =
    case cell of
        Present ->
            td [ class "cell", style [ ( "padding", "25px" ), ( "width", "5px" ), ( "height", "5px" ), ( "background-color", "black" ) ] ] []

        Absent ->
            td [ class "cell", style [ ( "padding", "25px" ), ( "width", "5px" ), ( "height", "5px" ), ( "background-color", "grey" ) ] ] []


view : Model -> Html Msg
view model =
    let
        reset =
            div [] [ button [ onClick Reset, class "btn" ] [ text "Reset" ] ]

        cells =
            model.board |> Matrix.map (\c -> renderCell c) >> Matrix.toList >> List.map (\r -> tr [] r)

        board =
            if model.status == Lost then
                div [ style [ ( "display", "flex" ), ( "padding", "200px" ) ] ] [ h1 [] [ text "You Lost!" ] ]
            else
                div [ style [ ( "display", "flex" ) ] ] [ table [] cells ]
    in
        div [] [ div [] [ (h1 [] [ text "Snake" ]), (h3 [ style [ ( "padding-bottom", "10px" ) ] ] [ text "Made with Elm" ]) ], board, reset ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map KeyboardMsg KE.subscriptions, Time.every Time.second Tick ]
