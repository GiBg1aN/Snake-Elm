module Main exposing (..)

import Matrix exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Keyboard.Extra exposing (Key, Direction(..), arrowsDirection)


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


model : ( Model, Cmd Msg )
model =
    ( { board = initMatrix 10 10, status = Moving, snake = initSnake 5, pressedKeys = [], lastMove = West }, Cmd.none )


initMatrix : Int -> Int -> Matrix Cell
initMatrix m n =
    Matrix.matrix m n (\loc -> Absent)



-- still a dummy function


initSnake : Int -> Snake
initSnake n =
    [ ( 3, 3 ), ( 3, 4 ), ( 3, 5 ) ]


parseHead : Direction -> Location -> Model -> Location
parseHead d l model =
    let
        ( i, j ) =
            l
    in
        case d of
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
isbackwardColliding oldS newS =
    case ( oldS, newS ) of
        ( x :: xx :: xs, y :: ys ) ->
            if xx == y then
                True
            else
                False

        ( _, _ ) ->
            False


updateSnake : Snake -> Direction -> Board -> Model -> Maybe Snake
updateSnake l d m model =
    -- TODO: collapse model and board in a single argument
    if d /= NoDirection then
        case List.head l of
            Just x ->
                let
                    newHead =
                        parseHead d x model
                in
                    case Matrix.get newHead m of
                        Just Present ->
                            Just (newHead :: l)

                        Just Absent ->
                            Just (newHead :: (l |> List.take (List.length l - 1)))

                        Nothing ->
                            Debug.crash "OUT OF RANGE INDEX"

            Nothing ->
                Nothing
    else
        Just l


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model | board = initMatrix 10 10, status = Moving, snake = initSnake 5, pressedKeys = [] }, Cmd.none )

        KeyboardMsg move ->
            let
                dir =
                    arrowsDirection <| Keyboard.Extra.update move model.pressedKeys

                new_snake =
                    updateSnake model.snake dir model.board model
            in
                if model.status /= Lost then
                    case new_snake of
                        Just (x :: xs) ->
                            if isbackwardColliding model.snake (x :: xs) then
                                ( model, Cmd.none )
                            else if isFailed x xs then
                                ( { model | status = Lost, pressedKeys = (Keyboard.Extra.update move model.pressedKeys), lastMove = dir }, Cmd.none )
                            else
                                ( { model | snake = x :: xs, pressedKeys = (Keyboard.Extra.update move model.pressedKeys) }, Cmd.none )

                        Just [] ->
                            Debug.crash "EMPTY SNAKE" ( model, Cmd.none )

                        Nothing ->
                            Debug.log "EMPTY NEWSNAKE" ( model, Cmd.none )
                else
                    ( { model | pressedKeys = [] }, Cmd.none )



-- VIEW


renderCell : Cell -> Html msg
renderCell p =
    case p of
        Present ->
            td [ style [ ( "margin", "5px" ), ( "padding", "30px" ), ( "width", "5px" ), ( "height", "5px" ), ( "background-color", "black" ) ] ] []

        Absent ->
            td [ style [ ( "margin", "5px" ), ( "padding", "30px" ), ( "width", "5px" ), ( "height", "5px" ), ( "background-color", "grey" ) ] ] []


addSnake : Snake -> Board -> Board
addSnake s m =
    m
        |> Matrix.mapWithLocation
            (\lct c ->
                if List.member lct s then
                    Present
                else
                    Absent
            )


view : Model -> Html Msg
view model =
    let
        reset =
            div [] [ button [ onClick Reset, class "btn" ] [ text "Reset" ] ]

        cells =
            model.board |> (addSnake model.snake) >> (Matrix.map (\c -> renderCell c)) >> Matrix.toList >> (List.map (\r -> tr [] r))

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
