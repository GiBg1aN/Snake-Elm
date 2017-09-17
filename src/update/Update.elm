module Update exposing (..)

import Model exposing (..)
import UpdateUtils as Utils


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        KeyboardMsg move ->
            Utils.handleKeyBoardMsg move model

        Food food ->
            Utils.handleFoodMsg food model

        Tick _ ->
            Utils.moveSnake model.lastMove model
