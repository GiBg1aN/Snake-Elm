module Update exposing (..)

import Model exposing (..)
import Update.Utils


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        KeyboardMsg move ->
            Update.Utils.handleKeyBoardMsg move model

        Food food ->
            Update.Utils.handleFoodMsg food model

        Tick tick ->
            Update.Utils.moveSnake model.lastMove model
