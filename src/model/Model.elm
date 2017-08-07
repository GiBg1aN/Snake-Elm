module Model exposing (..)

import Matrix exposing (Location)
import Keyboard.Extra as KE exposing (Direction(..), Key, arrowsDirection)
import Time exposing (..)


--

import Board exposing (..)
import Snake exposing (..)
import Status exposing (..)


type Msg
    = Reset
    | KeyboardMsg KE.Msg
    | Food Location
    | Tick Time


type alias Model =
    { board : Board
    , status : Status
    , snake : Snake
    , pressedKeys : List Key
    , lastMove : Direction
    , foodLocation : Location
    }


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
