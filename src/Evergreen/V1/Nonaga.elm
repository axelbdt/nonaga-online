module Evergreen.V1.Nonaga exposing (..)

import Dict
import Set


type Player
    = Red
    | Black


type TurnPhase
    = MoveToken
    | MovePlatform


type alias Platform =
    ( Int, Int )


type alias Board =
    Set.Set Platform


type alias Tokens =
    Dict.Dict Platform Player


type alias Model =
    { currentPlayer : Player
    , turnPhase : TurnPhase
    , board : Board
    , tokens : Tokens
    , lastMovedPlatform : Platform
    , selectedToken : Maybe Platform
    , selectedPlatform : Maybe Platform
    }


type Msg
    = SelectToken Player Platform
    | ChooseTokenDestination Platform Platform
    | SelectPlatform Platform
    | ChoosePlatformDestination Platform Platform
    | Reset
