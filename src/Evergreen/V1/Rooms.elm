module Evergreen.V1.Rooms exposing (..)

import Dict
import Evergreen.V1.Nonaga
import Evergreen.V1.RoomId
import Set


type alias UserId =
    String


type Room
    = WaitingForPlayers
        { id : Evergreen.V1.RoomId.RoomId
        , users : Set.Set UserId
        }
    | Playing
        { id : Evergreen.V1.RoomId.RoomId
        , users : Dict.Dict UserId Evergreen.V1.Nonaga.Player
        , gameModel : Evergreen.V1.Nonaga.Model
        }


type Rooms
    = Rooms (Dict.Dict String Room)
