module Evergreen.V2.Rooms exposing (..)

import Dict
import Evergreen.V2.Nonaga
import Evergreen.V2.RoomId
import Set


type alias UserId =
    String


type Room
    = WaitingForPlayers
        { id : Evergreen.V2.RoomId.RoomId
        , users : Set.Set UserId
        }
    | Playing
        { id : Evergreen.V2.RoomId.RoomId
        , users : Dict.Dict UserId Evergreen.V2.Nonaga.Player
        , gameModel : Evergreen.V2.Nonaga.Model
        }


type Rooms
    = Rooms (Dict.Dict String Room)
