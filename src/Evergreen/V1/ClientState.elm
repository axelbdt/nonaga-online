module Evergreen.V1.ClientState exposing (..)

import Evergreen.V1.Nonaga
import Evergreen.V1.RoomId
import Evergreen.V1.Rooms


type ClientState
    = RoomSelection
        { roomIdInputText : String
        , roomFull : Bool
        }
    | WaitingForPlayers
        { roomId : Evergreen.V1.RoomId.RoomId
        , userId : Evergreen.V1.Rooms.UserId
        , playersNeeded : Int
        }
    | Playing
        { roomId : Evergreen.V1.RoomId.RoomId
        , userId : Evergreen.V1.Rooms.UserId
        , player : Evergreen.V1.Nonaga.Player
        , gameModel : Evergreen.V1.Nonaga.Model
        }
