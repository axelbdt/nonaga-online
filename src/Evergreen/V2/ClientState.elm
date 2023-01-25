module Evergreen.V2.ClientState exposing (..)

import Evergreen.V2.Nonaga
import Evergreen.V2.RoomId
import Evergreen.V2.Rooms


type ClientState
    = RoomSelection
        { roomIdInputText : String
        , roomFull : Bool
        }
    | WaitingForPlayers
        { roomId : Evergreen.V2.RoomId.RoomId
        , userId : Evergreen.V2.Rooms.UserId
        , playersNeeded : Int
        }
    | Playing
        { roomId : Evergreen.V2.RoomId.RoomId
        , userId : Evergreen.V2.Rooms.UserId
        , player : Evergreen.V2.Nonaga.Player
        , gameModel : Evergreen.V2.Nonaga.Model
        }
