module Evergreen.V2.Clients exposing (..)

import Dict
import Evergreen.V2.Rooms
import Lamdera


type alias Clients =
    Dict.Dict Evergreen.V2.Rooms.UserId Lamdera.ClientId
