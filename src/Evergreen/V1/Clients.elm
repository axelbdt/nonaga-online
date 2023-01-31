module Evergreen.V1.Clients exposing (..)

import Dict
import Evergreen.V1.Rooms
import Lamdera


type alias Clients =
    Dict.Dict Evergreen.V1.Rooms.UserId Lamdera.ClientId
