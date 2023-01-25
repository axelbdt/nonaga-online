module Clients exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Rooms exposing (UserId)


type alias Clients =
    Dict UserId ClientId


findUserId : ClientId -> Clients -> Maybe UserId
findUserId clientId clients =
    clients
        |> Dict.filter (\userId aClientId -> aClientId == clientId)
        |> Dict.keys
        |> List.head
