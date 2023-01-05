module ClientRooms exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import RoomId exposing (RoomId)
import Set exposing (Set)


type alias RoomClients =
    Set ClientId


type alias ClientRooms =
    Dict ClientId RoomId


emptyClientRooms =
    Dict.empty


getClientRoomId : ClientId -> ClientRooms -> Maybe RoomId
getClientRoomId clientId clientRooms =
    Dict.get clientId clientRooms


roomClients : RoomId -> ClientRooms -> RoomClients
roomClients roomId clientRooms =
    clientRooms
        |> Dict.filter (\_ v -> RoomId.equal roomId v)
        |> Dict.keys
        |> Set.fromList


joinOrCreate : ClientId -> RoomId -> ClientRooms -> ClientRooms
joinOrCreate clientId roomId clientRooms =
    Dict.insert clientId roomId clientRooms


leave : ClientId -> ClientRooms -> ClientRooms
leave clientId clientRooms =
    Dict.remove clientId clientRooms
