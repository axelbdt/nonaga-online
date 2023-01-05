module Rooms exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import RoomId exposing (RoomId)
import Set exposing (Set)


type alias Room =
    { id : RoomId, content : RoomContent }


type alias RoomContent =
    { clients : Set ClientId

    -- , gameModel : GameModel
    }


type alias RoomClients =
    Set ClientId


type Rooms
    = Rooms (Dict String RoomContent)


type alias ClientRooms =
    Dict ClientId RoomId


emptyClientRooms =
    Dict.empty


roomClients : RoomId -> ClientRooms -> RoomClients
roomClients roomId clientRooms =
    clientRooms
        |> Dict.filter (\_ v -> RoomId.equal roomId v)
        |> Dict.keys
        |> Set.fromList


empty =
    Rooms Dict.empty


get : ClientId -> ClientRooms -> Maybe RoomId
get clientId clientRooms =
    Dict.get clientId clientRooms


findClient : ClientId -> Rooms -> Maybe RoomId
findClient clientId rooms =
    Nothing


clientInRoom : ClientId -> RoomContent -> Bool
clientInRoom clientId roomContent =
    Set.member clientId roomContent.clients


joinOrCreate : ClientId -> RoomId -> ClientRooms -> ClientRooms
joinOrCreate clientId roomId clientRooms =
    Dict.insert clientId roomId clientRooms


leave : ClientId -> ClientRooms -> ClientRooms
leave clientId clientRooms =
    Dict.remove clientId clientRooms


removeClient : ClientId -> RoomContent -> ( RoomContent, Bool )
removeClient clientId roomContent =
    ( { roomContent | clients = Set.remove clientId roomContent.clients }
    , Set.member clientId roomContent.clients
    )
