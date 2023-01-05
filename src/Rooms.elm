module Rooms exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import RoomId exposing (RoomId)


type alias Room =
    { id : RoomId, state : RoomState }


type RoomState
    = WaitingForPlayers
    | Playing


type Rooms
    = Rooms (Dict String RoomState)


empty =
    Rooms Dict.empty


updateState : RoomId -> RoomState -> Rooms -> Rooms
updateState roomId roomState rooms =
    case roomState of
        WaitingForPlayers ->
            rooms

        Playing ->
            insert roomId roomState rooms


get : RoomId -> Rooms -> Maybe RoomState
get roomId (Rooms roomsDict) =
    Dict.get (RoomId.toString roomId) roomsDict


insert : RoomId -> RoomState -> Rooms -> Rooms
insert roomId roomState (Rooms roomsDict) =
    Dict.insert (RoomId.toString roomId) roomState roomsDict
        |> Rooms


getState : RoomId -> Rooms -> RoomState
getState roomId rooms =
    case get roomId rooms of
        Nothing ->
            WaitingForPlayers

        Just roomState ->
            roomState


findClient : ClientId -> Rooms -> Maybe RoomId
findClient clientId rooms =
    Nothing
