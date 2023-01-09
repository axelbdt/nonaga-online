module Rooms exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Nonaga exposing (Player(..))
import RoomId exposing (RoomId)
import Set exposing (Set)


type alias FrontendRoom =
    { id : RoomId, state : FrontendRoomState }


type alias BackendRoom =
    { id : RoomId, state : BackendRoomState }


type alias WaitingClients =
    Set ClientId


type alias PlayingClients =
    Dict ClientId Player


type BackendRoomState
    = WaitingForPlayers WaitingClients
    | Playing PlayingClients


type FrontendRoomState
    = FrontWaitingForPlayers WaitingClients
    | FrontPlaying PlayingClients


type Rooms
    = Rooms (Dict String BackendRoom)


empty =
    Rooms Dict.empty


emptyRoomClients =
    Set.empty


getClients : BackendRoomState -> WaitingClients
getClients roomState =
    case roomState of
        WaitingForPlayers clients ->
            clients

        Playing clients ->
            Dict.keys clients
                |> Set.fromList


updateState : BackendRoom -> BackendRoomState -> BackendRoom
updateState room newState =
    { room | state = newState }


clientInRoom : ClientId -> BackendRoomState -> Bool
clientInRoom clientId roomState =
    case roomState of
        WaitingForPlayers clients ->
            Set.member clientId clients

        Playing clients ->
            Dict.member clientId clients


toFrontendRoomState roomState =
    case roomState of
        WaitingForPlayers clients ->
            FrontWaitingForPlayers clients

        Playing clients ->
            FrontPlaying clients


toFrontendRoom room =
    { id = room.id, state = toFrontendRoomState room.state }


get : RoomId -> Rooms -> Maybe BackendRoom
get roomId (Rooms roomsDict) =
    Dict.get (RoomId.toString roomId) roomsDict


getWithDefault : RoomId -> Rooms -> BackendRoom
getWithDefault roomId rooms =
    case get roomId rooms of
        Nothing ->
            { id = roomId, state = WaitingForPlayers emptyRoomClients }

        Just room ->
            room


insert : BackendRoom -> Rooms -> Rooms
insert room (Rooms roomsDict) =
    Dict.insert (RoomId.toString room.id) room roomsDict
        |> Rooms


findClientRoom : ClientId -> Rooms -> Maybe BackendRoom
findClientRoom clientId (Rooms roomsDict) =
    Dict.filter (\_ { state } -> clientInRoom clientId state) roomsDict
        |> Dict.values
        |> List.head



{-
   join : ClientId -> BackendRoom -> Result String BackendRoom
   join clientId room =
       Ok room


   assignPlayer : ClientId -> PlayingClients -> Result String PlayingClients
   assignPlayer clientId clients =
       Ok clients



      assignPlayers : WaitingClients -> Result String PlayingClients
      assignPlayers clients =
-}


leave : ClientId -> Rooms -> ( Maybe BackendRoom, Rooms )
leave clientId rooms =
    case findClientRoom clientId rooms of
        Nothing ->
            ( Nothing, rooms )

        Just room ->
            let
                newState =
                    removeFromState clientId room.state

                newRoom =
                    { room | state = newState }

                newRooms =
                    insert newRoom rooms
            in
            ( Just newRoom, newRooms )


removeFromState : ClientId -> BackendRoomState -> BackendRoomState
removeFromState clientId roomState =
    case roomState of
        WaitingForPlayers clients ->
            Set.remove clientId clients
                |> WaitingForPlayers

        Playing clients ->
            Dict.remove clientId clients
                |> Playing
