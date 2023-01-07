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
    = FrontWaitingForPlayers
    | FrontPlaying


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


clientInRoom : ClientId -> BackendRoomState -> Bool
clientInRoom clientId roomState =
    case roomState of
        WaitingForPlayers clients ->
            Set.member clientId clients

        Playing clients ->
            Dict.member clientId clients


toFrontendRoomState roomState =
    case roomState of
        WaitingForPlayers _ ->
            FrontWaitingForPlayers

        Playing _ ->
            FrontPlaying


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


joinOrCreate : ClientId -> RoomId -> Rooms -> Rooms
joinOrCreate clientId roomId rooms =
    let
        room =
            getWithDefault roomId rooms

        newState =
            case join clientId room of
                Ok { state } ->
                    state

                Err _ ->
                    room.state

        newRoom =
            { room | state = newState }
    in
    insert newRoom rooms


join : ClientId -> BackendRoom -> Result BackendRoom BackendRoom
join clientId room =
    case room.state of
        WaitingForPlayers clients ->
            let
                newClients =
                    Set.insert clientId clients

                newState =
                    if Set.size newClients > 1 then
                        Playing (assignPlayers newClients)

                    else
                        WaitingForPlayers newClients
            in
            Ok { room | state = newState }

        Playing clients ->
            let
                newClients =
                    assignPlayer clientId clients
            in
            Ok { room | state = Playing newClients }


assignPlayer : ClientId -> PlayingClients -> PlayingClients
assignPlayer clientId clients =
    if Dict.size clients < 2 then
        case
            [ Red, Black ]
                |> List.filter (\p -> List.any (Nonaga.playerEquals p) (Dict.values clients))
                |> List.head
        of
            Nothing ->
                clients

            Just player ->
                Dict.insert clientId player clients

    else
        clients


assignPlayers : WaitingClients -> PlayingClients
assignPlayers clients =
    clients
        |> Set.toList
        |> List.foldl assignPlayer Dict.empty


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
