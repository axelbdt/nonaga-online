module Rooms exposing (..)

import Dict exposing (Dict)
import Nonaga exposing (Player(..))
import RoomId exposing (RoomId)
import Set exposing (Set)


type alias FrontendRoom =
    { id : RoomId, state : FrontendRoomState }


type alias UserId =
    String


type alias BackendRoom =
    { id : RoomId, state : BackendRoomState }


type BackendRoomState
    = WaitingForPlayers (Set UserId)
    | Playing (Dict UserId Player)


type FrontendRoomState
    = FrontWaitingForPlayers (Set UserId)
    | FrontPlaying (Dict UserId Player)


type Rooms
    = Rooms (Dict String BackendRoom)


empty =
    Rooms Dict.empty


emptyRoomUsers =
    Set.empty


getUsers : BackendRoom -> Set UserId
getUsers room =
    case room.state of
        WaitingForPlayers users ->
            users

        Playing users ->
            Dict.keys users
                |> Set.fromList


updateState : BackendRoom -> BackendRoomState -> BackendRoom
updateState room newState =
    { room | state = newState }


userInRoom : UserId -> BackendRoomState -> Bool
userInRoom userId roomState =
    case roomState of
        WaitingForPlayers users ->
            Set.member userId users

        Playing users ->
            Dict.member userId users


toFrontendRoomState roomState =
    case roomState of
        WaitingForPlayers users ->
            FrontWaitingForPlayers users

        Playing users ->
            FrontPlaying users


toFrontendRoom room =
    { id = room.id, state = toFrontendRoomState room.state }


get : RoomId -> Rooms -> Maybe BackendRoom
get roomId (Rooms roomsDict) =
    Dict.get (RoomId.toString roomId) roomsDict


getWithDefault : RoomId -> Rooms -> BackendRoom
getWithDefault roomId rooms =
    case get roomId rooms of
        Nothing ->
            { id = roomId, state = WaitingForPlayers emptyRoomUsers }

        Just room ->
            room


insert : BackendRoom -> Rooms -> Rooms
insert room (Rooms roomsDict) =
    Dict.insert (RoomId.toString room.id) room roomsDict
        |> Rooms


findUserRoom : UserId -> Rooms -> Maybe BackendRoom
findUserRoom userId (Rooms roomsDict) =
    Dict.filter (\_ { state } -> userInRoom userId state) roomsDict
        |> Dict.values
        |> List.head


leave : UserId -> Rooms -> ( Maybe BackendRoom, Rooms )
leave userId rooms =
    case findUserRoom userId rooms of
        Nothing ->
            ( Nothing, rooms )

        Just room ->
            let
                newState =
                    removeFromState userId room.state

                newRoom =
                    { room | state = newState }

                newRooms =
                    insert newRoom rooms
            in
            ( Just newRoom, newRooms )


removeFromState : UserId -> BackendRoomState -> BackendRoomState
removeFromState userId roomState =
    case roomState of
        WaitingForPlayers users ->
            Set.remove userId users
                |> WaitingForPlayers

        Playing users ->
            Dict.remove userId users
                |> Playing


assignPlayer aUserId assignedUsers =
    case
        [ Red, Black ]
            |> List.filter (\p -> not (List.any (Nonaga.playerEquals p) (Dict.values assignedUsers)))
            |> List.head
    of
        Nothing ->
            assignedUsers

        Just player ->
            Dict.insert aUserId player assignedUsers


startPlayingIfReady roomState =
    case roomState of
        Playing _ ->
            roomState

        WaitingForPlayers clients ->
            if Set.size clients == 2 then
                let
                    assignedClients =
                        clients
                            |> Set.toList
                            |> List.foldl assignPlayer Dict.empty
                in
                Playing assignedClients

            else
                WaitingForPlayers clients


join userId roomToJoin =
    case roomToJoin.state of
        WaitingForPlayers users ->
            let
                newUsers =
                    Set.insert userId users

                newState =
                    WaitingForPlayers newUsers
                        |> startPlayingIfReady
            in
            updateState roomToJoin newState
                |> Ok

        Playing users ->
            if Dict.size users < 2 then
                let
                    newUsers =
                        assignPlayer userId users

                    newState =
                        Playing newUsers
                in
                updateState roomToJoin newState
                    |> Ok

            else
                Err roomToJoin
