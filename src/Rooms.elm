module Rooms exposing (..)

import Dict exposing (Dict)
import Nonaga as Game exposing (Player(..))
import RoomId exposing (RoomId)
import Set exposing (Set)


type alias FrontendRoom =
    { id : RoomId, userId : UserId, state : FrontendRoomState }


type alias UserId =
    String


type alias BackendRoom =
    { id : RoomId, state : BackendRoomState }


type BackendRoomState
    = WaitingForPlayers (Set UserId)
    | Playing { users : Dict UserId Player, gameModel : Game.Model }


type FrontendRoomState
    = FrontWaitingForPlayers Int
    | FrontPlaying { player : Player, gameModel : Game.Model }


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

        Playing { users } ->
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

        Playing { users } ->
            Dict.member userId users


toFrontendRoomState userId roomState =
    case roomState of
        WaitingForPlayers users ->
            FrontWaitingForPlayers (gamePlayerNumber - Set.size users)

        Playing { users, gameModel } ->
            case Dict.get userId users of
                Nothing ->
                    Debug.log "TODO" FrontWaitingForPlayers 0

                Just player ->
                    FrontPlaying { player = player, gameModel = gameModel }


toFrontendRoom userId room =
    { id = room.id, userId = userId, state = toFrontendRoomState userId room.state }


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

        Playing state ->
            Playing { state | users = Dict.remove userId state.users }


assignPlayer aUserId assignedUsers =
    case
        [ Red, Black ]
            |> List.filter (\p -> not (List.any (Game.playerEquals p) (Dict.values assignedUsers)))
            |> List.head
    of
        Nothing ->
            assignedUsers

        Just player ->
            Dict.insert aUserId player assignedUsers


gamePlayerNumber : Int
gamePlayerNumber =
    2


startPlayingIfReady roomState =
    case roomState of
        Playing _ ->
            roomState

        WaitingForPlayers clients ->
            if Set.size clients == gamePlayerNumber then
                let
                    assignedClients =
                        clients
                            |> Set.toList
                            |> List.foldl assignPlayer Dict.empty
                in
                Playing { users = assignedClients, gameModel = Game.initialModel }

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

        Playing state ->
            if Dict.size state.users < 2 then
                let
                    newUsers =
                        assignPlayer userId state.users

                    newState =
                        Playing { state | users = newUsers }
                in
                updateState roomToJoin newState
                    |> Ok

            else
                Err roomToJoin
