module Rooms exposing (..)

import Dict exposing (Dict)
import Nonaga as Game exposing (Player(..))
import RoomId exposing (RoomId)
import Set exposing (Set)


type alias UserId =
    String


type BackendRoom
    = WaitingForPlayers { id : RoomId, users : Set UserId }
    | Playing { id : RoomId, users : Dict UserId Player, gameModel : Game.Model }


type Rooms
    = Rooms (Dict String BackendRoom)


empty =
    Rooms Dict.empty


emptyRoomUsers =
    Set.empty


getId : BackendRoom -> RoomId
getId room =
    case room of
        WaitingForPlayers { id } ->
            id

        Playing { id } ->
            id


getUsers : BackendRoom -> Set UserId
getUsers room =
    case room of
        WaitingForPlayers { users } ->
            users

        Playing { users } ->
            Dict.keys users
                |> Set.fromList


userInRoom : UserId -> BackendRoom -> Bool
userInRoom userId room =
    case room of
        WaitingForPlayers { users } ->
            Set.member userId users

        Playing { users } ->
            Dict.member userId users


get : RoomId -> Rooms -> Maybe BackendRoom
get roomId (Rooms roomsDict) =
    Dict.get (RoomId.toString roomId) roomsDict


getWithDefault : RoomId -> Rooms -> BackendRoom
getWithDefault roomId rooms =
    case get roomId rooms of
        Nothing ->
            WaitingForPlayers { id = roomId, users = emptyRoomUsers }

        Just room ->
            room


insert : BackendRoom -> Rooms -> Rooms
insert room (Rooms roomsDict) =
    Dict.insert (RoomId.toString (getId room)) room roomsDict
        |> Rooms


findUserRoom : UserId -> Rooms -> Maybe BackendRoom
findUserRoom userId (Rooms roomsDict) =
    roomsDict
        |> Dict.values
        |> List.filter (userInRoom userId)
        |> List.head


leave : UserId -> Rooms -> ( Maybe BackendRoom, Rooms )
leave userId rooms =
    case findUserRoom userId rooms of
        Nothing ->
            ( Nothing, rooms )

        Just room ->
            let
                newRoom =
                    removeFromRoom userId room

                newRooms =
                    insert newRoom rooms
            in
            ( Just (Debug.log "Leave Room" newRoom), newRooms )


removeFromRoom : UserId -> BackendRoom -> BackendRoom
removeFromRoom userId room =
    case room of
        WaitingForPlayers state ->
            WaitingForPlayers { state | users = Set.remove userId state.users }

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


startPlayingIfReady room =
    case room of
        Playing _ ->
            room

        WaitingForPlayers state ->
            if Set.size state.users == Game.playerNumber then
                let
                    assignedClients =
                        state.users
                            |> Set.toList
                            |> List.foldl assignPlayer Dict.empty
                in
                Playing
                    { id = state.id
                    , users = assignedClients
                    , gameModel = Game.initialModel
                    }

            else
                room


join userId roomToJoin =
    case roomToJoin of
        WaitingForPlayers state ->
            let
                newUsers =
                    Set.insert userId state.users

                newRoom =
                    WaitingForPlayers { state | users = newUsers }
                        |> startPlayingIfReady
            in
            Ok newRoom

        Playing state ->
            if Dict.size state.users < 2 then
                let
                    newUsers =
                        assignPlayer userId state.users

                    newRoom =
                        Playing { state | users = newUsers }
                in
                Ok newRoom

            else
                Err roomToJoin


handleGameMsg userId gameMsg room =
    case room of
        WaitingForPlayers _ ->
            room

        Playing state ->
            if
                Dict.get userId state.users
                    |> Maybe.map (Game.playerEquals state.gameModel.currentPlayer)
                    |> Maybe.withDefault False
            then
                Playing { state | gameModel = Game.update gameMsg state.gameModel }

            else
                room
