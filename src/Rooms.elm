module Rooms exposing (..)

import Dict exposing (Dict)
import Nonaga as Game exposing (Player(..))
import RoomId exposing (RoomId)
import Set exposing (Set)


type alias UserId =
    String


type Room
    = WaitingForPlayers { id : RoomId, users : Set UserId }
    | Playing { id : RoomId, users : Dict UserId Player, gameModel : Game.Model }


type Rooms
    = Rooms (Dict String Room)


empty =
    Rooms Dict.empty


getId : Room -> RoomId
getId room =
    case room of
        WaitingForPlayers { id } ->
            id

        Playing { id } ->
            id


getUsers : Room -> Set UserId
getUsers room =
    case room of
        WaitingForPlayers { users } ->
            users

        Playing { users } ->
            Dict.keys users
                |> Set.fromList


userInRoom : UserId -> Room -> Bool
userInRoom userId room =
    case room of
        WaitingForPlayers { users } ->
            Set.member userId users

        Playing { users } ->
            Dict.member userId users


get : RoomId -> Rooms -> Maybe Room
get roomId (Rooms roomsDict) =
    Dict.get (RoomId.toString roomId) roomsDict


getWithDefault : RoomId -> Rooms -> Room
getWithDefault roomId rooms =
    case get roomId rooms of
        Nothing ->
            WaitingForPlayers { id = roomId, users = Set.empty }

        Just room ->
            room


insert : Room -> Rooms -> Rooms
insert room (Rooms roomsDict) =
    Dict.insert (RoomId.toString (getId room)) room roomsDict
        |> Rooms


remove : Room -> Rooms -> Rooms
remove room (Rooms roomsDict) =
    Dict.remove (RoomId.toString (getId room)) roomsDict
        |> Rooms


isEmpty : Room -> Bool
isEmpty room =
    case room of
        WaitingForPlayers { users } ->
            Set.size users == 0

        Playing { users } ->
            Dict.size users == 0


findUserRoom : UserId -> Rooms -> Maybe Room
findUserRoom userId (Rooms roomsDict) =
    roomsDict
        |> Dict.values
        |> List.filter (userInRoom userId)
        |> List.head


leave : UserId -> Rooms -> ( Maybe Room, Rooms )
leave userId rooms =
    case findUserRoom userId rooms of
        Nothing ->
            ( Nothing, rooms )

        Just room ->
            let
                newRoom =
                    case room of
                        WaitingForPlayers state ->
                            WaitingForPlayers { state | users = Set.remove userId state.users }

                        Playing state ->
                            Playing { state | users = Dict.remove userId state.users }

                newRooms =
                    if isEmpty newRoom then
                        remove newRoom rooms

                    else
                        insert newRoom rooms
            in
            ( Just newRoom, newRooms )


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
