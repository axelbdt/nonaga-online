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


type Rooms
    = Rooms (Dict String RoomContent)


empty =
    Rooms Dict.empty


joinOrCreate : ClientId -> RoomId -> Rooms -> ( RoomContent, Rooms )
joinOrCreate clientId roomId (Rooms roomsDict) =
    let
        roomIdString =
            RoomId.toString roomId
    in
    case Dict.get roomIdString roomsDict of
        Nothing ->
            let
                newRoom =
                    { clients = Set.singleton clientId

                    -- , gameModel = Game.initialModel
                    }

                updatedRooms =
                    Dict.insert roomIdString newRoom roomsDict
                        |> Rooms
            in
            ( newRoom, updatedRooms )

        Just room ->
            let
                updatedRoom =
                    { room | clients = Set.insert clientId room.clients }

                updatedRooms =
                    Dict.insert roomIdString updatedRoom roomsDict
                        |> Rooms
            in
            ( updatedRoom, updatedRooms )
