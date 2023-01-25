module ClientState exposing (..)

import Dict
import Nonaga as Game exposing (Player)
import RoomId exposing (RoomId)
import Rooms exposing (Room, UserId)
import Set


type ClientState
    = RoomSelection { roomIdInputText : String, roomFull : Bool }
    | WaitingForPlayers { roomId : RoomId, userId : UserId, playersNeeded : Int }
    | Playing { roomId : RoomId, userId : UserId, player : Player, gameModel : Game.Model }


roomSelectionInitialState : ClientState
roomSelectionInitialState =
    RoomSelection { roomIdInputText = "", roomFull = False }


getUserId : ClientState -> Maybe UserId
getUserId clientState =
    case clientState of
        RoomSelection _ ->
            Nothing

        WaitingForPlayers { userId } ->
            Just userId

        Playing { userId } ->
            Just userId


getRoomId : ClientState -> Maybe RoomId
getRoomId state =
    case state of
        RoomSelection _ ->
            Nothing

        WaitingForPlayers { roomId } ->
            Just roomId

        Playing { roomId } ->
            Just roomId


fromRoom : UserId -> Room -> Result String ClientState
fromRoom userId room =
    let
        roomId =
            Rooms.getId room
    in
    case room of
        Rooms.WaitingForPlayers state ->
            Ok
                (WaitingForPlayers
                    { roomId = roomId
                    , userId = userId
                    , playersNeeded = Game.playerNumber - Set.size state.users
                    }
                )

        Rooms.Playing { users, gameModel } ->
            case Dict.get userId users of
                Nothing ->
                    Err "Client not found in room"

                Just player ->
                    Ok
                        (Playing
                            { roomId = roomId
                            , userId = userId
                            , player = player
                            , gameModel = gameModel
                            }
                        )
