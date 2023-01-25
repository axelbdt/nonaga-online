module ClientState exposing (..)

import Dict
import Nonaga as Game exposing (Player)
import RoomId exposing (RoomId)
import Rooms exposing (Room(..), UserId)
import Set


type ClientState
    = RoomSelection { roomIdInputText : String, roomFull : Bool }
    | ClientWaitingForPlayers { roomId : RoomId, userId : UserId, playersNeeded : Int }
    | ClientPlaying { roomId : RoomId, userId : UserId, player : Player, gameModel : Game.Model }


roomSelectionInitialState : ClientState
roomSelectionInitialState =
    RoomSelection { roomIdInputText = "", roomFull = False }


getUserId : ClientState -> Maybe UserId
getUserId clientState =
    case clientState of
        RoomSelection _ ->
            Nothing

        ClientWaitingForPlayers { userId } ->
            Just userId

        ClientPlaying { userId } ->
            Just userId


getRoomId : ClientState -> Maybe RoomId
getRoomId state =
    case state of
        RoomSelection _ ->
            Nothing

        ClientWaitingForPlayers { roomId } ->
            Just roomId

        ClientPlaying { roomId } ->
            Just roomId


toClientState : UserId -> Room -> Result String ClientState
toClientState userId room =
    let
        roomId =
            Rooms.getId room
    in
    case room of
        WaitingForPlayers state ->
            Ok
                (ClientWaitingForPlayers
                    { roomId = roomId
                    , userId = userId
                    , playersNeeded = Game.playerNumber - Set.size state.users
                    }
                )

        Playing { users, gameModel } ->
            case Dict.get userId users of
                Nothing ->
                    Err "Client not found in room"

                Just player ->
                    Ok
                        (ClientPlaying
                            { roomId = roomId
                            , userId = userId
                            , player = player
                            , gameModel = gameModel
                            }
                        )
