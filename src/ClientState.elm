module ClientState exposing (..)

import Dict
import Nonaga as Game exposing (Player)
import RoomId exposing (RoomId)
import Rooms exposing (BackendRoom(..), UserId)
import Set


type ClientState
    = RoomSelection { roomIdInputText : String, roomFull : Bool }
    | ClientWaitingForPlayers { roomId : RoomId, userId : UserId, playersNeeded : Int }
    | ClientPlaying { roomId : RoomId, userId : UserId, player : Player, gameModel : Game.Model }


getUserId : ClientState -> Maybe UserId
getUserId clientState =
    case clientState of
        RoomSelection _ ->
            Nothing

        ClientWaitingForPlayers { userId } ->
            Just userId

        ClientPlaying { userId } ->
            Just userId


toClientState : UserId -> BackendRoom -> ClientState
toClientState userId room =
    let
        roomId =
            Rooms.getId room
    in
    case room of
        WaitingForPlayers state ->
            ClientWaitingForPlayers
                { roomId = roomId
                , userId = userId
                , playersNeeded = Game.playerNumber - Set.size state.users
                }

        Playing { users, gameModel } ->
            case Dict.get userId users of
                Nothing ->
                    ClientWaitingForPlayers { roomId = roomId, userId = userId, playersNeeded = 0 }

                Just player ->
                    ClientPlaying
                        { roomId = roomId
                        , userId = userId
                        , player = player
                        , gameModel = gameModel
                        }
