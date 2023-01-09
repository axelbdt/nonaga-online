module Backend exposing (..)

-- import Nonaga as Game

import Dict
import Lamdera exposing (ClientId, SessionId, onConnect, onDisconnect, sendToFrontend)
import Nonaga exposing (Player(..))
import Rooms
import Set
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch [ onConnect ClientConnected, onDisconnect ClientDisconnected ]


initialModel : BackendModel
initialModel =
    { rooms = Rooms.empty
    }


init : ( Model, Cmd BackendMsg )
init =
    ( initialModel
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            ( model, Cmd.none )

        ClientDisconnected sessionId clientId ->
            case
                Rooms.leave clientId model.rooms
            of
                ( Nothing, _ ) ->
                    ( model, Cmd.none )

                ( Just newRoom, newRooms ) ->
                    ( { model | rooms = newRooms }
                    , updateClientsRoom newRoom
                    )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        ForwardGameMsg gameMsg ->
            ( model, Cmd.none )

        JoinOrCreateRoom roomId ->
            let
                assignPlayer aClientId assignedClients =
                    case
                        [ Red, Black ]
                            |> List.filter (\p -> not (List.any (Nonaga.playerEquals p) (Dict.values assignedClients)))
                            |> List.head
                    of
                        Nothing ->
                            assignedClients

                        Just player ->
                            Dict.insert aClientId player assignedClients

                startPlayingIfReady roomState =
                    case roomState of
                        Rooms.Playing _ ->
                            roomState

                        Rooms.WaitingForPlayers clients ->
                            if Set.size clients == 2 then
                                let
                                    assignedClients =
                                        clients
                                            |> Set.toList
                                            |> List.foldl assignPlayer Dict.empty
                                in
                                Rooms.Playing assignedClients

                            else
                                Rooms.WaitingForPlayers clients

                roomToJoin =
                    Rooms.getWithDefault roomId model.rooms

                newStateResult =
                    case roomToJoin.state of
                        Rooms.WaitingForPlayers clients ->
                            let
                                newClients =
                                    Set.insert clientId clients
                            in
                            Rooms.WaitingForPlayers newClients
                                |> startPlayingIfReady
                                |> Ok

                        Rooms.Playing clients ->
                            if Dict.size clients < 2 then
                                let
                                    newClients =
                                        assignPlayer clientId clients
                                in
                                Ok (Rooms.Playing newClients)

                            else
                                let
                                    newClients =
                                        clients
                                in
                                Err (Rooms.Playing newClients)

                newRoomResult =
                    case newStateResult of
                        Ok newState ->
                            Ok (Rooms.updateState roomToJoin newState)

                        Err newState ->
                            Err (Rooms.updateState roomToJoin newState)

                newRooms =
                    case newRoomResult of
                        Ok newRoom ->
                            Rooms.insert newRoom model.rooms

                        Err _ ->
                            model.rooms

                newModel =
                    { model | rooms = newRooms }

                command =
                    case newRoomResult of
                        Ok newRoom ->
                            Cmd.batch
                                [ sendToFrontend clientId
                                    (JoinedRoom (Rooms.toFrontendRoom newRoom))
                                , updateClientsRoom
                                    newRoom
                                ]

                        Err _ ->
                            sendToFrontend clientId RoomFull
            in
            ( newModel
            , command
            )


updateClientsRoom : BackendRoom -> Cmd BackendMsg
updateClientsRoom room =
    broadcastToRoomClients (Rooms.getClients room.state)
        (UpdateRoom
            (Rooms.toFrontendRoom room)
        )


broadcastToRoomClients roomClients msg =
    roomClients
        |> Set.toList
        |> List.map (\aClientId -> sendToFrontend aClientId msg)
        |> Cmd.batch
