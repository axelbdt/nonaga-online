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
    { clients = Dict.empty
    , rooms = Rooms.empty
    }


init : ( Model, Cmd BackendMsg )
init =
    ( initialModel
    , Cmd.none
    )


getSessionData : SessionId -> Sessions -> Maybe SessionData
getSessionData sessionId sessions =
    Dict.get sessionId sessions


retrieveUserId : SessionId -> Sessions -> Maybe UserId
retrieveUserId sessionId sessions =
    getSessionData sessionId sessions
        |> Maybe.map .userId


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            ( model, Cmd.none )

        ClientDisconnected sessionId clientId ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        ForwardGameMsg gameMsg ->
            ( model, Cmd.none )

        JoinOrCreateRoom maybeUserId roomId ->
            let
                -- TODO fix user join room
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

                userId =
                    case maybeUserId of
                        Nothing ->
                            String.fromInt (7 + Dict.size model.clients)

                        Just id ->
                            id

                newStateResult =
                    case roomToJoin.state of
                        Rooms.WaitingForPlayers clients ->
                            let
                                newClients =
                                    Set.insert userId clients
                            in
                            Rooms.WaitingForPlayers newClients
                                |> startPlayingIfReady
                                |> Ok

                        Rooms.Playing clients ->
                            if Dict.size clients < 2 then
                                let
                                    newClients =
                                        assignPlayer userId clients
                                in
                                Ok (Rooms.Playing newClients)

                            else
                                let
                                    newClients =
                                        clients
                                in
                                Err (Rooms.Playing newClients)
            in
            case newStateResult of
                Ok newState ->
                    let
                        newRoom =
                            Rooms.updateState roomToJoin newState

                        newRooms =
                            Rooms.insert newRoom model.rooms

                        newClients =
                            Dict.insert userId clientId model.clients
                    in
                    ( { model | rooms = newRooms, clients = newClients }
                    , Cmd.batch
                        [ sendToFrontend clientId
                            (JoinedRoom userId (Rooms.toFrontendRoom newRoom))
                        , updateRoomClients
                            newRoom
                            newClients
                        ]
                    )

                Err _ ->
                    ( model
                    , sendToFrontend clientId RoomFull
                    )


updateRoomClients : BackendRoom -> Clients -> Cmd BackendMsg
updateRoomClients room clients =
    broadcastToRoom room
        clients
        (UpdateRoom
            (Rooms.toFrontendRoom room)
        )


broadcastToRoom : BackendRoom -> Clients -> ToFrontend -> Cmd BackendMsg
broadcastToRoom room clients msg =
    Rooms.getUsers room.state
        |> Set.toList
        |> List.filterMap (\userId -> Dict.get userId clients)
        |> List.map (\clientId -> sendToFrontend clientId msg)
        |> Cmd.batch
