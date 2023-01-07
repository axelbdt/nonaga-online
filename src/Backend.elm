module Backend exposing (..)

-- import Nonaga as Game

import Lamdera exposing (ClientId, SessionId, onConnect, onDisconnect, sendToFrontend)
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
                joinResult =
                    Rooms.getWithDefault roomId model.rooms
                        |> Rooms.join clientId
            in
            case joinResult of
                Ok room ->
                    let
                        newRooms =
                            Rooms.insert room model.rooms
                    in
                    ( { model | rooms = newRooms }
                    , Cmd.batch
                        [ sendToFrontend clientId
                            (JoinedRoom (Rooms.toFrontendRoom room))
                        , updateClientsRoom
                            room
                        ]
                    )

                Err _ ->
                    ( model
                    , sendToFrontend clientId RoomFull
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
