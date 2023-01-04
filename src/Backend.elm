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
    { clientRooms = Rooms.emptyClientRooms }


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
            case Rooms.leave clientId model.clientRooms of
                ( Nothing, newClientRooms ) ->
                    ( model, Cmd.none )

                ( Just roomId, newClientRooms ) ->
                    let
                        roomClients =
                            Rooms.roomClients roomId newClientRooms

                        newModel =
                            { model | clientRooms = newClientRooms }
                    in
                    ( newModel
                    , updateRoomClients roomClients
                    )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        ForwardGameMsg gameMsg ->
            ( model, Cmd.none )

        JoinOrCreateRoom roomId ->
            let
                newClientRooms =
                    Rooms.joinOrCreate clientId roomId model.clientRooms

                roomClients =
                    Rooms.roomClients roomId newClientRooms

                newModel =
                    { model | clientRooms = newClientRooms }
            in
            ( newModel
            , Cmd.batch
                [ sendToFrontend clientId (JoinedRoom roomId roomClients)
                , updateRoomClients roomClients
                ]
            )


updateRoomClients roomClients =
    broadcastToRoomClients roomClients (UpdateRoomClients roomClients)


broadcastToRoomClients roomClients msg =
    roomClients
        |> Set.toList
        |> List.map (\aClientId -> sendToFrontend aClientId msg)
        |> Cmd.batch
