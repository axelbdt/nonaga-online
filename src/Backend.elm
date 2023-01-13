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
                roomToJoin =
                    Rooms.getWithDefault roomId model.rooms

                userId =
                    case maybeUserId of
                        Nothing ->
                            String.fromInt (7 + Dict.size model.clients)

                        Just id ->
                            id

                newRoomResult =
                    Rooms.join userId roomToJoin
            in
            case newRoomResult of
                Ok newRoom ->
                    let
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
    Rooms.getUsers room
        |> Set.toList
        |> List.filterMap (\userId -> Dict.get userId clients)
        |> List.map (\clientId -> sendToFrontend clientId msg)
        |> Cmd.batch
