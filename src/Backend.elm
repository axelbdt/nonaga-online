module Backend exposing (..)

-- import Nonaga as Game

import Dict
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


initialModel =
    { rooms = Rooms.empty }


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
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        ForwardGameMsg gameMsg ->
            ( model, Cmd.none )

        JoinOrCreateRoom roomId ->
            let
                ( joinedRoomContent, rooms ) =
                    Rooms.joinOrCreate clientId roomId model.rooms

                newModel =
                    { model | rooms = rooms }
            in
            ( newModel
            , Cmd.batch
                [ sendToFrontend clientId (JoinedRoom roomId)
                , broadcastBackendModel newModel
                ]
            )


broadcastBackendModel backendModel =
    Lamdera.broadcast (UpdateBackendModel backendModel)
