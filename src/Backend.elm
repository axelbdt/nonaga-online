module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId, broadcast, onConnect, onDisconnect, sendToFrontend)
import Nonaga as Game
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


init : ( Model, Cmd BackendMsg )
init =
    ( { rooms = Dict.empty }
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
                ( joinedRoom, rooms ) =
                    joinOrCreateRoom clientId roomId model.rooms
            in
            ( { model | rooms = rooms }
            , sendToFrontend clientId (JoinedRoom roomId joinedRoom)
            )


joinOrCreateRoom clientId roomId rooms =
    case Dict.get roomId rooms of
        Nothing ->
            let
                newRoom =
                    { clients = Set.singleton clientId, gameModel = Game.initialModel }

                updatedRooms =
                    Dict.insert roomId newRoom rooms
            in
            ( newRoom, updatedRooms )

        Just room ->
            let
                updatedRoom =
                    { room | clients = Set.insert clientId room.clients }

                updatedRooms =
                    Dict.insert roomId updatedRoom rooms
            in
            ( updatedRoom, updatedRooms )
