module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import ClientState exposing (ClientState(..))
import Components
import Element exposing (..)
import GraphicSVG.Widget as GraphicWidget
import Lamdera exposing (sendToBackend)
import Nonaga as Game
import RoomId as RoomId
import Types exposing (..)
import Url
import Url.Parser as Parser


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


subscriptions model =
    Sub.batch
        [ Sub.map GameWidgetMsg GraphicWidget.subscriptions
        ]


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        ( gameWidgetState, gameWidgetCommand ) =
            GraphicWidget.init 3000 1000 "gameWidget"

        maybeRoomId =
            parseRoomId url
    in
    ( { key = key
      , state =
            ClientState.RoomSelection
                { roomIdInputText =
                    maybeRoomId
                        |> Maybe.map RoomId.toString
                        |> Maybe.withDefault ""
                , roomFull = False
                }
      , gameWidgetState = gameWidgetState
      }
    , Cmd.batch
        [ Cmd.map GameWidgetMsg gameWidgetCommand
        , case maybeRoomId of
            Nothing ->
                Cmd.none

            Just roomId ->
                sendToBackend (JoinOrCreateRoom Nothing roomId)
        ]
    )


parseRoomId : Url.Url -> Maybe RoomId
parseRoomId url =
    Parser.parse Parser.string url
        |> Maybe.map RoomId.parse


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch
                        [ Nav.pushUrl model.key (Url.toString url)
                        , Debug.log "TODO: enter room by url click" Cmd.none
                        ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model
            , Cmd.none
            )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GameMsg gameMsg ->
            case model.state of
                RoomSelection _ ->
                    ( model, Cmd.none )

                ClientWaitingForPlayers _ ->
                    ( model, Cmd.none )

                ClientPlaying { roomId, userId } ->
                    ( model, sendToBackend (ForwardGameMsg { roomId = roomId, userId = userId, gameMsg = gameMsg }) )

        GameWidgetMsg gameWidgetMessage ->
            let
                ( newWidgetState, widgetCommand ) =
                    GraphicWidget.update gameWidgetMessage model.gameWidgetState
            in
            ( { model | gameWidgetState = newWidgetState }, Cmd.map GameWidgetMsg widgetCommand )

        SetRoomIdInputText inputText ->
            let
                newState =
                    ClientState.RoomSelection { roomIdInputText = inputText, roomFull = False }
            in
            ( { model | state = newState }
            , Cmd.none
            )

        SubmitRoomId ->
            case model.state of
                ClientState.RoomSelection { roomIdInputText } ->
                    ( model
                    , sendToBackend
                        (JoinOrCreateRoom (ClientState.getUserId model.state) (RoomId.parse roomIdInputText))
                    )

                _ ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        JoinedRoom clientState ->
            let
                commands =
                    case ClientState.getRoomId clientState of
                        Nothing ->
                            Cmd.none

                        Just roomId ->
                            Nav.pushUrl model.key (RoomId.toString roomId)
            in
            ( { model | state = clientState }
            , commands
            )

        UpdateRoom clientState ->
            case model.state of
                ClientState.RoomSelection _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        newModel =
                            { model | state = clientState }
                    in
                    ( newModel, Cmd.none )

        RoomFull ->
            let
                newState =
                    case model.state of
                        ClientState.RoomSelection state ->
                            ClientState.RoomSelection { state | roomFull = True }

                        _ ->
                            model.state
            in
            ( { model | state = newState }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout [ Element.width fill ]
            (case model.state of
                ClientState.RoomSelection { roomIdInputText, roomFull } ->
                    Components.joinRoomForm SubmitRoomId roomIdInputText roomFull

                ClientState.ClientWaitingForPlayers { playersNeeded } ->
                    let
                        message =
                            "Waiting for players: " ++ String.fromInt playersNeeded ++ " more needed"
                    in
                    Element.text message

                ClientState.ClientPlaying { player, gameModel } ->
                    Element.column [ Element.width fill ]
                        [ let
                            message =
                                "Playing as " ++ Game.playerText player
                          in
                          Element.text message
                        , Element.map GameMsg
                            (Element.html
                                (GraphicWidget.view model.gameWidgetState (Game.view gameModel))
                            )
                        ]
            )
        ]
    }
