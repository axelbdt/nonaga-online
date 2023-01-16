module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import ClientState
import Components
import Element exposing (..)
import GraphicSVG.Widget as GraphicWidget
import Lamdera exposing (sendToBackend)
import Nonaga as Game
import RoomId as RoomId
import Rooms
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
        , subscriptions = \_ -> Sub.none -- \_ -> Sub.map GameWidgetMsg GraphicWidget.subscriptions
        , view = view
        }


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
                { roomIdInputText = ""
                , roomFull = False
                }

      -- , gameModel = Game.initialModel
      -- , gameWidgetState = gameWidgetState
      }
    , Cmd.batch
        [ Cmd.none -- Cmd.map GameWidgetMsg gameWidgetCommand
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

        {-
           GameMsg gameMsg ->
               ( model, sendToBackend (ForwardGameMsg gameMsg) )

           GameWidgetMsg gameWidgetMessage ->
               let
                   ( newWidgetState, widgetCommand ) =
                       GraphicWidget.update gameWidgetMessage model.gameWidgetState
               in
               ( { model | gameWidgetState = newWidgetState }, Cmd.map GameWidgetMsg widgetCommand )
        -}
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
        UpdateGameModel gameModel ->
            ( model, Cmd.none )

        JoinedRoom clientState ->
            ( { model | state = clientState }
            , Nav.pushUrl model.key (Debug.log "TODO: RoomId in the url, proper message" "plop")
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
        [ Element.layout []
            (case model.state of
                ClientState.RoomSelection { roomIdInputText, roomFull } ->
                    Components.joinRoomForm SubmitRoomId roomIdInputText roomFull

                ClientState.ClientWaitingForPlayers { playersNeeded } ->
                    let
                        message =
                            "Waiting for players: " ++ String.fromInt playersNeeded ++ " more needed"
                    in
                    Element.text message

                ClientState.ClientPlaying { player } ->
                    let
                        message =
                            "Playing as " ++ Game.playerText player
                    in
                    Element.text message
             -- [ GraphicWidget.view model.gameWidgetState (Game.view model.gameModel) ]
            )
        ]
    }
