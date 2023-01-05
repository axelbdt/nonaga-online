module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
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
        , subscriptions = \_ -> Sub.map GameWidgetMsg GraphicWidget.subscriptions
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
      , room = Nothing
      , gameModel = Game.initialModel
      , gameWidgetState = gameWidgetState
      , roomIdInputText = ""
      }
    , Cmd.batch
        [ Cmd.map GameWidgetMsg gameWidgetCommand
        , case maybeRoomId of
            Nothing ->
                Cmd.none

            Just roomId ->
                sendToBackend (JoinOrCreateRoom roomId)
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
                        , sendToBackend (JoinOrCreateRoom (RoomId.parse url.path))
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
            ( model, sendToBackend (ForwardGameMsg gameMsg) )

        GameWidgetMsg gameWidgetMessage ->
            let
                ( newWidgetState, widgetCommand ) =
                    GraphicWidget.update gameWidgetMessage model.gameWidgetState
            in
            ( { model | gameWidgetState = newWidgetState }, Cmd.map GameWidgetMsg widgetCommand )

        SetRoomIdInputText inputText ->
            ( { model | roomIdInputText = inputText }
            , Cmd.none
            )

        SubmitRoomId ->
            ( model
            , sendToBackend
                (JoinOrCreateRoom (RoomId.parse model.roomIdInputText))
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        UpdateGameModel gameModel ->
            ( { model | gameModel = gameModel }, Cmd.none )

        JoinedRoom roomId roomState ->
            ( { model | room = Just { id = roomId, state = roomState } }
            , Nav.pushUrl model.key (RoomId.toString roomId)
            )

        UpdateRoomClients roomClients ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout []
            (case model.room of
                Nothing ->
                    Components.joinRoomForm SubmitRoomId model.roomIdInputText

                Just room ->
                    let
                        message =
                            case room.state of
                                Rooms.WaitingForPlayers ->
                                    "Waiting for players"

                                Rooms.Playing ->
                                    "Playing"
                    in
                    Element.text message
             -- [ GraphicWidget.view model.gameWidgetState (Game.view model.gameModel) ]
            )
        ]
    }
