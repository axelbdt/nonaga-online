module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Components
import Element exposing (..)
import GraphicSVG.Widget as GraphicWidget
import Lamdera exposing (sendToBackend)
import Nonaga as Game
import RoomId as RoomId
import Rooms exposing (RoomClientState(..))
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
      , room = None
      , gameModel = Game.initialModel
      , gameWidgetState = gameWidgetState
      , roomIdInputText = ""
      , roomFull = False
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
                        , sendToBackend
                            (JoinOrCreateRoom (Rooms.getUserId model.room)
                                (RoomId.parse url.path)
                            )
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
            ( { model | roomIdInputText = inputText, roomFull = False }
            , Cmd.none
            )

        SubmitRoomId ->
            ( model
            , sendToBackend
                (JoinOrCreateRoom (Rooms.getUserId model.room) (RoomId.parse model.roomIdInputText))
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        UpdateGameModel gameModel ->
            ( { model | gameModel = gameModel }, Cmd.none )

        JoinedRoom userId room ->
            ( { model | room = Inside userId room }
            , Nav.pushUrl model.key (RoomId.toString room.id)
            )

        UpdateRoom room ->
            case model.room of
                None ->
                    ( model, Cmd.none )

                Pending ->
                    ( model, Cmd.none )

                Inside userId _ ->
                    let
                        newModel =
                            { model | room = Inside userId (Debug.log "room update" room) }
                    in
                    ( newModel, Cmd.none )

        RoomFull ->
            ( { model | roomFull = True }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout []
            (case model.room of
                None ->
                    Components.joinRoomForm SubmitRoomId model.roomIdInputText model.roomFull

                Pending ->
                    Element.text "Joining room..."

                Inside userId room ->
                    let
                        message =
                            case room.state of
                                Rooms.FrontWaitingForPlayers _ ->
                                    userId ++ ": Waiting for players"

                                Rooms.FrontPlaying _ ->
                                    userId ++ ": Playing"
                    in
                    Element.text message
             -- [ GraphicWidget.view model.gameWidgetState (Game.view model.gameModel) ]
            )
        ]
    }
