module Frontend exposing (..)

import Backend
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Components
import Debug
import Element exposing (..)
import GraphicSVG.Widget as GraphicWidget
import Lamdera exposing (sendToBackend)
import Nonaga as Game
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
      , backendModel = Backend.initialModel
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


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch
                        [ Nav.pushUrl model.key (Url.toString url)
                        , sendToBackend (JoinOrCreateRoom url.path)
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
            , sendToBackend (JoinOrCreateRoom model.roomIdInputText)
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        UpdateGameModel gameModel ->
            ( { model | gameModel = gameModel }, Cmd.none )

        JoinedRoom room ->
            ( { model | room = Just room }, Nav.pushUrl model.key room.id )

        UpdateBackendModel backendModel ->
            ( { model | backendModel = backendModel }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout []
            (case model.room of
                Nothing ->
                    Element.column []
                        [ Element.text (Debug.toString model.backendModel)
                        , Components.joinRoomForm SubmitRoomId model.roomIdInputText
                        ]

                Just room ->
                    Element.text room.id
             -- [ GraphicWidget.view model.gameWidgetState (Game.view model.gameModel) ]
            )
        ]
    }
