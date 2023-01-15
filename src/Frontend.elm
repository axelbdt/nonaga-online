module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Components
import Dict
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
            RoomSelection
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
                        , sendToBackend
                            (JoinOrCreateRoom (getUserId model.state)
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
                    RoomSelection { roomIdInputText = inputText, roomFull = False }
            in
            ( { model | state = newState }
            , Cmd.none
            )

        SubmitRoomId ->
            case model.state of
                RoomSelection { roomIdInputText } ->
                    ( model
                    , sendToBackend
                        (JoinOrCreateRoom (getUserId model.state) (RoomId.parse roomIdInputText))
                    )

                Inside _ ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        UpdateGameModel gameModel ->
            ( model, Cmd.none )

        JoinedRoom room ->
            ( { model | state = Inside room }
            , Nav.pushUrl model.key (RoomId.toString room.id)
            )

        UpdateRoom room ->
            case model.state of
                RoomSelection _ ->
                    ( model, Cmd.none )

                Inside { userId } ->
                    let
                        newModel =
                            { model | state = Inside room }
                    in
                    ( newModel, Cmd.none )

        RoomFull ->
            let
                newState =
                    case model.state of
                        RoomSelection state ->
                            RoomSelection { state | roomFull = True }

                        Inside _ ->
                            model.state
            in
            ( { model | state = newState }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout []
            (case model.state of
                RoomSelection { roomIdInputText, roomFull } ->
                    Components.joinRoomForm SubmitRoomId roomIdInputText roomFull

                Inside room ->
                    let
                        message =
                            case room.state of
                                Rooms.FrontWaitingForPlayers playersNeeded ->
                                    "Waiting for players: " ++ String.fromInt playersNeeded ++ " more needed"

                                Rooms.FrontPlaying { player } ->
                                    "Playing as " ++ Game.playerText player
                    in
                    Element.text message
             -- [ GraphicWidget.view model.gameWidgetState (Game.view model.gameModel) ]
            )
        ]
    }


getUserId : ClientState -> Maybe UserId
getUserId clientState =
    case clientState of
        RoomSelection _ ->
            Nothing

        Inside { userId } ->
            Just userId
