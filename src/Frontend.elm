module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import ClientState
import Components
import Element as El
import GraphicSVG.Widget as GraphicWidget
import Lamdera exposing (sendToBackend)
import Nonaga as Game
import RoomId as RoomId
import Task
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


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Sub.map GameWidgetMsg GraphicWidget.subscriptions
        , Sub.map WindowResized (onResize coordsToSize)
        ]


coordsToSize : Int -> Int -> Size
coordsToSize x y =
    { w = toFloat x, h = toFloat y }


viewportToSize : Viewport -> Size
viewportToSize vport =
    { w = vport.viewport.width, h = vport.viewport.height }


updateWindowSize : Cmd FrontendMsg
updateWindowSize =
    Task.perform (viewportToSize >> WindowResized) getViewport


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        ( gameWidgetState, _ ) =
            initGameWidget 1000 1000
    in
    case parseRoomId url of
        Nothing ->
            ( initialModel key gameWidgetState, Cmd.batch [ updateWindowSize ] )

        Just roomId ->
            let
                model =
                    initialModel key gameWidgetState

                clientState =
                    ClientState.RoomSelection { roomIdInputText = RoomId.toString roomId, roomFull = False }
            in
            ( model
            , Cmd.batch
                [ sendToBackend
                    (JoinOrCreateRoom Nothing roomId)
                , updateWindowSize
                ]
            )


initialClientState : ClientState
initialClientState =
    ClientState.RoomSelection
        { roomIdInputText = ""
        , roomFull = False
        }


initGameWidget : Float -> Float -> ( GraphicWidget.Model, Cmd GraphicWidget.Msg )
initGameWidget width height =
    GraphicWidget.init width height "gameWidget"


initialModel : Nav.Key -> GraphicWidget.Model -> FrontendModel
initialModel key gameWidgetState =
    { key = key
    , state = initialClientState
    , gameWidgetState = gameWidgetState
    }


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
                    , Nav.pushUrl model.key (Url.toString url)
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

        WindowResized { w, h } ->
            let
                ( gameWidgetState, gameWidgetCmd ) =
                    initGameWidget (1.8 * w) (1.5 * h)

                newModel =
                    { model | gameWidgetState = gameWidgetState }
            in
            ( newModel, Cmd.map GameWidgetMsg gameWidgetCmd )

        GameMsg gameMsg ->
            case model.state of
                ClientState.RoomSelection _ ->
                    ( model, Cmd.none )

                ClientState.WaitingForPlayers _ ->
                    ( model, Cmd.none )

                ClientState.Playing { roomId, userId } ->
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
        JoinedRoom newClientState ->
            let
                commands =
                    case ClientState.getRoomId newClientState of
                        Nothing ->
                            Cmd.none

                        Just roomId ->
                            Nav.pushUrl model.key (RoomId.toString roomId)
            in
            ( { model | state = newClientState }
            , commands
            )

        LeftRoom ->
            let
                newClientState =
                    ClientState.roomSelectionInitialState

                commands =
                    Nav.pushUrl model.key "/"
            in
            ( { model | state = newClientState }
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
            ( { model | state = newState }, Nav.pushUrl model.key "/" )


view : Model -> Browser.Document FrontendMsg
view model =
    { title =
        let
            suffix =
                " - Nonaga"
        in
        case model.state of
            ClientState.RoomSelection _ ->
                "Play Nonaga online!"

            ClientState.WaitingForPlayers { roomId } ->
                RoomId.toString roomId ++ suffix

            ClientState.Playing { roomId } ->
                RoomId.toString roomId ++ suffix
    , body =
        [ El.layout [ El.width El.fill, El.height El.fill ]
            (El.column
                [ El.width El.fill, El.centerY, El.spacing 24 ]
                [ case model.state of
                    ClientState.RoomSelection { roomIdInputText, roomFull } ->
                        El.column [ El.centerX, El.padding 24, El.spacing 24 ]
                            [ Components.title "Play Nonaga online"
                            , Components.joinRoomForm SubmitRoomId roomIdInputText roomFull
                            ]

                    ClientState.WaitingForPlayers { roomId, playersNeeded } ->
                        let
                            waitingMessage =
                                "Waiting for players: " ++ String.fromInt playersNeeded ++ " more needed."
                        in
                        Components.messagesColumn [ waitingMessage, "Invite a friend by sharing the link to this room." ]

                    ClientState.Playing { player, gameModel } ->
                        El.column [ El.width El.fill ]
                            [ let
                                playerInfo =
                                    "Playing as " ++ Game.playerText player

                                turnInfo =
                                    case Game.getWinner gameModel of
                                        Just winner ->
                                            Game.playerText winner ++ " has won."

                                        Nothing ->
                                            case gameModel.turnPhase of
                                                Game.MoveToken ->
                                                    Game.playerText gameModel.currentPlayer ++ " must move a token."

                                                Game.MovePlatform ->
                                                    Game.playerText gameModel.currentPlayer ++ " must move a platform."
                              in
                              Components.messagesColumn
                                [ playerInfo
                                , turnInfo
                                ]
                            , case Game.getWinner gameModel of
                                Nothing ->
                                    El.none

                                Just _ ->
                                    Components.playAgainButton (GameMsg Game.Reset)
                            , El.map GameMsg
                                (El.html
                                    (GraphicWidget.view model.gameWidgetState (Game.view gameModel))
                                )
                            ]
                , Components.footer
                ]
            )
        ]
    }
