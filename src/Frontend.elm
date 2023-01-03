module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Debug
import Element exposing (..)
import Element.Input as Input
import GraphicSVG.Widget as GraphicWidget
import Html
import Html.Attributes as Attr
import Html.Events
import Lamdera exposing (sendToBackend)
import Material.Icons exposing (login)
import Material.Icons.Types exposing (Coloring(..))
import Nonaga as Game
import Types exposing (..)
import Url
import Widget as W
import Widget.Customize as Customize
import Widget.Icon exposing (Icon)
import Widget.Material as Material


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
    in
    ( { key = key
      , roomId = Nothing
      , gameModel = Game.initialModel
      , gameWidgetState = gameWidgetState
      , roomIdInputText = ""
      }
    , Cmd.map GameWidgetMsg gameWidgetCommand
    )


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
            ( model, Cmd.none )

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

        JoinedRoom roomId room ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        case model.roomId of
            Nothing ->
                [ Element.layout [] (joinRoomForm model.roomIdInputText) ]

            Just roomId ->
                [ GraphicWidget.view model.gameWidgetState (Game.view model.gameModel) ]
    }


palette =
    Material.defaultPalette


joinRoomForm roomId =
    let
        joinRoomButton =
            W.button
                (Material.containedButton palette
                    |> Customize.elementButton [ Element.centerX ]
                )
                { text = "Enter room", icon = loginIcon, onPress = Just SubmitRoomId }

        roomIdInput =
            { chips = []
            , text = roomId
            , placeholder = Just (Input.placeholder [] (Element.text "Create or join a room"))
            , label = "roomId"
            , onChange = SetRoomIdInputText
            }
                |> W.textInput (Material.textInput palette)

        loginIcon =
            Material.Icons.login |> Widget.Icon.elmMaterialIcons Color
    in
    Element.el [ Element.centerX ]
        (Element.html
            (Html.form
                [ Html.Events.onSubmit SubmitRoomId ]
                [ Element.layout []
                    (Element.column
                        [ Element.padding 50, Element.spacing 10 ]
                        [ roomIdInput
                        , joinRoomButton
                        ]
                    )
                ]
            )
        )


createOrJoinRoomView model =
    { title = ""
    , body = joinRoomForm model.roomIdInputText
    }
