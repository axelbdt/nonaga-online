module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import GraphicSVG.Widget as Widget
import Set exposing (Set)
import Url exposing (Url)


type Player
    = Red
    | Black


type TurnPhase
    = MoveToken
    | MovePlatform


type alias Platform =
    ( Int, Int )


type alias Board =
    Set Platform


type alias Tokens =
    Dict Platform Player


type alias GameModel =
    { currentPlayer : Player
    , turnPhase : TurnPhase
    , board : Board
    , tokens : Tokens
    , lastMovedPlatform : Platform
    , selectedToken : Maybe Platform
    , selectedPlatform : Maybe Platform
    }


type GameMsg
    = SelectToken Player Platform
    | ChooseTokenDestination Platform Platform
    | SelectPlatform Platform
    | ChoosePlatformDestination Platform Platform
    | Reset


type alias FrontendModel =
    { key : Key
    , game : GameModel
    , gameWidgetState : Widget.Model
    }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GameWidgetMsg Widget.Msg
    | GameMsg GameMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
