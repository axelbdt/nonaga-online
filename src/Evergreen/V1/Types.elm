module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V1.ClientState
import Evergreen.V1.Clients
import Evergreen.V1.Nonaga
import Evergreen.V1.RoomId
import Evergreen.V1.Rooms
import GraphicSVG.Widget
import Lamdera
import Url


type alias ClientState =
    Evergreen.V1.ClientState.ClientState


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , state : ClientState
    , gameWidgetState : GraphicSVG.Widget.Model
    }


type alias Clients =
    Evergreen.V1.Clients.Clients


type alias Rooms =
    Evergreen.V1.Rooms.Rooms


type alias BackendModel =
    { clients : Clients
    , rooms : Rooms
    }


type alias Size =
    { w : Float
    , h : Float
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | WindowResized Size
    | NoOpFrontendMsg
    | GameWidgetMsg GraphicSVG.Widget.Msg
    | GameMsg Evergreen.V1.Nonaga.Msg
    | SubmitRoomId
    | SetRoomIdInputText String


type alias UserId =
    Evergreen.V1.Rooms.UserId


type alias RoomId =
    Evergreen.V1.RoomId.RoomId


type ToBackend
    = ForwardGameMsg
        { userId : UserId
        , roomId : RoomId
        , gameMsg : Evergreen.V1.Nonaga.Msg
        }
    | JoinOrCreateRoom (Maybe UserId) RoomId


type BackendMsg
    = ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = JoinedRoom ClientState
    | LeftRoom
    | UpdateRoom ClientState
    | RoomFull
