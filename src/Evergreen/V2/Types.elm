module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V2.ClientState
import Evergreen.V2.Clients
import Evergreen.V2.Nonaga
import Evergreen.V2.RoomId
import Evergreen.V2.Rooms
import GraphicSVG.Widget
import Lamdera
import Url


type alias ClientState =
    Evergreen.V2.ClientState.ClientState


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , state : ClientState
    , gameWidgetState : GraphicSVG.Widget.Model
    }


type alias Clients =
    Evergreen.V2.Clients.Clients


type alias Rooms =
    Evergreen.V2.Rooms.Rooms


type alias BackendModel =
    { clients : Clients
    , rooms : Rooms
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | GameWidgetMsg GraphicSVG.Widget.Msg
    | GameMsg Evergreen.V2.Nonaga.Msg
    | SubmitRoomId
    | SetRoomIdInputText String


type alias UserId =
    Evergreen.V2.Rooms.UserId


type alias RoomId =
    Evergreen.V2.RoomId.RoomId


type ToBackend
    = ForwardGameMsg
        { userId : UserId
        , roomId : RoomId
        , gameMsg : Evergreen.V2.Nonaga.Msg
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
