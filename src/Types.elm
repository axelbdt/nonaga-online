module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Element exposing (..)
import GraphicSVG.Widget as Widget
import Lamdera exposing (ClientId, SessionId)
import Nonaga as Game
import RoomId
import Rooms
import Set exposing (Set)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , state : ClientState

    -- , gameModel : Game.Model
    -- , gameWidgetState : Widget.Model
    }


type ClientState
    = RoomSelection { roomIdInputText : String, roomFull : Bool }
    | Inside { userId : UserId, room : FrontendRoom }


type alias Rooms =
    Rooms.Rooms


type alias RoomId =
    RoomId.RoomId


type alias BackendRoom =
    Rooms.BackendRoom


type alias BackendRoomState =
    Rooms.BackendRoomState


type alias FrontendRoom =
    Rooms.FrontendRoom


type alias FrontendRoomState =
    Rooms.FrontendRoomState


type alias UserId =
    Rooms.UserId


type alias SessionData =
    { sessionId : SessionId, userId : UserId }


type alias Sessions =
    Dict SessionId SessionData


type alias Clients =
    Dict UserId ClientId


type alias BackendModel =
    { clients : Clients
    , rooms : Rooms
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
      -- | GameWidgetMsg Widget.Msg
      -- | GameMsg Game.Msg
    | SubmitRoomId
    | SetRoomIdInputText String


type ToBackend
    = ForwardGameMsg Game.Msg
    | JoinOrCreateRoom (Maybe UserId) RoomId


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = UpdateGameModel Game.Model
    | JoinedRoom UserId FrontendRoom
    | UpdateRoom FrontendRoom
    | RoomFull
