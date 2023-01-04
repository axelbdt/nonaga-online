module RoomId exposing (..)


type RoomId
    = RoomId String


parse : String -> RoomId
parse string =
    RoomId string


toString : RoomId -> String
toString (RoomId string) =
    string
