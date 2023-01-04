module RoomId exposing (..)


type RoomId
    = RoomId String


parse : String -> RoomId
parse string =
    RoomId string


toString : RoomId -> String
toString (RoomId string) =
    string


equal : RoomId -> RoomId -> Bool
equal (RoomId string1) (RoomId string2) =
    string1 == string2
