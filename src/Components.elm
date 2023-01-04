module Components exposing (joinRoomForm)

import Element exposing (..)
import Element.Input as Input
import Html
import Html.Attributes as Attr
import Html.Events
import Material.Icons exposing (login)
import Material.Icons.Types exposing (Coloring(..))
import Types exposing (..)
import Widget as W
import Widget.Customize as Customize
import Widget.Icon exposing (Icon)
import Widget.Material as Material


palette =
    Material.defaultPalette


joinRoomForm onSubmit roomId =
    let
        joinRoomButton =
            W.button
                (Material.containedButton palette
                    |> Customize.elementButton [ Element.centerX ]
                )
                { text = "Enter room", icon = loginIcon, onPress = Just onSubmit }

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
                [ Html.Events.onSubmit onSubmit ]
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
