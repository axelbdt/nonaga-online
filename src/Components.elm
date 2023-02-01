module Components exposing (footer, joinRoomForm, messagesColumn, playAgainButton, shareLink, title)

import Element as El
import Element.Font as Font
import Element.Input as Input
import Html
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


title text =
    El.el
        [ El.centerX
        , Font.size 24
        ]
        (El.text text)


joinRoomForm onSubmit roomId roomFull =
    let
        roomIdInput =
            { chips = []
            , text = roomId
            , placeholder = Just (Input.placeholder [] (El.text "Create or join a room"))
            , label = "roomId"
            , onChange = SetRoomIdInputText
            }
                |> W.textInput (Material.textInput palette)

        joinRoomButton =
            W.button
                (Material.containedButton palette
                    |> Customize.elementButton [ El.centerX ]
                )
                { text = "Enter room", icon = loginIcon, onPress = Just onSubmit }

        loginIcon =
            Material.Icons.login |> Widget.Icon.elmMaterialIcons Color

        errorMessage =
            El.el [ Font.size 16, El.centerX ]
                (El.text
                    (if roomFull then
                        "This room is full, please enter another."

                     else
                        " "
                    )
                )
    in
    El.el [ El.centerX ]
        (El.html
            (Html.form
                [ Html.Events.onSubmit onSubmit ]
                [ El.layout []
                    (El.column
                        [ El.spacing 12 ]
                        [ errorMessage
                        , roomIdInput
                        , joinRoomButton
                        ]
                    )
                ]
            )
        )


playAgainButton onClick =
    W.textButton
        (Material.containedButton palette
            |> Customize.elementButton [ El.centerX ]
        )
        { text = "Play again", onPress = Just onClick }


messagesColumn messages =
    El.column [ El.centerX, El.padding 8 ]
        (List.map (\m -> El.el [ El.centerX ] (El.text m)) messages)


shareLink url =
    El.link [ Font.underline ] { url = url, label = El.text url }


footer =
    let
        link =
            El.link [ Font.underline ]
    in
    El.column [ El.centerX, Font.size 16 ]
        [ El.paragraph [Font.center] [El.text
            "Nonaga is an abstract strategy game by Viktor Bautista I Roca."]
        , El.row [ El.width El.fill, El.spaceEvenly, El.padding 12 ]
            [ link { url = "https://www.steffen-spiele.de/fileadmin/media/Spiele/Nonaga/Nonaga_EN.pdf", label = El.text "Read the rules" }
            , link { url = "https://www.steffen-spiele.de/index.php?id=2399#popup-4040", label = El.text "Buy it" }
            , link { url = "https://github.com/axelbdt/nonaga-online", label = El.text "Source code" }
            ]
        ]
