module Colors exposing (..)

import Element exposing (Color, rgb255, rgba255, toRgb)


toSvgColor : Color -> String
toSvgColor color =
    let
        to255 accessor =
            toRgb color
                |> accessor
                |> (*) 255
                |> String.fromFloat
    in
    String.concat
        [ "rgb("
        , to255 .red
        , ","
        , to255 .green
        , ","
        , to255 .blue
        , ")"
        ]


background : Bool -> Color
background darkMode =
    if darkMode then
        black

    else
        white


foreground : Bool -> Color
foreground darkMode =
    if darkMode then
        white

    else
        black


washLight : Bool -> Color
washLight darkMode =
    if darkMode then
        slateGrey

    else
        lightGrey


washHeavy : Bool -> Color
washHeavy darkMode =
    if darkMode then
        lightGrey

    else
        darkGrey


accentPrimary : Bool -> Color
accentPrimary darkMode =
    if darkMode then
        blue

    else
        darkBlue


accentSecondary : Bool -> Color
accentSecondary darkMode =
    if darkMode then
        lightBlue

    else
        darkBlue


shadowColor : Bool -> Color
shadowColor darkMode =
    if darkMode then
        black

    else
        blackTranslucent


white : Color
white =
    rgb255 255 255 255


lightBlue : Color
lightBlue =
    rgb255 0 98 255


blue : Color
blue =
    rgb255 53 69 177


darkBlue : Color
darkBlue =
    rgb255 48 63 159


gold : Color
gold =
    rgb255 153 130 0


lightGrey : Color
lightGrey =
    rgb255 235 235 235


darkGrey : Color
darkGrey =
    rgb255 179 179 179


slateGrey : Color
slateGrey =
    rgb255 41 41 41


black : Color
black =
    rgb255 26 26 26


blackTranslucent : Color
blackTranslucent =
    rgba255 0 0 0 0.15
