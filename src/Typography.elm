module Typography exposing (..)

import Element.Font as Font
import Utils


font4Xl : Int -> Int
font4Xl =
    scaleFont << scaleFont << font2Xl


font2Xl : Int -> Int
font2Xl =
    scaleFont << fontXl


fontXl : Int -> Int
fontXl =
    scaleFont << fontLg


fontLg : Int -> Int
fontLg =
    scaleFont << fontMd


fontMd : Int -> Int
fontMd =
    scaleFont << fontSm


fontSm : Int -> Int
fontSm width =
    Utils.scaleFromWidth 0.03 width


scaleFont : Int -> Int
scaleFont =
    Utils.scale 1.33


fontPrimary : List Font.Font
fontPrimary =
    [ Font.typeface "Barlow Semi Condensed"
    , Font.sansSerif
    ]


fontSecondary : List Font.Font
fontSecondary =
    [ Font.typeface "Fira Sans"
    , Font.sansSerif
    ]
