module Padding exposing (..)

import Utils


pad4Xl : Int -> Int
pad4Xl =
    scalePad << pad3Xl


pad3Xl : Int -> Int
pad3Xl =
    scalePad << pad2Xl


pad2Xl : Int -> Int
pad2Xl =
    scalePad << padXl


padXl : Int -> Int
padXl =
    scalePad << padLg


padLg : Int -> Int
padLg =
    scalePad << padMd


padMd : Int -> Int
padMd =
    scalePad << padSm


padSm : Int -> Int
padSm width =
    Utils.scaleFromWidth 0.022 width


scalePad : Int -> Int
scalePad =
    Utils.scale 1.5


edges : { left : Int, right : Int, top : Int, bottom : Int }
edges =
    { left = 0, right = 0, top = 0, bottom = 0 }
