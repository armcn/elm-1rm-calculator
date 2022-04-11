module Utils exposing (..)


scaleFromWidth : Float -> Int -> Int
scaleFromWidth factor width =
    scale factor <|
        min maxScreenWidth width


maxScreenWidth : Int
maxScreenWidth =
    450


scale : Float -> Int -> Int
scale factor number =
    number
        |> toFloat
        |> (*) factor
        |> round
