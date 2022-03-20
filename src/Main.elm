module Main exposing (..)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



---- MODEL ----


type alias ScreenSize =
    { width : Int
    , height : Int
    }


type Unit
    = Lb
    | Kg


type alias Model =
    { screenSize : ScreenSize
    , weight : Int
    , reps : Int
    , oneRepMax : Int
    , unit : Unit
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { screenSize = ScreenSize flags.width flags.height
      , weight = 0
      , reps = 0
      , oneRepMax = 0
      , unit = Lb
      }
    , Cmd.none
    )


type alias Flags =
    { width : Int
    , height : Int
    }



---- UPDATE ----


type Msg
    = SetScreenSize Int Int
    | UpdateWeight String
    | UpdateReps String
    | Calculate
    | ChangeUnit Unit
    | Reset
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetScreenSize width height ->
            ( model, Cmd.none )

        UpdateWeight weight ->
            ( updateWeight weight model, Cmd.none )

        UpdateReps reps ->
            ( updateReps reps model, Cmd.none )

        Calculate ->
            ( calculate model, Cmd.none )

        ChangeUnit unit ->
            ( changeUnit unit model, Cmd.none )

        Reset ->
            ( reset model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateWeight : String -> Model -> Model
updateWeight numberString model =
    let
        newWeight =
            inputStringToNumber numberString model.weight
    in
    { model | weight = newWeight }


updateReps : String -> Model -> Model
updateReps numberString model =
    let
        newReps =
            inputStringToNumber numberString model.reps
    in
    { model | reps = newReps }


calculate : Model -> Model
calculate model =
    let
        oneRepMax =
            if model.reps == 1 then
                model.weight

            else
                round <|
                    toFloat model.weight
                        * (1 + toFloat model.reps / 30)
    in
    { model | oneRepMax = oneRepMax }


changeUnit : Unit -> Model -> Model
changeUnit unit model =
    { model | unit = unit }


reset : Model -> Model
reset model =
    { model
        | weight = 0
        , reps = 0
        , oneRepMax = 0
    }


inputStringToNumber : String -> Int -> Int
inputStringToNumber numberString default =
    if String.isEmpty numberString then
        0

    else
        numberString
            |> String.toInt
            |> Maybe.withDefault default



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onResize SetScreenSize ]



---- VIEW ----


view : Model -> Html Msg
view model =
    viewPhone model


viewPhone : Model -> Html Msg
viewPhone model =
    layout
        [ width fill
        , height fill
        , padding <| padSm model
        ]
    <|
        column
            [ width fill
            , padding <| padMd model
            , Background.color grey
            , Border.rounded 10
            ]
            [ title model
            , numberInputs model
            , unitRadio model
            , calculateButton model
            , result model
            ]


title : Model -> Element Msg
title model =
    row [ width fill ]
        [ el
            [ centerX
            , Font.size <| fontXxl model
            , Font.family fontPrimary
            ]
          <|
            text "1 RM Calculator"
        ]


numberInputs : Model -> Element Msg
numberInputs model =
    let
        multiply =
            el
                [ Font.size <| fontLg model
                , Font.family fontPrimary
                ]
            <|
                text "x"
    in
    row
        [ paddingEach { edges | top = padMd model }
        , spacing <| padXxs model
        ]
        [ weightInput model
        , multiply
        , repsInput model
        ]


weightInput : Model -> Element Msg
weightInput model =
    let
        numberString =
            if model.weight == 0 then
                ""

            else
                String.fromInt model.weight
    in
    row [ width fill ]
        [ Input.text
            numberInputStyle
            { onChange = UpdateWeight
            , text = numberString
            , placeholder = textInputPlaceholder "Weight"
            , label = Input.labelHidden "Weight"
            }
        ]


repsInput : Model -> Element Msg
repsInput model =
    let
        numberString =
            if model.reps == 0 then
                ""

            else
                String.fromInt model.reps
    in
    row [ width fill ]
        [ Input.text
            numberInputStyle
            { onChange = UpdateReps
            , text = numberString
            , placeholder = textInputPlaceholder "Reps"
            , label = Input.labelHidden "Weight"
            }
        ]


unitRadio : Model -> Element Msg
unitRadio model =
    row
        [ paddingEach { edges | top = padXs model }
        ]
        [ Input.radioRow
            [ spacing <| padSm model
            , Font.family fontSecondary
            ]
            { onChange = ChangeUnit
            , selected = Just model.unit
            , options =
                [ Input.optionWith Lb <| radioOption "lb" model
                , Input.optionWith Kg <| radioOption "kg" model
                ]
            , label = Input.labelHidden "Choose weight unit"
            }
        ]


radioOption : String -> Model -> Input.OptionState -> Element Msg
radioOption label model state =
    let
        backgroundColor : Color
        backgroundColor =
            case state of
                Input.Idle ->
                    white

                Input.Focused ->
                    white

                Input.Selected ->
                    blue

        button : Element Msg
        button =
            el
                [ width <| px 20
                , height <| px 20
                , Background.color grey
                , Border.rounded 10
                , Border.width 1
                , Border.color black
                ]
            <|
                el
                    [ centerX
                    , centerY
                    , width <| px 14
                    , height <| px 14
                    , Border.rounded 7
                    , Background.color backgroundColor
                    ]
                    none

        textLabel =
            el
                [ Font.size <| fontLg model
                , Font.family fontSecondary
                ]
            <|
                text label
    in
    row
        [ spacing <| padXxs model
        ]
        [ button
        , textLabel
        ]


numberInputStyle : List (Attribute Msg)
numberInputStyle =
    let
        borderShadow =
            { offset = ( 0, 3 )
            , size = 0
            , blur = 3
            , color = grey
            }
    in
    [ Border.width 2
    , Border.rounded 5
    , Border.color darkGrey
    , Border.innerShadow borderShadow
    , Font.alignLeft
    , Font.family fontSecondary
    , focused []
    ]


textInputPlaceholder : String -> Maybe (Input.Placeholder Msg)
textInputPlaceholder placeholder =
    Just <|
        Input.placeholder [ alignLeft ] <|
            text placeholder


calculateButton : Model -> Element Msg
calculateButton model =
    let
        borderShadow =
            { offset = ( 0, 4 )
            , size = 0
            , blur = 4
            , color = blackTranslucent
            }

        pads =
            { edges
                | top = padSm model
                , bottom = padSm model
            }

        label =
            el
                [ centerX
                , Font.color white
                , Font.size <| fontXl model
                , Font.family fontPrimary
                ]
            <|
                text "Calculate 1 RM"
    in
    row
        [ width fill
        , paddingEach { edges | top = padMd model }
        ]
        [ Input.button
            [ centerX
            , width fill
            , paddingEach pads
            , Background.color blue
            , Border.rounded 5
            , Border.shadow borderShadow
            , focused []
            ]
            { onPress = Just Calculate
            , label = label
            }
        ]


result : Model -> Element Msg
result model =
    let
        unit =
            case model.unit of
                Lb ->
                    " lb"

                Kg ->
                    " kg"

        resultTitle =
            el
                [ centerX
                , Font.size <| fontLg model
                , Font.family fontSecondary
                ]
            <|
                text "Your Estimated 1 Rep Max"

        oneRepMax =
            el
                [ centerX
                , Font.size <| font4Xl model
                , Font.family fontPrimary
                ]
            <|
                (text <|
                    String.fromInt model.oneRepMax
                        ++ unit
                )

        resetButton =
            Input.button
                [ centerX
                , focused []
                ]
                { onPress = Just Reset
                , label = text "Reset"
                }
    in
    if model.oneRepMax == 0 then
        none

    else
        column [ width fill ]
            [ row
                [ width fill
                , paddingEach { edges | top = padLg model }
                ]
                [ resultTitle ]
            , row
                [ width fill
                , paddingEach { edges | top = padXs model }
                ]
                [ oneRepMax ]
            , row
                [ width fill
                , paddingEach { edges | top = padLg model }
                ]
                [ resetButton ]
            ]


black : Color
black =
    rgb255 0 0 0


blackTranslucent : Color
blackTranslucent =
    rgba255 0 0 0 0.1


blue : Color
blue =
    rgb255 48 63 159


grey : Color
grey =
    rgb255 235 235 235


darkGrey : Color
darkGrey =
    rgb255 204 204 204


white : Color
white =
    rgb255 255 255 255


padXxl : Model -> Int
padXxl =
    scalePad << padXl


padXl : Model -> Int
padXl =
    scalePad << padLg


padLg : Model -> Int
padLg =
    scalePad << padMd


padMd : Model -> Int
padMd =
    scalePad << padSm


padSm : Model -> Int
padSm model =
    scaleFromWidth 0.05 model


padXs : Model -> Int
padXs model =
    round <| toFloat (padSm model) / 1.5


padXxs : Model -> Int
padXxs model =
    round <| toFloat (padXs model) / 1.5


scalePad : Int -> Int
scalePad =
    scale 1.5


edges : { left : Int, right : Int, top : Int, bottom : Int }
edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


font4Xl : Model -> Int
font4Xl =
    scaleFont << scaleFont << fontXxl


fontXxl : Model -> Int
fontXxl =
    scaleFont << fontXl


fontXl : Model -> Int
fontXl =
    scaleFont << fontLg


fontLg : Model -> Int
fontLg =
    scaleFont << fontMd


fontMd : Model -> Int
fontMd =
    scaleFont << fontSm


fontSm : Model -> Int
fontSm model =
    scaleFromWidth 0.03 model


scaleFont : Int -> Int
scaleFont =
    scale 1.33


scaleFromWidth : Float -> Model -> Int
scaleFromWidth factor model =
    scale factor <|
        min model.screenSize.width 500


scale : Float -> Int -> Int
scale factor number =
    number
        |> toFloat
        |> (*) factor
        |> round


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



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
