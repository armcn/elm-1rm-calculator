module Main exposing (..)

import Browser
import Browser.Events
import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Json.Decode as Decode
import Padding exposing (..)
import Svg.Attributes
import Typography exposing (..)
import Utils



---- MODEL ----


type alias ScreenSize =
    { width : Int
    , height : Int
    }


type Device
    = Phone
    | Desktop


type Unit
    = Lb
    | Kg


type CalcButtonState
    = CalcResting
    | CalcHovered
    | CalcPressed


type ResetButtonState
    = ResetResting
    | ResetHovered
    | ResetPressed


type alias Model =
    { screenSize : ScreenSize
    , device : Device
    , darkMode : Bool
    , weight : Float
    , reps : Int
    , unit : Unit
    , oneRepMax : Int
    , calcButtonState : CalcButtonState
    , resetButtonState : ResetButtonState
    }


type alias Flags =
    { width : Int
    , height : Int
    , darkMode : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { screenSize = ScreenSize flags.width flags.height
      , device = classifyDevice flags.width flags.height
      , darkMode = flags.darkMode
      , weight = 0
      , reps = 0
      , unit = Lb
      , oneRepMax = 0
      , calcButtonState = CalcResting
      , resetButtonState = ResetResting
      }
    , Cmd.none
    )



---- UPDATE ----


type CalculateMsg
    = CalcClick
    | CalcHover
    | CalcPress
    | CalcUnpress
    | CalcLeave


type ResetMsg
    = ResetHover
    | ResetPress
    | ResetUnpress
    | ResetLeave


type Msg
    = SetScreenSize Int Int
    | ToggleDarkMode
    | UpdateWeight String
    | UpdateReps String
    | ChangeUnit Unit
    | UpdateCalculate CalculateMsg
    | UpdateReset ResetMsg
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetScreenSize width height ->
            ( setScreenSize width height model, Cmd.none )

        ToggleDarkMode ->
            ( toggleDarkMode model, Cmd.none )

        UpdateWeight weight ->
            ( updateWeight weight model, Cmd.none )

        UpdateReps reps ->
            ( updateReps reps model, Cmd.none )

        ChangeUnit unit ->
            ( changeUnit unit model, Cmd.none )

        UpdateCalculate calculateMsg ->
            ( updateCalculate calculateMsg model, Cmd.none )

        UpdateReset resetMsg ->
            ( updateReset resetMsg model, Cmd.none )

        Reset ->
            ( reset model, Cmd.none )


setScreenSize : Int -> Int -> Model -> Model
setScreenSize width height model =
    { model
        | screenSize = ScreenSize width height
        , device = classifyDevice width height
    }


toggleDarkMode : Model -> Model
toggleDarkMode model =
    { model | darkMode = not model.darkMode }


updateWeight : String -> Model -> Model
updateWeight numberString model =
    let
        newWeight : Float
        newWeight =
            numberStringToFloat numberString model.weight
    in
    if newWeight < 0 then
        model

    else
        { model | weight = newWeight }


updateReps : String -> Model -> Model
updateReps numberString model =
    let
        newReps : Int
        newReps =
            numberStringToInt numberString model.reps
    in
    if newReps < 0 then
        model

    else
        { model | reps = newReps }


numberStringToInt : String -> Int -> Int
numberStringToInt numberString default =
    if String.isEmpty numberString then
        0

    else
        numberString
            |> String.toInt
            |> Maybe.withDefault default


numberStringToFloat : String -> Float -> Float
numberStringToFloat numberString default =
    if String.isEmpty numberString then
        0

    else
        numberString
            |> String.toFloat
            |> Maybe.withDefault default


changeUnit : Unit -> Model -> Model
changeUnit unit model =
    { model | unit = unit }


updateCalculate : CalculateMsg -> Model -> Model
updateCalculate calculateMsg model =
    let
        calcButtonState : CalcButtonState
        calcButtonState =
            case calculateMsg of
                CalcClick ->
                    CalcResting

                CalcHover ->
                    CalcHovered

                CalcPress ->
                    CalcPressed

                CalcUnpress ->
                    CalcResting

                CalcLeave ->
                    CalcResting

        newModel : Model
        newModel =
            { model | calcButtonState = calcButtonState }
    in
    if calculateMsg == CalcUnpress || calculateMsg == CalcClick then
        calculate newModel

    else
        newModel


calculate : Model -> Model
calculate model =
    let
        oneRepMax : Int
        oneRepMax =
            if model.reps == 1 then
                round model.weight

            else
                model.weight
                    * (1 + toFloat model.reps / 30)
                    |> round
    in
    { model | oneRepMax = oneRepMax }


updateReset : ResetMsg -> Model -> Model
updateReset resetMsg model =
    let
        resetButtonState : ResetButtonState
        resetButtonState =
            case resetMsg of
                ResetHover ->
                    ResetHovered

                ResetPress ->
                    ResetPressed

                ResetUnpress ->
                    ResetResting

                ResetLeave ->
                    ResetResting

        newModel : Model
        newModel =
            { model | resetButtonState = resetButtonState }
    in
    if resetMsg == ResetUnpress then
        reset newModel

    else
        newModel


reset : Model -> Model
reset model =
    { model
        | weight = 0
        , reps = 0
        , oneRepMax = 0
    }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onResize SetScreenSize ]



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.device of
        Phone ->
            viewPhone model

        Desktop ->
            viewDesktop model


viewPhone : Model -> Html Msg
viewPhone model =
    let
        pads =
            { edges
                | top = padSm model.screenSize.width
                , left = padLg model.screenSize.width
                , right = padLg model.screenSize.width
                , bottom = pad3Xl model.screenSize.width
            }
    in
    layout
        [ width fill
        , height fill
        , paddingEach pads
        , Background.color (Colors.background model.darkMode)
        , onEnter (UpdateCalculate CalcClick)
        , overflowScroll
        ]
    <|
        column [ width fill ]
            [ darkModeToggle model
            , column
                [ centerX
                , width (maximum Utils.maxScreenWidth fill)
                , spacing (padLg model.screenSize.width)
                ]
                [ calculatorPanel model
                , affiliateTextLink model
                , infoPanel model
                , developerLinks model
                , affiliateImageLinks model
                ]
            ]


viewDesktop : Model -> Html Msg
viewDesktop model =
    let
        columnStyle : List (Attribute Msg)
        columnStyle =
            [ width (maximum Utils.maxScreenWidth fill)
            , spacing (padLg model.screenSize.width)
            , alignTop
            ]

        pads =
            { edges
                | top = padMd model.screenSize.width
                , left = pad2Xl model.screenSize.width
                , right = pad2Xl model.screenSize.width
                , bottom = pad2Xl model.screenSize.width
            }
    in
    layout
        [ width fill
        , paddingEach pads
        , Background.color (Colors.background model.darkMode)
        , onEnter (UpdateCalculate CalcClick)
        , overflowScroll
        ]
    <|
        column [ width fill ]
            [ darkModeToggle model
            , row
                [ centerX
                , spacing (pad3Xl model.screenSize.width)
                ]
                [ column
                    columnStyle
                    [ calculatorPanel model
                    , affiliateImageLinks model
                    , developerLinks model
                    ]
                , column
                    columnStyle
                    [ infoPanel model ]
                ]
            ]


darkModeToggle : Model -> Element Msg
darkModeToggle model =
    let
        iconWidth : Int
        iconWidth =
            Utils.scaleFromWidth 0.05 model.screenSize.width

        iconButtonWidth : Int
        iconButtonWidth =
            iconWidth + padSm model.screenSize.width

        radius : Int
        radius =
            iconButtonWidth
                |> toFloat
                |> (\x -> x / 2)
                |> round

        icon : Icons.Icon Msg
        icon =
            if model.darkMode then
                Icons.sun

            else
                Icons.moon

        label : Element Msg
        label =
            el [ centerX, centerY ] <|
                html <|
                    icon
                        [ Svg.Attributes.fill <|
                            Colors.toSvgColor <|
                                Colors.foreground model.darkMode
                        , Svg.Attributes.height <|
                            String.fromInt <|
                                iconWidth
                        ]

        shadow =
            { offset = ( 0, 4 )
            , size = 2
            , blur = 8
            , color = Colors.shadowColor model.darkMode
            }
    in
    row
        [ alignRight
        , paddingEach { edges | bottom = padSm model.screenSize.width }
        ]
        [ Input.button
            [ width (px iconButtonWidth)
            , height (px iconButtonWidth)
            , Border.rounded radius
            , Border.shadow shadow
            , focused []
            ]
            { onPress = Just ToggleDarkMode
            , label = label
            }
        ]


calculatorPanel : Model -> Element Msg
calculatorPanel model =
    panel
        [ calculatorTitle model
        , numberInputs model
        , unitRadio model
        , calculateButton model
        , result model
        ]
        model


calculatorTitle : Model -> Element Msg
calculatorTitle model =
    el
        [ centerX
        , Font.size (font2Xl model.screenSize.width)
        , Font.color (Colors.foreground model.darkMode)
        , Font.family fontPrimary
        , Font.letterSpacing 0.3
        ]
        (text "1 RM Calculator")


numberInputs : Model -> Element Msg
numberInputs model =
    let
        multiply : Element Msg
        multiply =
            el
                [ centerY
                , Font.size (fontLg model.screenSize.width)
                , Font.color (Colors.foreground model.darkMode)
                , Font.family fontPrimary
                ]
                (text "x")
    in
    row
        [ paddingEach { edges | top = padXl model.screenSize.width }
        , spacing (padSm model.screenSize.width)
        ]
        [ weightInput model
        , multiply
        , repsInput model
        ]


weightInput : Model -> Element Msg
weightInput model =
    let
        numberString : String
        numberString =
            if model.weight == 0 then
                ""

            else
                String.fromFloat model.weight
    in
    Input.text
        (numberInputStyle model)
        { onChange = UpdateWeight
        , text = numberString
        , placeholder = numberInputPlaceholder "Weight" model
        , label = Input.labelHidden "Weight"
        }


repsInput : Model -> Element Msg
repsInput model =
    let
        numberString : String
        numberString =
            if model.reps == 0 then
                ""

            else
                String.fromInt model.reps
    in
    Input.text
        (numberInputStyle model)
        { onChange = UpdateReps
        , text = numberString
        , placeholder = numberInputPlaceholder "Reps" model
        , label = Input.labelHidden "Weight"
        }


numberInputStyle : Model -> List (Attribute Msg)
numberInputStyle model =
    let
        shadow3 =
            { offset = ( 0, 2 )
            , size = 0
            , blur = 4
            , color = Colors.shadowColor model.darkMode
            }
    in
    [ width fill
    , Border.width 1
    , Border.rounded 5
    , Background.color (Colors.background model.darkMode)
    , Border.color (Colors.washHeavy model.darkMode)
    , Border.innerShadow shadow3
    , Font.alignLeft
    , Font.size (fontLg model.screenSize.width)
    , Font.color (Colors.foreground model.darkMode)
    , Font.family fontSecondary
    , Element.htmlAttribute (Html.Attributes.type_ "number")
    , focused []
    ]


numberInputPlaceholder : String -> Model -> Maybe (Input.Placeholder Msg)
numberInputPlaceholder placeholder model =
    Input.placeholder
        [ alignLeft
        , Font.size (fontLg model.screenSize.width)
        , Font.color (Colors.washHeavy model.darkMode)
        , Font.family fontSecondary
        ]
        (text placeholder)
        |> Just


unitRadio : Model -> Element Msg
unitRadio model =
    Input.radioRow
        [ paddingEach { edges | top = padMd model.screenSize.width }
        , spacing (padLg model.screenSize.width)
        ]
        { onChange = ChangeUnit
        , selected = Just model.unit
        , options =
            [ Input.optionWith Lb (radioOption "lb" model)
            , Input.optionWith Kg (radioOption "kg" model)
            ]
        , label = Input.labelHidden "Choose weight unit"
        }


radioOption : String -> Model -> Input.OptionState -> Element Msg
radioOption label model state =
    let
        outerRadius : Int
        outerRadius =
            10

        outerDiameter : Int
        outerDiameter =
            outerRadius * 2

        innerRadius : Int
        innerRadius =
            7

        innerDiameter : Int
        innerDiameter =
            innerRadius * 2

        selectedColor : Color
        selectedColor =
            case state of
                Input.Idle ->
                    Colors.background model.darkMode

                Input.Focused ->
                    Colors.background model.darkMode

                Input.Selected ->
                    Colors.accentPrimary model.darkMode

        radioButton : Element Msg
        radioButton =
            el
                [ width (px outerDiameter)
                , height (px outerDiameter)
                , Background.color (Colors.washLight model.darkMode)
                , Border.width 1
                , Border.color (Colors.foreground model.darkMode)
                , Border.rounded outerRadius
                ]
            <|
                el
                    [ centerX
                    , centerY
                    , width (px innerDiameter)
                    , height (px innerDiameter)
                    , Background.color selectedColor
                    , Border.rounded innerRadius
                    ]
                    none

        textLabel : Element Msg
        textLabel =
            el
                [ Font.size (fontLg model.screenSize.width)
                , Font.color (Colors.foreground model.darkMode)
                , Font.family fontSecondary
                ]
                (text label)
    in
    row
        [ spacing (padSm model.screenSize.width) ]
        [ radioButton
        , textLabel
        ]


calculateButton : Model -> Element Msg
calculateButton model =
    let
        shadowY : Float
        shadowY =
            4

        shadow =
            case model.calcButtonState of
                CalcResting ->
                    { offset = ( 0, shadowY )
                    , size = 0
                    , blur = shadowY
                    , color = Colors.shadowColor model.darkMode
                    }

                CalcHovered ->
                    { offset = ( 0, shadowY * 3 )
                    , size = 2
                    , blur = shadowY * 3
                    , color = Colors.shadowColor model.darkMode
                    }

                CalcPressed ->
                    { offset = ( 0, 0 )
                    , size = 0
                    , blur = 0
                    , color = Colors.shadowColor model.darkMode
                    }

        letterSpacing : Float
        letterSpacing =
            case model.calcButtonState of
                CalcResting ->
                    0.3

                CalcHovered ->
                    0.4

                CalcPressed ->
                    0.2

        label : Element Msg
        label =
            el
                [ centerX
                , Font.size (fontXl model.screenSize.width)
                , Font.color Colors.white
                , Font.family fontPrimary
                , Font.letterSpacing letterSpacing
                ]
                (text "Calculate 1 RM")
    in
    el
        [ width fill
        , paddingEach { edges | top = padXl model.screenSize.width }
        ]
    <|
        Input.button
            [ width fill
            , padding (padLg model.screenSize.width)
            , Background.color (Colors.accentPrimary model.darkMode)
            , Border.rounded 5
            , Border.shadow shadow
            , Events.onMouseEnter (UpdateCalculate CalcHover)
            , Events.onMouseDown (UpdateCalculate CalcPress)
            , Events.onMouseUp (UpdateCalculate CalcUnpress)
            , Events.onMouseLeave (UpdateCalculate CalcLeave)
            , focused []
            ]
            { onPress = Nothing
            , label = label
            }


result : Model -> Element Msg
result model =
    let
        unitString : String
        unitString =
            case model.unit of
                Lb ->
                    "lb"

                Kg ->
                    "kg"

        resultTitle : Element Msg
        resultTitle =
            el
                [ centerX
                , Font.size (fontLg model.screenSize.width)
                , Font.color (Colors.foreground model.darkMode)
                , Font.family fontSecondary
                ]
                (text "Your Estimated 1 Rep Max")

        resultValue : Element Msg
        resultValue =
            el
                [ centerX
                , Font.size (font4Xl model.screenSize.width)
                , Font.color (Colors.foreground model.darkMode)
                , Font.family fontPrimary
                ]
            <|
                text <|
                    String.fromInt model.oneRepMax
                        ++ " "
                        ++ unitString
    in
    if model.oneRepMax == 0 then
        none

    else
        column [ width fill ]
            [ el
                [ width fill
                , paddingEach { edges | top = pad2Xl model.screenSize.width }
                ]
                resultTitle
            , el
                [ width fill
                , paddingEach { edges | top = padMd model.screenSize.width }
                ]
                resultValue
            , el
                [ width fill
                , paddingEach { edges | top = pad2Xl model.screenSize.width }
                ]
                (resetButton model)
            ]


resetButton : Model -> Element Msg
resetButton model =
    let
        iconWidth : Int
        iconWidth =
            Utils.scaleFromWidth 0.07 model.screenSize.width

        iconButtonWidth : Int
        iconButtonWidth =
            iconWidth + padSm model.screenSize.width

        radius : Int
        radius =
            iconButtonWidth
                |> toFloat
                |> (\x -> x / 2)
                |> round

        rotateIcon : String
        rotateIcon =
            if model.resetButtonState == ResetHovered then
                "rotate(-45)"

            else
                "rotate(0"

        label : Element Msg
        label =
            el [ centerX, centerY ] <|
                html <|
                    Icons.reset
                        [ Svg.Attributes.fill <|
                            Colors.toSvgColor <|
                                Colors.accentSecondary model.darkMode
                        , Svg.Attributes.height <|
                            String.fromInt <|
                                iconWidth
                        , Svg.Attributes.transform rotateIcon
                        ]

        shadowY : Float
        shadowY =
            3

        shadow =
            case model.resetButtonState of
                ResetResting ->
                    { offset = ( 0, shadowY )
                    , size = 1
                    , blur = shadowY
                    , color = Colors.shadowColor model.darkMode
                    }

                ResetHovered ->
                    { offset = ( 0, shadowY * 2 )
                    , size = 2
                    , blur = shadowY * 3
                    , color = Colors.shadowColor model.darkMode
                    }

                ResetPressed ->
                    { offset = ( 0, 0 )
                    , size = 0
                    , blur = 0
                    , color = Colors.shadowColor model.darkMode
                    }
    in
    row
        [ centerX ]
        [ Input.button
            [ width (px iconButtonWidth)
            , height (px iconButtonWidth)
            , Border.rounded radius
            , Border.shadow shadow
            , Events.onMouseEnter (UpdateReset ResetHover)
            , Events.onMouseDown (UpdateReset ResetPress)
            , Events.onMouseUp (UpdateReset ResetUnpress)
            , Events.onMouseLeave (UpdateReset ResetLeave)
            , focused []
            ]
            { onPress = Nothing
            , label = label
            }
        ]


affiliateTextLink : Model -> Element Msg
affiliateTextLink model =
    let
        label : Element Msg
        label =
            paragraph
                [ Font.size (fontMd model.screenSize.width)
                , Font.family fontSecondary
                ]
                [ el
                    [ Font.color (Colors.washHeavy model.darkMode) ]
                    (text "Buy")
                , el
                    [ Font.color Colors.gold
                    , Font.bold
                    ]
                    (text " GOLD STANDARD ")
                , el
                    [ Font.color (Colors.washHeavy model.darkMode) ]
                    (text "100% Whey Protein Powder")
                ]
    in
    newTabLink
        [ centerX ]
        { url = "https://www.amazon.ca/gp/product/B08R8LRMWQ/ref=as_li_tl?ie=UTF8&camp=15121&creative=330641&creativeASIN=B08R8LRMWQ&linkCode=as2&tag=1repmaximum-20&linkId=906890c83da68e917254bfa8a78f8146"
        , label = label
        }


infoPanel : Model -> Element Msg
infoPanel model =
    panel
        [ infoTitle model
        , infoContent model
        ]
        model


infoTitle : Model -> Element Msg
infoTitle model =
    el
        [ centerX
        , Font.size (fontLg model.screenSize.width)
        , Font.color (Colors.foreground model.darkMode)
        , Font.family fontPrimary
        , Font.letterSpacing 0.3
        ]
        (text "About 1 RM Calculator")


infoContent : Model -> Element Msg
infoContent model =
    let
        paragraphWithStyle : String -> Element Msg
        paragraphWithStyle content =
            paragraph
                [ spacing (padSm model.screenSize.width)
                , Font.alignLeft
                , Font.size (fontMd model.screenSize.width)
                , Font.color (Colors.foreground model.darkMode)
                , Font.family fontSecondary
                , Font.light
                ]
                [ text content ]

        moreInfoLink : Element Msg
        moreInfoLink =
            newTabLink
                [ Font.size (fontLg model.screenSize.width)
                , Font.color (Colors.accentSecondary model.darkMode)
                , Font.family fontSecondary
                , Font.light
                ]
                { url = "https://en.wikipedia.org/wiki/One-repetition_maximum"
                , label = text "More info about 1RM"
                }
    in
    column
        [ paddingEach { edges | top = padXl model.screenSize.width }
        , spacing (padXl model.screenSize.width)
        ]
        [ paragraphWithStyle "A one repetition maximum (one rep max or 1RM) in weight training is the maximum amount of weight that a person can possibly lift for one repetition. It can be used for determining an individuals maximum strength and is the method for determining the winner in events such as powerlifting and weightlifting competitions. A one repetition maximum can also be used as an upper limit, in order to determine the desired load for an exercise (as a percentage of the 1RM)."
        , paragraphWithStyle "The 1RM can either be calculated directly using maximal testing or indirectly using submaximal estimation. The submaximal estimation method is preferred as it is safer, quicker, and less unnerving for inexperienced exercisers, however, it may underestimate the actual 1RM. One rep maximum calculators are used to predict a one rep maximum lift. The degree of accuracy can vary largely depending on the weight training experience and muscular composition of the athlete. Also, most one rep maximum calculators are designed for seasoned strength trainers, and those with little experience may find their actual one rep maximum is much lower because their nervous system cannot handle the stress of a high weight. This test should be performed with a spotter for reasons of safety."
        , paragraphWithStyle "There are several common formulas used to estimate 1RM using the submaximal method, the Epley and the Brzycki being the most common. This app uses the Epley method."
        , moreInfoLink
        ]


developerLinks : Model -> Element Msg
developerLinks model =
    let
        textStyle : List (Attribute Msg)
        textStyle =
            [ Font.center
            , Font.size (fontMd model.screenSize.width)
            , Font.color (Colors.foreground model.darkMode)
            , Font.family fontSecondary
            ]

        linkStyle : List (Attribute Msg)
        linkStyle =
            [ Font.color (Colors.accentSecondary model.darkMode)
            , Font.bold
            ]

        designerLink : Element Msg
        designerLink =
            newTabLink
                linkStyle
                { url = "https://www.mariayevickery.com"
                , label = text "Mariaye Vickery"
                }

        developerLink : Element Msg
        developerLink =
            newTabLink
                linkStyle
                { url = "https://www.github.com/armcn"
                , label = text "Andrew McNeil"
                }
    in
    column
        [ centerX
        , spacing <| padMd model.screenSize.width
        ]
        [ paragraph
            textStyle
            [ text "Product design by "
            , designerLink
            ]
        , paragraph
            textStyle
            [ text "Built with elm-lang and elm-ui by "
            , developerLink
            ]
        ]


affiliateImageLinks : Model -> Element Msg
affiliateImageLinks model =
    let
        imageLink src =
            Html.iframe
                [ Html.Attributes.src src
                , Html.Attributes.style "width" "120px"
                , Html.Attributes.style "height" "240px"
                , Html.Attributes.style "marginwidth" "0"
                , Html.Attributes.style "marginheight" "0"
                , Html.Attributes.style "frameborder" "0"
                , Html.Attributes.style "scrolling" "no"
                ]
                []
                |> html
    in
    row
        [ centerX
        , paddingEach { edges | top = padLg model.screenSize.width }
        ]
    <|
        List.map imageLink
            [ "//ws-na.amazon-adsystem.com/widgets/cm?ref=tf_til&t=1repmaximum-20&m=amazon&o=15&p=8&l=as1&IS1=1&asins=B08R8LRMWQ&linkId=cb68196a37380a7ce5b4d9c43a3b0c03&bc1=FFFFFF&amp;lt1=_top&fc1=333333&lc1=0066C0&bg1=FFFFFF&f=ifr"
            , "//ws-na.amazon-adsystem.com/widgets/cm?ref=tf_til&t=1repmaximum-20&m=amazon&o=15&p=8&l=as1&IS1=1&asins=B078RZZSF1&linkId=3884cb68c0639febf5ada4964108afa6&bc1=ffffff&amp;lt1=_top&fc1=333333&lc1=0066c0&bg1=ffffff&f=ifr"
            , "//ws-na.amazon-adsystem.com/widgets/cm?ref=tf_til&t=1repmaximum-20&m=amazon&o=15&p=8&l=as1&IS1=1&asins=B074NSYGJ2&linkId=49117e07e31540b31d96d52880d6a6da&bc1=FFFFFF&amp;lt1=_top&fc1=333333&lc1=0066C0&bg1=FFFFFF&f=ifr"
            ]


panel : List (Element Msg) -> Model -> Element Msg
panel children model =
    let
        shadow =
            { offset = ( 0, 4 )
            , size = 0
            , blur = 6
            , color = Colors.shadowColor model.darkMode
            }
    in
    column
        [ padding (padXl model.screenSize.width)
        , Background.color (Colors.washLight model.darkMode)
        , Border.rounded 10
        , Border.shadow shadow
        ]
        children


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        enterPressed =
            Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
    in
    htmlAttribute (Html.Events.on "keyup" enterPressed)


overflowScroll : Attribute Msg
overflowScroll =
    htmlAttribute (Html.Attributes.style "overflow" "scroll")


classifyDevice : Int -> Int -> Device
classifyDevice width _ =
    if width < 1000 then
        Phone

    else
        Desktop



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
