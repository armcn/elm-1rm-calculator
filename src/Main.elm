module Main exposing (..)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icons
import Json.Decode as Decode
import Svg.Attributes



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


type alias Model =
    { screenSize : ScreenSize
    , device : Device
    , darkMode : Bool
    , weight : Float
    , reps : Int
    , unit : Unit
    , oneRepMax : Int
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
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetScreenSize Int Int
    | ToggleDarkMode
    | UpdateWeight String
    | UpdateReps String
    | ChangeUnit Unit
    | Calculate
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

        Calculate ->
            ( calculate model, Cmd.none )

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
                | top = padSm model
                , left = padLg model
                , right = padLg model
                , bottom = pad3Xl model
            }
    in
    layout
        [ width fill
        , height fill
        , paddingEach pads
        , Background.color (background model)
        , onEnter Calculate
        , overflowScroll
        ]
    <|
        column [ width fill ]
            [ darkModeToggle model
            , column
                [ centerX
                , width (maximum maxScreenWidth fill)
                , spacing (padLg model)
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
            [ width (maximum maxScreenWidth fill)
            , spacing (padLg model)
            , alignTop
            ]

        pads =
            { edges
                | top = padMd model
                , left = pad2Xl model
                , right = pad2Xl model
                , bottom = pad2Xl model
            }
    in
    layout
        [ width fill
        , paddingEach pads
        , Background.color (background model)
        , onEnter Calculate
        , overflowScroll
        ]
    <|
        column [ width fill ]
            [ darkModeToggle model
            , row
                [ centerX
                , spacing (pad3Xl model)
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
            scaleFromWidth 0.05 model

        iconButtonWidth : Int
        iconButtonWidth =
            iconWidth + padSm model

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
                            toSvgColor <|
                                foreground model
                        , Svg.Attributes.height <|
                            String.fromInt <|
                                iconWidth
                        ]

        shadow2 =
            { offset = ( 0, 4 )
            , size = 2
            , blur = 8
            , color = shadowColor model
            }
    in
    row
        [ alignRight
        , paddingEach { edges | bottom = padSm model }
        ]
        [ Input.button
            [ width (px iconButtonWidth)
            , height (px iconButtonWidth)
            , Border.rounded radius
            , Border.shadow shadow2
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
        , Font.size (font2xl model)
        , Font.color (foreground model)
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
                , Font.size (fontLg model)
                , Font.color (foreground model)
                , Font.family fontPrimary
                ]
                (text "x")
    in
    row
        [ paddingEach { edges | top = padXl model }
        , spacing (padSm model)
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
            , color = shadowColor model
            }
    in
    [ width fill
    , Border.width 1
    , Border.rounded 5
    , Background.color (background model)
    , Border.color (washHeavy model)
    , Border.innerShadow shadow3
    , Font.alignLeft
    , Font.size (fontLg model)
    , Font.color (foreground model)
    , Font.family fontSecondary
    , Element.htmlAttribute (Html.Attributes.type_ "number")
    , focused []
    ]


numberInputPlaceholder : String -> Model -> Maybe (Input.Placeholder Msg)
numberInputPlaceholder placeholder model =
    Input.placeholder
        [ alignLeft
        , Font.size (fontLg model)
        , Font.color (washHeavy model)
        , Font.family fontSecondary
        ]
        (text placeholder)
        |> Just


unitRadio : Model -> Element Msg
unitRadio model =
    Input.radioRow
        [ paddingEach { edges | top = padMd model }
        , spacing (padLg model)
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
                    background model

                Input.Focused ->
                    background model

                Input.Selected ->
                    accent model

        radioButton : Element Msg
        radioButton =
            el
                [ width (px outerDiameter)
                , height (px outerDiameter)
                , Background.color (washLight model)
                , Border.width 1
                , Border.color (foreground model)
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
                [ Font.size (fontLg model)
                , Font.color (foreground model)
                , Font.family fontSecondary
                ]
                (text label)
    in
    row
        [ spacing (padSm model) ]
        [ radioButton
        , textLabel
        ]


calculateButton : Model -> Element Msg
calculateButton model =
    let
        label : Element Msg
        label =
            el
                [ centerX
                , Font.size (fontXl model)
                , Font.color white
                , Font.family fontPrimary
                , Font.letterSpacing 0.3
                ]
                (text "Calculate 1 RM")

        shadow4 =
            { offset = ( 0, 2 )
            , size = 0
            , blur = 4
            , color = shadowColor model
            }
    in
    el
        [ width fill
        , paddingEach { edges | top = padXl model }
        ]
    <|
        Input.button
            [ width fill
            , padding (padLg model)
            , Background.color (accent model)
            , Border.rounded 5
            , Border.shadow shadow4
            , focused []
            ]
            { onPress = Just Calculate
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
                , Font.size (fontLg model)
                , Font.color (foreground model)
                , Font.family fontSecondary
                ]
                (text "Your Estimated 1 Rep Max")

        resultValue : Element Msg
        resultValue =
            el
                [ centerX
                , Font.size (font4Xl model)
                , Font.color (foreground model)
                , Font.family fontPrimary
                ]
            <|
                text <|
                    String.fromInt model.oneRepMax
                        ++ " "
                        ++ unitString

        resetlabel : Element Msg
        resetlabel =
            html <|
                Icons.reset
                    [ Svg.Attributes.fill <|
                        toSvgColor <|
                            accent model
                    , Svg.Attributes.height <|
                        String.fromInt <|
                            scaleFromWidth 0.1 model
                    ]

        resetButton : Element Msg
        resetButton =
            Input.button
                [ centerX
                , focused []
                ]
                { onPress = Just Reset
                , label = resetlabel
                }
    in
    if model.oneRepMax == 0 then
        none

    else
        column [ width fill ]
            [ el
                [ width fill
                , paddingEach { edges | top = pad2Xl model }
                ]
                resultTitle
            , el
                [ width fill
                , paddingEach { edges | top = padMd model }
                ]
                resultValue
            , el
                [ width fill
                , paddingEach { edges | top = pad2Xl model }
                ]
                resetButton
            ]


affiliateTextLink : Model -> Element Msg
affiliateTextLink model =
    let
        label : Element Msg
        label =
            paragraph
                [ Font.size <| fontMd model
                , Font.family fontSecondary
                ]
                [ el
                    [ Font.color (washHeavy model) ]
                    (text "Buy")
                , el
                    [ Font.color gold
                    , Font.bold
                    ]
                    (text " GOLD STANDARD ")
                , el
                    [ Font.color (washHeavy model) ]
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
        , Font.size <| fontLg model
        , Font.color <| foreground model
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
                [ spacing (padSm model)
                , Font.size (fontMd model)
                , Font.color (foreground model)
                , Font.family fontSecondary
                , Font.alignLeft
                ]
                [ text content ]

        moreInfoLink : Element Msg
        moreInfoLink =
            newTabLink
                [ Font.size (fontLg model)
                , Font.color (accent model)
                , Font.family fontSecondary
                ]
                { url = "https://en.wikipedia.org/wiki/One-repetition_maximum"
                , label = text "More info about 1RM"
                }
    in
    column
        [ paddingEach { edges | top = padXl model }
        , spacing (padXl model)
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
            , Font.size (fontMd model)
            , Font.color (foreground model)
            , Font.family fontSecondary
            ]

        linkStyle : List (Attribute Msg)
        linkStyle =
            [ Font.color (accent model)
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
        , spacing <| padMd model
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
        , paddingEach { edges | top = padLg model }
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
            , color = shadowColor model
            }
    in
    column
        [ padding (padXl model)
        , Background.color (washLight model)
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


background : Model -> Color
background model =
    if model.darkMode then
        black

    else
        white


foreground : Model -> Color
foreground model =
    if model.darkMode then
        white

    else
        black


washLight : Model -> Color
washLight model =
    if model.darkMode then
        slateGrey

    else
        lightGrey


washHeavy : Model -> Color
washHeavy model =
    if model.darkMode then
        lightGrey

    else
        darkGrey


accent : Model -> Color
accent model =
    if model.darkMode then
        lightBlue

    else
        blue


shadowColor : Model -> Color
shadowColor model =
    if model.darkMode then
        black

    else
        blackTranslucent


white : Color
white =
    rgb255 255 255 255


lightBlue : Color
lightBlue =
    rgb255 53 69 177


blue : Color
blue =
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


pad4Xl : Model -> Int
pad4Xl =
    scalePad << pad3Xl


pad3Xl : Model -> Int
pad3Xl =
    scalePad << pad2Xl


pad2Xl : Model -> Int
pad2Xl =
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
padSm =
    scaleFromWidth 0.022


scalePad : Int -> Int
scalePad =
    scale 1.5


edges : { left : Int, right : Int, top : Int, bottom : Int }
edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


font4Xl : Model -> Int
font4Xl =
    scaleFont << scaleFont << font2xl


font2xl : Model -> Int
font2xl =
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
        min maxScreenWidth model.screenSize.width


scale : Float -> Int -> Int
scale factor number =
    number
        |> toFloat
        |> (*) factor
        |> round


maxScreenWidth : Int
maxScreenWidth =
    450


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
