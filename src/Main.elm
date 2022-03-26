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
    | SwitchDarkMode
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

        SwitchDarkMode ->
            ( switchDarkMode model, Cmd.none )

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


setScreenSize : Int -> Int -> Model -> Model
setScreenSize width height model =
    { model
        | screenSize = ScreenSize width height
        , device = classifyDevice width height
    }


switchDarkMode : Model -> Model
switchDarkMode model =
    { model | darkMode = not model.darkMode }


updateWeight : String -> Model -> Model
updateWeight numberString model =
    let
        newWeight : Float
        newWeight =
            inputStringToFloat numberString model.weight
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
            inputStringToInt numberString model.reps
    in
    if newReps < 0 then
        model

    else
        { model | reps = newReps }


inputStringToInt : String -> Int -> Int
inputStringToInt numberString default =
    if String.isEmpty numberString then
        0

    else
        numberString
            |> String.toInt
            |> Maybe.withDefault default


inputStringToFloat : String -> Float -> Float
inputStringToFloat numberString default =
    if String.isEmpty numberString then
        0

    else
        numberString
            |> String.toFloat
            |> Maybe.withDefault default


calculate : Model -> Model
calculate model =
    let
        oneRepMax : Int
        oneRepMax =
            if model.reps == 1 then
                round model.weight

            else
                round <|
                    model.weight
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
    layout
        [ width fill
        , height fill
        , paddingEach
            { edges
                | top = padXxs model
                , left = padSm model
                , right = padSm model
                , bottom = padXl model
            }
        , Background.color <| background model
        , htmlAttribute <|
            Html.Attributes.style "overflow" "scroll"
        , onEnter Calculate
        ]
    <|
        column [ width fill ]
            [ el
                [ alignRight
                , paddingEach { edges | bottom = padXxs model }
                ]
              <|
                darkModeSwitch model
            , column
                [ centerX
                , width <| maximum maxWidth fill
                , spacing <| padSm model
                ]
                [ calculatorPanel model
                , affiliateLink model
                , infoPanel model
                , developerLinks model
                , affiliateLinks model
                ]
            ]


viewDesktop : Model -> Html Msg
viewDesktop model =
    let
        columnStyle : List (Attribute Msg)
        columnStyle =
            [ width <| maximum maxWidth fill
            , spacing <| padSm model
            , alignTop
            ]
    in
    layout
        [ width fill
        , paddingEach
            { edges
                | top = padXs model
                , left = padLg model
                , right = padLg model
                , bottom = padLg model
            }
        , Background.color <|
            background model
        , htmlAttribute <|
            Html.Attributes.style "overflow" "scroll"
        , onEnter Calculate
        ]
    <|
        column [ width fill ]
            [ el
                [ alignRight
                , paddingEach { edges | bottom = padXxs model }
                ]
              <|
                darkModeSwitch model
            , row
                [ centerX
                , spacing <| padXl model
                ]
                [ column
                    columnStyle
                    [ calculatorPanel model
                    , affiliateLinks model
                    , developerLinks model
                    ]
                , column
                    columnStyle
                    [ infoPanel model
                    ]
                ]
            ]


darkModeSwitch : Model -> Element Msg
darkModeSwitch model =
    let
        iconWidth : Int
        iconWidth =
            scaleFromWidth 0.05 model

        iconButtonWidth : Int
        iconButtonWidth =
            iconWidth + 12

        roundedSize : Int
        roundedSize =
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
            el
                [ centerX
                , centerY
                ]
            <|
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
        [ alignRight ]
        [ Input.button
            [ width <| px iconButtonWidth
            , height <| px iconButtonWidth
            , Border.rounded roundedSize
            , Border.shadow shadow2
            , focused []
            ]
            { onPress = Just SwitchDarkMode
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
    row [ width fill ]
        [ el
            [ centerX
            , Font.size <| fontXxl model
            , Font.letterSpacing 0.3
            , Font.family fontPrimary
            , Font.color <| foreground model
            ]
          <|
            text "1 RM Calculator"
        ]


numberInputs : Model -> Element Msg
numberInputs model =
    let
        multiply : Element Msg
        multiply =
            el
                [ centerY
                , Font.size <| fontLg model
                , Font.family fontPrimary
                , Font.color <| foreground model
                ]
            <|
                text "x"
    in
    row
        [ width fill
        , paddingEach { edges | top = padMd model }
        , spacing <| padXxs model
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
    row [ width fill ]
        [ Input.text
            (numberInputStyle model)
            { onChange = UpdateWeight
            , text = numberString
            , placeholder = textInputPlaceholder "Weight" model
            , label = Input.labelHidden "Weight"
            }
        ]


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
    row [ width fill ]
        [ Input.text
            (numberInputStyle model)
            { onChange = UpdateReps
            , text = numberString
            , placeholder = textInputPlaceholder "Reps" model
            , label = Input.labelHidden "Weight"
            }
        ]


numberInputStyle : Model -> List (Attribute Msg)
numberInputStyle model =
    let
        borderShadow =
            { offset = ( 0, 2 )
            , size = 0
            , blur = 4
            , color = shadowColor model
            }
    in
    [ width fill
    , Background.color <| background model
    , Border.width 1
    , Border.rounded 5
    , Border.color <| washHeavy model
    , Border.innerShadow borderShadow
    , Font.alignLeft
    , Font.size <| fontLg model
    , Font.family fontSecondary
    , Font.color <| foreground model
    , Element.htmlAttribute <|
        Html.Attributes.type_ "number"
    , focused []
    ]


textInputPlaceholder : String -> Model -> Maybe (Input.Placeholder Msg)
textInputPlaceholder placeholder model =
    Just <|
        Input.placeholder
            [ alignLeft
            , Font.size <| fontLg model
            , Font.color <| washHeavy model
            , Font.family fontSecondary
            ]
        <|
            text placeholder


unitRadio : Model -> Element Msg
unitRadio model =
    row
        [ paddingEach { edges | top = padXs model } ]
        [ Input.radioRow
            [ spacing <| padSm model ]
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
                [ width <| px 20
                , height <| px 20
                , Background.color <|
                    washLight model
                , Border.rounded 10
                , Border.width 1
                , Border.color <| foreground model
                ]
            <|
                el
                    [ centerX
                    , centerY
                    , width <| px 14
                    , height <| px 14
                    , Border.rounded 7
                    , Background.color selectedColor
                    ]
                    none

        textLabel =
            el
                [ Font.size <| fontLg model
                , Font.family fontSecondary
                , Font.color <| foreground model
                ]
            <|
                text label
    in
    row
        [ spacing <| padXxs model ]
        [ radioButton
        , textLabel
        ]


calculateButton : Model -> Element Msg
calculateButton model =
    let
        borderShadow =
            { offset = ( 0, 2 )
            , size = 0
            , blur = 4
            , color = shadowColor model
            }

        pads =
            { edges
                | top = padSm model
                , bottom = padSm model
            }

        label : Element Msg
        label =
            el
                [ centerX
                , Font.color white
                , Font.size <| fontXl model
                , Font.letterSpacing 0.3
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
            , Background.color <| accent model
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
        unit : String
        unit =
            case model.unit of
                Lb ->
                    " lb"

                Kg ->
                    " kg"

        resultTitle : Element Msg
        resultTitle =
            el
                [ centerX
                , Font.size <| fontLg model
                , Font.family fontSecondary
                , Font.color <| foreground model
                ]
            <|
                text "Your Estimated 1 Rep Max"

        oneRepMax : Element Msg
        oneRepMax =
            el
                [ centerX
                , Font.size <| font4Xl model
                , Font.family fontPrimary
                , Font.color <| foreground model
                ]
            <|
                text <|
                    String.fromInt model.oneRepMax
                        ++ unit

        label : Element Msg
        label =
            el
                [ width fill
                , height fill
                , centerX
                ]
            <|
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
                , label = label
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


affiliateLink : Model -> Element Msg
affiliateLink model =
    let
        label =
            paragraph []
                [ el
                    [ Font.color <|
                        washHeavy model
                    ]
                  <|
                    text "Buy"
                , el
                    [ Font.color gold
                    , Font.bold
                    ]
                  <|
                    text " GOLD STANDARD "
                , el
                    [ Font.color <|
                        washHeavy model
                    ]
                  <|
                    text "100% Whey Protein Powder"
                ]
    in
    row
        [ width fill ]
        [ newTabLink
            [ width fill
            , Font.center
            , Font.size <| fontMd model
            , Font.family fontSecondary
            ]
            { url = "https://www.amazon.ca/gp/product/B08R8LRMWQ/ref=as_li_tl?ie=UTF8&camp=15121&creative=330641&creativeASIN=B08R8LRMWQ&linkCode=as2&tag=1repmaximum-20&linkId=906890c83da68e917254bfa8a78f8146"
            , label = label
            }
        ]


infoPanel : Model -> Element Msg
infoPanel model =
    panel
        [ infoTitle model
        , infoContent model
        ]
        model


infoTitle : Model -> Element Msg
infoTitle model =
    row [ width fill ]
        [ el
            [ centerX
            , Font.size <| fontLg model
            , Font.letterSpacing 0.3
            , Font.family fontPrimary
            , Font.color <| foreground model
            ]
          <|
            text "About 1 RM Calculator"
        ]


infoContent : Model -> Element Msg
infoContent model =
    let
        paragraphWithStyle : String -> Element Msg
        paragraphWithStyle content =
            paragraph
                [ spacing <| padXxs model
                , Font.size <| fontMd model
                , Font.family fontSecondary
                , Font.alignLeft
                , Font.color <| foreground model
                ]
                [ text content ]

        moreInfoLink : Element Msg
        moreInfoLink =
            newTabLink
                [ Font.size <| fontLg model
                , Font.color <| accent model
                , Font.family fontSecondary
                ]
                { url = "https://en.wikipedia.org/wiki/One-repetition_maximum"
                , label = text "More info about 1RM"
                }
    in
    column
        [ width fill
        , paddingEach { edges | top = padMd model }
        , spacing <| padMd model
        ]
        [ paragraphWithStyle
            "A one repetition maximum (one rep max or 1RM) in weight training is the maximum amount of weight that a person can possibly lift for one repetition. It can be used for determining an individuals maximum strength and is the method for determining the winner in events such as powerlifting and weightlifting competitions. A one repetition maximum can also be used as an upper limit, in order to determine the desired load for an exercise (as a percentage of the 1RM)."
        , paragraphWithStyle
            "The 1RM can either be calculated directly using maximal testing or indirectly using submaximal estimation. The submaximal estimation method is preferred as it is safer, quicker, and less unnerving for inexperienced exercisers, however, it may underestimate the actual 1RM. One rep maximum calculators are used to predict a one rep maximum lift. The degree of accuracy can vary largely depending on the weight training experience and muscular composition of the athlete. Also, most one rep maximum calculators are designed for seasoned strength trainers, and those with little experience may find their actual one rep maximum is much lower because their nervous system cannot handle the stress of a high weight. This test should be performed with a spotter for reasons of safety."
        , paragraphWithStyle
            "There are several common formulas used to estimate 1RM using the submaximal method, the Epley and the Brzycki being the most common. This app uses the Epley method."
        , moreInfoLink
        ]


developerLinks : Model -> Element Msg
developerLinks model =
    let
        linkStyle : List (Attribute Msg)
        linkStyle =
            [ Font.center
            , Font.color <| accent model
            , Font.size <| fontMd model
            , Font.family fontSecondary
            , Font.bold
            ]

        textStyle : List (Attribute Msg)
        textStyle =
            [ Font.center
            , Font.color <| foreground model
            , Font.size <| fontMd model
            , Font.family fontSecondary
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
        [ width fill
        , spacing <| padXs model
        ]
        [ paragraph textStyle
            [ text "Product design by "
            , designerLink
            ]
        , paragraph textStyle
            [ text "Built with elm-lang and elm-ui by "
            , developerLink
            ]
        ]


affiliateLinks : Model -> Element Msg
affiliateLinks model =
    let
        singleLink src =
            html <|
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
    in
    row
        [ centerX
        , paddingEach { edges | top = padSm model }
        ]
    <|
        List.map singleLink
            [ "//ws-na.amazon-adsystem.com/widgets/cm?ref=tf_til&t=1repmaximum-20&m=amazon&o=15&p=8&l=as1&IS1=1&asins=B08R8LRMWQ&linkId=cb68196a37380a7ce5b4d9c43a3b0c03&bc1=FFFFFF&amp;lt1=_top&fc1=333333&lc1=0066C0&bg1=FFFFFF&f=ifr"
            , "//ws-na.amazon-adsystem.com/widgets/cm?ref=tf_til&t=1repmaximum-20&m=amazon&o=15&p=8&l=as1&IS1=1&asins=B078RZZSF1&linkId=3884cb68c0639febf5ada4964108afa6&bc1=ffffff&amp;lt1=_top&fc1=333333&lc1=0066c0&bg1=ffffff&f=ifr"
            , "//ws-na.amazon-adsystem.com/widgets/cm?ref=tf_til&t=1repmaximum-20&m=amazon&o=15&p=8&l=as1&IS1=1&asins=B074NSYGJ2&linkId=49117e07e31540b31d96d52880d6a6da&bc1=FFFFFF&amp;lt1=_top&fc1=333333&lc1=0066C0&bg1=FFFFFF&f=ifr"
            ]


panel : List (Element Msg) -> Model -> Element Msg
panel children model =
    let
        panelShadow =
            { offset = ( 0, 4 )
            , size = 0
            , blur = 6
            , color = shadowColor model
            }
    in
    column
        [ width fill
        , padding <| padMd model
        , Background.color <| washLight model
        , Border.rounded 10
        , Border.shadow panelShadow
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
    htmlAttribute <|
        Html.Events.on "keyup" enterPressed


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
        min maxWidth model.screenSize.width


maxWidth : Int
maxWidth =
    450


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
