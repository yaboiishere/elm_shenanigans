module RandomMain exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, br, button, div)
import Html.Events exposing (..)
import Maybe exposing (andThen)
import Platform exposing (Task)
import Process
import Random exposing (Generator, map3)
import String
import Svg exposing (..)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, stroke, viewBox, width, x, y)
import Task exposing (Task)
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { firstDieFace : Int
    , secondDieFace : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1 1
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewFace Dice


type alias Dice =
    { firstDie : Int
    , secondDie : Int
    , randomNum : Int
    }


roll : Generator Int
roll =
    Random.weighted
        ( 10, 1 )
        [ ( 20, 2 )
        , ( 20, 3 )
        , ( 20, 4 )
        , ( 20, 5 )
        , ( 10, 6 )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace (map3 Dice roll roll roll)
            )

        NewFace { firstDie, secondDie, randomNum } ->
            if randomNum == 6 then
                ( Model firstDie secondDie
                , Cmd.none
                )

            else
                ( Model firstDie secondDie
                , Task.perform (\_ -> Roll) (Process.sleep 100)
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Roll ] [ Html.text "Roll" ]
        , br [] []
        , determineDie model.firstDieFace 120
        , determineDie model.secondDieFace 60
        ]


determineDie : Int -> Int -> Svg Msg
determineDie num size =
    case num of
        1 ->
            die size oneDie

        2 ->
            die size twoDie

        3 ->
            die size threeDie

        4 ->
            die size fourDie

        5 ->
            die size fiveDie

        6 ->
            die size sixDie

        _ ->
            die num oneDie


die : Int -> (Int -> Int -> Svg Msg) -> Svg Msg
die sizeInt innerDie =
    let
        sizeStr =
            String.fromInt sizeInt

        twelvthSizeStr =
            String.fromInt (sizeInt // 12)

        w =
            sizeInt // 12 * 10
    in
    svg
        [ width sizeStr
        , height sizeStr
        , viewBox ("0 0 " ++ sizeStr ++ " " ++ sizeStr)
        ]
        [ rect
            [ x twelvthSizeStr
            , y twelvthSizeStr
            , width (String.fromInt w)
            , height (String.fromInt w)
            , rx "15"
            , ry "15"
            , fill "white"
            , stroke "black"
            ]
            []
        , innerDie sizeInt (sizeInt // 12)
        ]


oneDie : Int -> Int -> Svg Msg
oneDie sizeInt twelvth =
    let
        sizeStr =
            String.fromInt (sizeInt // 2)
    in
    circle
        [ cx sizeStr
        , cy sizeStr
        , r (String.fromInt twelvth)
        ]
        []


twoDie : Int -> Int -> Svg Msg
twoDie sizeInt twelvth =
    let
        firstDot =
            sizeInt // 3

        secondDot =
            sizeInt - firstDot
    in
    g []
        [ circle [ cx (String.fromInt firstDot), cy (String.fromInt firstDot), r (String.fromInt twelvth) ] []
        , circle [ cx (String.fromInt secondDot), cy (String.fromInt secondDot), r (String.fromInt twelvth) ] []
        ]


threeDie : Int -> Int -> Svg Msg
threeDie sizeInt twelvth =
    let
        firstDot =
            sizeInt // 3

        secondDot =
            sizeInt // 2

        thirdDot =
            sizeInt - firstDot
    in
    g []
        [ circle [ cx (String.fromInt firstDot), cy (String.fromInt firstDot), r (String.fromInt twelvth) ] []
        , circle [ cx (String.fromInt secondDot), cy (String.fromInt secondDot), r (String.fromInt twelvth) ] []
        , circle [ cx (String.fromInt thirdDot), cy (String.fromInt thirdDot), r (String.fromInt twelvth) ] []
        ]


fourDie : Int -> Int -> Svg Msg
fourDie sizeInt twelvth =
    let
        firstDot =
            sizeInt // 3

        secondDot =
            sizeInt - firstDot

        firstRow =
            g []
                [ circle [ cx (String.fromInt firstDot), cy (String.fromInt firstDot), r (String.fromInt twelvth) ] []
                , circle [ cx (String.fromInt secondDot), cy (String.fromInt firstDot), r (String.fromInt twelvth) ] []
                ]

        secondRow =
            g []
                [ circle [ cx (String.fromInt firstDot), cy (String.fromInt secondDot), r (String.fromInt twelvth) ] []
                , circle [ cx (String.fromInt secondDot), cy (String.fromInt secondDot), r (String.fromInt twelvth) ] []
                ]
    in
    g []
        [ firstRow
        , secondRow
        ]


fiveDie : Int -> Int -> Svg Msg
fiveDie sizeInt twelvth =
    let
        middleDot =
            sizeInt // 2
    in
    g []
        [ circle [ cx (String.fromInt middleDot), cy (String.fromInt middleDot), r (String.fromInt twelvth) ] []
        , fourDie sizeInt twelvth
        ]


sixDie : Int -> Int -> Svg Msg
sixDie sizeInt twelvth =
    let
        firstDot =
            sizeInt // 3

        secondDot =
            sizeInt - firstDot

        thirdDot =
            sizeInt // 2
    in
    g []
        [ fourDie sizeInt twelvth
        , g []
            [ circle [ cx (String.fromInt thirdDot), cy (String.fromInt firstDot), r (String.fromInt twelvth) ] []
            , circle [ cx (String.fromInt thirdDot), cy (String.fromInt secondDot), r (String.fromInt twelvth) ] []
            ]
        ]
