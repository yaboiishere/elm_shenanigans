module TimeMain exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (Svg, animateTransform, circle, line, svg)
import Svg.Attributes exposing (attributeName, attributeType, cx, cy, dur, fill, from, height, r, repeatCount, stroke, strokeWidth, to, type_, viewBox, width, x1, x2, y1, y2)
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , isStopped : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) False
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | ToggleStopTime


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            if model.isStopped then
                ( model, Cmd.none )

            else
                ( { model | time = newTime }
                , Cmd.none
                )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        ToggleStopTime ->
            ( { model | isStopped = not model.isStopped }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second =
            String.fromInt (Time.toSecond model.zone model.time)
    in
    h1 []
        [ text (hour ++ ":" ++ minute ++ ":" ++ second)
        , button [ onClick ToggleStopTime ]
            [ text
                (if model.isStopped then
                    "Start"

                 else
                    "Stop"
                )
            ]
        , br [] []
        , clockSvg hour minute second
        ]


clockSvg : String -> String -> String -> Svg msg
clockSvg hours minutes seconds =
    let
        secondsRotation =
            String.toInt seconds
                |> Maybe.withDefault 0
                |> (\x -> x * 6)
                |> String.fromInt

        minutesRotation =
            String.toInt minutes
                |> Maybe.withDefault 0
                |> (\x -> x * 6)
                |> String.fromInt

        hoursRotation =
            String.toInt hours
                |> Maybe.withDefault 0
                |> (\x -> x * 60 // 2)
                |> String.fromInt
    in
    svg
        [ width "300"
        , height "300"
        , viewBox "0 0 300 300"
        ]
        [ circle [ cx "150", cy "150", r "125", fill "none", stroke "black", strokeWidth "4" ] []
        , line [ x1 "150", x2 "150", y1 "150", y2 "30", stroke "red", strokeWidth "4" ]
            [ animateTransform
                [ attributeName "transform"
                , attributeType "XML"
                , type_ "rotate"
                , to (secondsRotation ++ " 150 150")
                , from (secondsRotation ++ " 150 150")
                , dur "60s"
                , repeatCount "indefinite"
                ]
                []
            ]
        , line [ x1 "150", y1 "150", x2 "150", y2 "50", stroke "black", strokeWidth "4" ]
            [ animateTransform
                [ attributeName "transform"
                , attributeType "XML"
                , type_ "rotate"
                , to (minutesRotation ++ " 150 150")
                , from (minutesRotation ++ " 150 150")
                , dur "3600s"
                , repeatCount "indefinite"
                ]
                []
            ]
        , line [ x1 "150", y1 "150", x2 "150", y2 "100", stroke "black", strokeWidth "4" ]
            [ animateTransform
                [ attributeName "transform"
                , attributeType "XML"
                , type_ "rotate"
                , to (hoursRotation ++ " 150 150")
                , from (hoursRotation ++ " 150 150")
                , dur "60s"
                , repeatCount "indefinite"
                ]
                []
            ]
        , circle [ cx "150", cy "150", r "5", fill "black" ] []
        ]
