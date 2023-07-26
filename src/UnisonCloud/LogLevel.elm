module UnisonCloud.LogLevel exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import UI.Icon as Icon


type LogLevel
    = Info
    | Warn
    | Error
    | Custom String


fromString : String -> LogLevel
fromString raw =
    case String.toLower raw of
        "info" ->
            Info

        "log" ->
            Info

        "warn" ->
            Warn

        "warning" ->
            Warn

        "error" ->
            Error

        "err" ->
            Error

        "fail" ->
            Error

        "failure" ->
            Error

        "bug" ->
            Error

        "fatal" ->
            Error

        _ ->
            Custom raw


toString : LogLevel -> String
toString level =
    case level of
        Info ->
            "Info"

        Warn ->
            "Warn"

        Error ->
            "Error"

        Custom r ->
            r


toClassName : LogLevel -> String
toClassName level =
    "log-level_" ++ toClassName_ level


toClassName_ : LogLevel -> String
toClassName_ level =
    case level of
        Info ->
            "info"

        Warn ->
            "warn"

        Error ->
            "error"

        Custom _ ->
            "custom"


view : LogLevel -> Html msg
view level =
    let
        icon =
            case level of
                Info ->
                    Icon.view Icon.info

                Warn ->
                    Icon.view Icon.warn

                Error ->
                    Icon.view Icon.bug

                Custom custom ->
                    case String.uncons custom of
                        Just ( a, _ ) ->
                            a |> String.fromChar |> String.toUpper |> text

                        _ ->
                            Icon.view Icon.writingPad
    in
    div [ class "log-level", class (toClassName level) ] [ icon ]
