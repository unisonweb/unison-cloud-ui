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

        "warn" ->
            Warn

        "error" ->
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


view : LogLevel -> Html msg
view level =
    let
        ( className, icon ) =
            case level of
                Info ->
                    ( "info", Icon.view Icon.info )

                Warn ->
                    ( "warn", Icon.view Icon.warn )

                Error ->
                    ( "error", Icon.view Icon.bug )

                Custom custom ->
                    case String.uncons custom of
                        Just ( a, _ ) ->
                            ( "custom", a |> String.fromChar |> String.toUpper |> text )

                        _ ->
                            ( "custom", Icon.view Icon.writingPad )
    in
    div
        [ class "log-level"
        , class ("log-level_" ++ className)
        ]
        [ icon ]
